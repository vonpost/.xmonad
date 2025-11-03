{-# LANGUAGE ScopedTypeVariables #-}


module XMonad.Hooks.WorkspaceSlide
  ( workspaceSlidePrepare
  , workspaceSlideSetOrder
  ) where
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.IndependentScreens
  ( VirtualWorkspace
  , marshall
  , unmarshall
  , workspaces'
  )

import Data.Word (Word32)
import Data.List (find, elemIndex, nub)
import qualified Data.Map.Strict as M
import Control.Monad (forM, forM_, void)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar, modifyMVar_)
import System.IO.Unsafe (unsafePerformIO)
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras
  ( changeProperty32
  , getWindowProperty32
  , xDeleteProperty
  , propModeReplace
  , queryTree
  )
-- keep existing imports; add:
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- global (module-local) preferred virtual order; empty means "no override"
preferredVirtOrder :: IORef [VirtualWorkspace]
preferredVirtOrder = unsafePerformIO (newIORef [])
{-# NOINLINE preferredVirtOrder #-}

-- call this once in your config startup to lock the intended order (e.g., ["1","2","3","4","5"])
workspaceSlideSetOrder :: [VirtualWorkspace] -> X ()
workspaceSlideSetOrder ord = io $ writeIORef preferredVirtOrder ord

--------------------------------------------------------------------------------
-- Internal state: (maybe running cleanup thread, generation counter)
--------------------------------------------------------------------------------

slideState :: MVar (Maybe ThreadId, Word32)
slideState = unsafePerformIO (newMVar (Nothing, 0))
{-# NOINLINE slideState #-}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- Parent frame of a client window (exclude root)
getParent :: X.Display -> X.Window -> IO (Maybe X.Window)
getParent dpy w = do
  (root, parent, _) <- queryTree dpy w
  let isFrame = parent /= root && parent /= 0
  pure $ if isFrame then Just parent else Nothing

-- All windows (tiled + floating) for a physical workspace tag
wsWindowsAll :: WorkspaceId -> WindowSet -> [Window]
wsWindowsAll physTag ws =
  nub (tiled ++ floaters)
 where
  tiled =
    maybe [] (W.integrate' . W.stack) $
      find ((== physTag) . W.tag) (W.workspaces ws)

  floaters =
    [ w
    | w <- M.keys (W.floating ws)
    , W.findTag w ws == Just physTag
    ]

-- Index of a virtual workspace in configured virtual order
virtIndex :: [VirtualWorkspace] -> VirtualWorkspace -> Maybe Int
virtIndex order vw = elemIndex vw order

--------------------------------------------------------------------------------
-- Write direction+generation to client + frame; return touched windows
--------------------------------------------------------------------------------

-- === replace the old getParent with this ancestry walker ===
-- Return the whole ancestor chain for a window (closest parent first), stopping at root.
getAncestors :: X.Display -> X.Window -> IO [X.Window]
getAncestors dpy w0 = go w0 []
  where
    go w acc = do
      (root, parent, _) <- queryTree dpy w
      -- stop if parent is 0 or parent == root
      if parent == 0 || parent == root
        then pure acc
        else go parent (parent : acc)

-- === replace setSwitchProperty with this deep stamping version ===
-- Write direction+generation to client and ALL ancestor frames (up to root).
setSwitchProperty :: (Word32, Word32) -> [Window] -> X [(Window, Word32)]
setSwitchProperty _ [] = pure []
setSwitchProperty (dir, gen) wins = do
  dpy  <- asks display
  atom <- io $ X.internAtom dpy "_MY_CUSTOM_WORKSPACE_SWITCH" False
  let payload = [fromIntegral dir, fromIntegral gen]
  touched <- io $ fmap concat $ forM wins $ \w -> do
    ancestors <- getAncestors dpy w
    -- client first, then every ancestor frame
    changeProperty32 dpy w atom X.cARDINAL propModeReplace payload
    mapM_ (\p -> changeProperty32 dpy p atom X.cARDINAL propModeReplace payload) ancestors
    pure ( (w,gen) : map (\p -> (p,gen)) ancestors )
  io $ X.sync dpy False
  pure touched

--------------------------------------------------------------------------------
-- Cleanup: generation-aware, cancel previous thread to avoid races
--------------------------------------------------------------------------------

scheduleSlideCleanup :: [(Window, Word32)] -> X ()
scheduleSlideCleanup [] = pure ()
scheduleSlideCleanup winGens = do
  dpy  <- asks display
  atom <- io $ X.internAtom dpy "_MY_CUSTOM_WORKSPACE_SWITCH" False

  io $ modifyMVar_ slideState $ \(mOldTid, gen) -> do
    maybe (pure ()) killThread mOldTid
    tid <- forkIO $ do
      threadDelay 360000  -- a hair > picom's 300ms anim
      forM_ winGens $ \(w, g) -> do
        m <- getWindowProperty32 dpy w atom
        case m of
          Just (_dir:gen' : _) | fromIntegral g == gen' ->
            void $ xDeleteProperty dpy w atom
          _ -> pure ()
      X.sync dpy False
    pure (Just tid, gen)

--------------------------------------------------------------------------------
-- Core helper: EXACT original semantics (pre -> return post)
--------------------------------------------------------------------------------

workspaceSlidePrepareCore :: VirtualWorkspace -> X (X ())
workspaceSlidePrepareCore targetVW =
  withWindowSet $ \ws -> do
    conf <- asks config
    let curPhys            = W.tag . W.workspace $ W.current ws
        (curScreen, _)     = unmarshall curPhys
        tgtPhys            = marshall curScreen targetVW

    -- screen-local order:
    -- 1) use explicitly set preferred virtual order if present
    -- 2) else fall back to current physical list
    ord <- io $ readIORef preferredVirtOrder
    let screenPhysOrder =
          if null ord
            then map W.tag (W.workspaces ws)
            else map (marshall curScreen) ord

        mCurIx = elemIndex curPhys screenPhysOrder
        mTgtIx = elemIndex tgtPhys screenPhysOrder

    case (mCurIx, mTgtIx) of
      (Just iCur, Just iTgt) | curPhys /= tgtPhys -> do
        let dir :: Word32
            dir = if iTgt > iCur then 2 else 1  -- right when target is later

        let currentWins = wsWindowsAll curPhys ws
            targetWins  = wsWindowsAll tgtPhys ws

        (gen :: Word32, mOldTid :: Maybe ThreadId) <- io $
          modifyMVar slideState $ \(mTid, g) ->
            let g' = g + 1 in pure ((mTid, g'), (g', mTid))

        io $ maybe (pure ()) killThread mOldTid
        outgoing <- setSwitchProperty (dir, gen) currentWins
        incoming <- setSwitchProperty (dir, gen) targetWins
        pure (scheduleSlideCleanup (outgoing ++ incoming))
      _ -> pure (pure ())


--------------------------------------------------------------------------------
-- Public combinator: keeps your EXACT call site
--------------------------------------------------------------------------------

-- Use exactly like:
--   workspaceSlidePrepare onCurrentScreenX toggleOrView name
--
workspaceSlidePrepare
  :: ((WorkspaceId -> X ()) -> VirtualWorkspace -> X ())  -- lifter, e.g. onCurrentScreenX
  -> (WorkspaceId -> X ())                                -- action, e.g. toggleOrView
  -> VirtualWorkspace                                     -- target virtual workspace
  -> X ()
workspaceSlidePrepare lifter action vw = do
  post <- workspaceSlidePrepareCore vw   -- pre: mark client+frame with dir/gen
  lifter action vw                       -- switch (your existing call)
  post                                   -- post: schedule cleanup
