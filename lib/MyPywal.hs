{-# LANGUAGE DeriveDataTypeable #-}

module MyPywal
  ( pywalStartupHook
  , pywalLogHook
  , pywalPrepareWorkspace
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Char (isDigit)
import qualified Data.Map.Strict as M
import Data.Typeable (Typeable)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

type WalTargets = M.Map WorkspaceId FilePath

wallpaperDir :: FilePath
wallpaperDir = "/home/dcol/wallpapers"

workspaceWallpapers :: WalTargets
workspaceWallpapers =
  -- Set one wallpaper per workspace; add/remove entries as needed.
  M.fromList
    [ ("browse", wallpaperDir </> "tennis.jpg")
    , ("code"  , wallpaperDir </> "clockwork.jpg")
    , ("read"  , wallpaperDir </> "eye")
    , ("chat"  , wallpaperDir </> "catupscale.png")
    , ("etc"   , wallpaperDir </> "stairs.jpg")
    ]

data WalState = WalState
  { walLastWorkspace :: Maybe WorkspaceId
  , walPendingWorkspace :: Maybe WorkspaceId
  }
  deriving (Read, Show, Typeable)

instance ExtensionClass WalState where
  initialValue = WalState Nothing Nothing

pywalStartupHook :: X ()
pywalStartupHook = do
  XS.put (WalState Nothing Nothing)
  runWalForCurrent

pywalLogHook :: X ()
pywalLogHook = do
  current <- gets (W.currentTag . windowset)
  WalState lastWorkspace pending <- XS.get
  case pending of
    Just target
      | target == current ->
          XS.put (WalState (Just current) Nothing)
      | otherwise ->
          runWal current
    Nothing ->
      when (Just current /= lastWorkspace) $
        runWal current

pywalPrepareWorkspace :: WorkspaceId -> X ()
pywalPrepareWorkspace ws = do
  runWal ws
  XS.modify (\s -> s { walPendingWorkspace = Just ws })

runWalForCurrent :: X ()
runWalForCurrent =
  gets (W.currentTag . windowset) >>= runWal

runWal :: WorkspaceId -> X ()
runWal ws = do
  XS.modify (\s -> s { walLastWorkspace = Just ws, walPendingWorkspace = Nothing })
  forM_ (resolveWalTarget ws) $ \wallpaper ->
    applyWalTheme wallpaper

resolveWalTarget :: WorkspaceId -> Maybe FilePath
resolveWalTarget ws =
  M.lookup ws workspaceWallpapers <|> M.lookup (baseWorkspaceName ws) workspaceWallpapers

baseWorkspaceName :: WorkspaceId -> WorkspaceId
baseWorkspaceName ws =
  case span isDigit ws of
    (digits, '_' : rest) | not (null digits) -> rest
    _ -> ws

applyWalTheme :: FilePath -> X ()
applyWalTheme wallpaper = do
  runQuiet "wal" ["-q", "-n", "-i", wallpaper]
  applyWallpaper wallpaper
  safeSpawn "bash" ["/home/dcol/dotfiles/scripts/set_wal"]

applyWallpaper :: FilePath -> X ()
applyWallpaper wallpaper =
  runQuiet "feh" ["--bg-fill", wallpaper]

runQuiet :: FilePath -> [String] -> X ()
runQuiet cmd args =
  io $
    catchIOError
      (runProcessWithInput cmd args "" >> pure ())
      (const (pure ()))
