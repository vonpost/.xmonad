module MyWindowHints
  ( windowHints
  , HintConfig(..)
  , ColorSpec(..)
  , ModeColorSpec(..)
  , defaultHintConfig
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (forM, unless)
import Data.Bits ((.|.))
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import Foreign
import Graphics.X11.Types (KeySym)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Types (Pixel)
import Graphics.X11.Xlib.Extras
import XMonad
import qualified XMonad.StackSet as W

data HintOverlay = HintOverlay
  { hintKey :: !KeySym
  , hintLabel :: !String
  , hintTarget :: !Window
  , hintWindow :: !Window
  , hintWidth :: !Dimension
  , hintHeight :: !Dimension
  }

data HintMode = ModeFocus | ModeSwap

data ModeColors = ModeColors
  { modeBg :: !Pixel
  , modeFg :: !Pixel
  }

data ColorSpec
  = ColorName String
  | ColorPixel Pixel
  deriving (Eq, Show)

data ModeColorSpec = ModeColorSpec
  { specBg :: !ColorSpec
  , specFg :: !ColorSpec
  } deriving (Eq, Show)

data HintConfig = HintConfig
  { hcLeaderKey :: !KeySym
  , hcFontName :: !(Maybe String)
  , hcAlphabet :: !String
  , hcFocusColors :: !ModeColorSpec
  , hcSwapColors :: !ModeColorSpec
  , hcWindowClass :: !(Maybe (String, String))
  } deriving (Eq, Show)

defaultHintConfig :: HintConfig
defaultHintConfig =
  HintConfig
    { hcLeaderKey = xK_semicolon
    , hcFontName = Just "-misc-fixed-medium-r-normal--18-120-100-100-c-90-iso8859-1"
    , hcAlphabet = "asdfghjklqwertyuiopzxcvbnm1234567890"
    , hcFocusColors = ModeColorSpec (ColorName "white") (ColorName "black")
    , hcSwapColors = ModeColorSpec (ColorName "black") (ColorName "white")
    , hcWindowClass = Just ("xmonad-window-hint", "XMonadWindowHint")
    }

-- | Show Vimium-style hints for windows on the current workspace. Pressing
-- the leader toggles into swap mode; selecting a hint focuses or swaps.
windowHints :: HintConfig -> X ()
windowHints cfg = withWindowSet $ \ws -> do
  let wins = W.integrate' . W.stack . W.workspace . W.current $ ws
      focusWin = W.peek ws
      selectableWins = maybe wins (\f -> filter (/= f) wins) focusWin
  unless (null selectableWins) $ withDisplay $ \dpy -> do
    let screen = defaultScreenOfDisplay dpy
        screenNumber = defaultScreen dpy
    focusColors <- io $ resolveModeColors dpy screenNumber (hcFocusColors cfg)
    swapColors <- io $ resolveModeColors dpy screenNumber (hcSwapColors cfg)
    let availableHints = mapMaybe mkHint (hcAlphabet cfg)
        hintPairs = zip availableHints selectableWins
    unless (null hintPairs) $ do
      font <- io $ loadFontMaybe dpy (hcFontName cfg)
      let winClass = hcWindowClass cfg
      overlays <- io $ forM hintPairs $ \((sym, label), w) -> do
        overlay <- createOverlay dpy screen focusColors font winClass label w
        pure overlay { hintKey = sym }
      selection <- io $ chooseWindow dpy focusColors swapColors font overlays (hcLeaderKey cfg)
      io $ do
        mapM_ (destroyWindow dpy . hintWindow) overlays
        sync dpy False
        maybe (pure ()) (freeFont dpy) font
      case selection of
        Just (ModeFocus, w) -> focusWindowX w
        Just (ModeSwap, w) -> swapWithFocused w
        Nothing -> pure ()
  where
    mkHint c =
      let sym = stringToKeysym [c]
       in if sym == noSymbol
            then Nothing
            else Just (sym, [toUpper c])
    focusWindowX win = windows (W.focusWindow win)
    swapWithFocused win = withWindowSet $ \ws' ->
      case W.peek ws' of
        Nothing -> pure ()
        Just focused
          | focused == win -> pure ()
          | otherwise -> windows (swapWindows focused win)

swapWindows :: (Eq a, Ord a) => a -> a -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
swapWindows a b stackSet =
  stackSet
    { W.current = swapScreen (W.current stackSet)
    , W.visible = map swapScreen (W.visible stackSet)
    , W.hidden = map swapWorkspace (W.hidden stackSet)
    , W.floating = M.mapKeys swapKey (W.floating stackSet)
    }
  where
    swapScreen sc = sc { W.workspace = swapWorkspace (W.workspace sc) }
    swapWorkspace ws = ws { W.stack = fmap swapStack (W.stack ws) }
    swapStack st =
      st
        { W.focus = swapItem (W.focus st)
        , W.up = map swapItem (W.up st)
        , W.down = map swapItem (W.down st)
        }
    swapItem x
      | x == a = b
      | x == b = a
      | otherwise = x
    swapKey x
      | x == a = b
      | x == b = a
      | otherwise = x

resolveModeColors :: Display -> ScreenNumber -> ModeColorSpec -> IO ModeColors
resolveModeColors dpy screenNum (ModeColorSpec bgSpec fgSpec) = do
  let defaultBg = whitePixel dpy screenNum
      defaultFg = blackPixel dpy screenNum
  bg <- resolveColor dpy screenNum defaultBg bgSpec
  fg <- resolveColor dpy screenNum defaultFg fgSpec
  pure $ ModeColors bg fg

resolveColor :: Display -> ScreenNumber -> Pixel -> ColorSpec -> IO Pixel
resolveColor _ _ fallback (ColorPixel px) = pure px
resolveColor dpy screenNum fallback (ColorName name) = do
  let cmap = defaultColormap dpy screenNum
  res <- try (allocNamedColor dpy cmap name) :: IO (Either SomeException (Color, Color))
  pure $ either (const fallback) (color_pixel . fst) res

loadFontMaybe :: Display -> Maybe String -> IO (Maybe FontStruct)
loadFontMaybe _ Nothing = pure Nothing
loadFontMaybe dpy (Just name) = do
  res <- try (loadQueryFont dpy name) :: IO (Either SomeException FontStruct)
  pure $ either (const Nothing) Just res

createOverlay :: Display -> Screen -> ModeColors -> Maybe FontStruct -> Maybe (String, String) -> String -> Window -> IO HintOverlay
createOverlay dpy screen colors font classHint label target = do
  attrs <- getWindowAttributes dpy target
  let winWidth = max 1 (fromIntegral (wa_width attrs) :: Int)
      winHeight = max 1 (fromIntegral (wa_height attrs) :: Int)
      overlayWidth :: Int
      overlayWidth = 60
      overlayHeight :: Int
      overlayHeight = 36
      posX = fromIntegral (wa_x attrs) + (winWidth `div` 2) - overlayWidth `div` 2
      posY = fromIntegral (wa_y attrs) + (winHeight `div` 2) - overlayHeight `div` 2
      rectWidth = fromIntegral overlayWidth
      rectHeight = fromIntegral overlayHeight
      coordsX = fromIntegral (max 0 posX)
      coordsY = fromIntegral (max 0 posY)
      depth = defaultDepthOfScreen screen
      visual = defaultVisualOfScreen screen
      rootw = rootWindowOfScreen screen
  allocaSetWindowAttributes $ \setAttr -> do
    set_override_redirect setAttr True
    set_background_pixel setAttr (modeBg colors)
    set_border_pixel setAttr (modeFg colors)
    let attrMask = cWOverrideRedirect .|. cWBackPixel .|. cWBorderPixel
    overlayWindow <-
      createWindow
        dpy
        rootw
        coordsX
        coordsY
        rectWidth
        rectHeight
        1
        depth
        inputOutput
        visual
        attrMask
        setAttr
    case classHint of
      Just (resName, resClass) ->
        setClassHint dpy overlayWindow (ClassHint resName resClass)
      Nothing -> pure ()
    mapRaised dpy overlayWindow
    drawOverlay dpy overlayWindow rectWidth rectHeight colors font label
    pure
      HintOverlay
        { hintKey = noSymbol
        , hintLabel = label
        , hintTarget = target
        , hintWindow = overlayWindow
        , hintWidth = rectWidth
        , hintHeight = rectHeight
        }

drawOverlay :: Display -> Window -> Dimension -> Dimension -> ModeColors -> Maybe FontStruct -> String -> IO ()
drawOverlay dpy win width height colors font label = do
  gc <- createGC dpy win
  setGraphicsExposures dpy gc False
  let zeroPos = 0 :: Position
  setForeground dpy gc (modeBg colors)
  fillRectangle dpy win gc zeroPos zeroPos width height
  setForeground dpy gc (modeFg colors)
  maybe (pure ()) (setFont dpy gc . fontFromFontStruct) font
  labelWidth <- maybe (pure 12) (\f -> pure $ textWidth f label) font
  let textX = ((fromIntegral width - fromIntegral labelWidth) `div` 2)
      textY = (fromIntegral height * 2 `div` 3)
      textXPos = fromIntegral (max 4 textX) :: Position
      textYPos = fromIntegral textY :: Position
  drawString dpy win gc textXPos textYPos label
  drawRectangle dpy win gc zeroPos zeroPos (width - 1) (height - 1)
  freeGC dpy gc
  sync dpy False

chooseWindow :: Display -> ModeColors -> ModeColors -> Maybe FontStruct -> [HintOverlay] -> KeySym -> IO (Maybe (HintMode, Window))
chooseWindow dpy focusColors swapColors font overlays leader = do
  status <- grabKeyboard dpy (defaultRootWindow dpy) False grabModeAsync grabModeAsync currentTime
  if status /= grabSuccess
    then do
      ungrabKeyboard dpy currentTime
      pure Nothing
    else do
      result <- loop ModeFocus
      ungrabKeyboard dpy currentTime
      pure result
  where
    focusMap = M.fromList [(hintKey hint, hintTarget hint) | hint <- overlays]
    loop mode = do
      mks <- nextKeySym
      case mks of
        Nothing -> pure Nothing
        Just sym
          | sym == xK_Escape -> pure Nothing
          | sym == leader -> do
              let nextMode = toggleMode mode
                  colors = case nextMode of
                    ModeFocus -> focusColors
                    ModeSwap -> swapColors
              mapM_ (\o -> drawOverlay dpy (hintWindow o) (hintWidth o) (hintHeight o) colors font (hintLabel o)) overlays
              loop nextMode
          | Just target <- M.lookup sym focusMap -> pure (Just (mode, target))
          | otherwise -> loop mode

    nextKeySym = allocaXEvent $ \e -> do
      nextEvent dpy e
      ev <- getEvent e
      case ev of
        KeyEvent {ev_event_type = t, ev_keycode = code}
          | t == keyPress -> do
            sym <- keycodeToKeysym dpy code 0
            if sym == noSymbol
              then nextKeySym
              else pure (Just sym)
          | otherwise -> nextKeySym
        _ -> nextKeySym

    toggleMode ModeFocus = ModeSwap
    toggleMode ModeSwap = ModeFocus
