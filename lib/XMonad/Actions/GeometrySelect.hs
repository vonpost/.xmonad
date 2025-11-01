module XMonad.Actions.GeometrySelect
  ( geometrySelect
  , GSConfig(..)
  , defaultGSConfig
  ) where

import Control.Exception (SomeException, finally, try)
import Control.Monad (forM, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Char (chr, isPrint, toLower)
import Data.Function (on)
import Data.List (find, findIndex, foldl', groupBy, intercalate, isPrefixOf, maximumBy, minimumBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Word (Word32, Word8)
import qualified Data.Map.Strict as M
import Foreign.Marshal.Alloc (alloca)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xft
import Graphics.X11.Xrender (XGlyphInfo(..))
import XMonad
import qualified XMonad.StackSet as W

data GSConfig = GSConfig
  { bgColor :: !String
  , fgColor :: !String
  , accentColor :: !String
  , inactiveColor :: !String
  , modelineBgColor :: !String
  , modelineFgColor :: !String
  , modelineHeight :: !Int
  , overlayOpacity :: !Double
  , workspaceGap :: !Int
  , padding :: !Int
  , fontName :: !String
  }

data RawItem = RawItem
  { rawWorkspace :: !WorkspaceId
  , rawWindow :: !Window
  , rawRect :: !Rectangle
  , rawTitle :: !String
  , rawCurrent :: !Bool
  }

getWindowTitle :: Display -> Window -> IO String
getWindowTitle dpy win = do
  utf8Title <- getUtf8WindowTitle dpy win
  case utf8Title of
    Just title -> pure title
    Nothing -> do
      legacy <- try (fetchName dpy win) :: IO (Either SomeException (Maybe String))
      pure $ maybe "" id (either (const Nothing) id legacy)

getUtf8WindowTitle :: Display -> Window -> IO (Maybe String)
getUtf8WindowTitle dpy win = do
  netAtom <- internAtom dpy "_NET_WM_NAME" False
  prop <- getWindowProperty8 dpy netAtom win
  case prop of
    Nothing -> pure Nothing
    Just bytes ->
      pure $ decodeUtf8Safe (map fromIntegral bytes)

decodeUtf8Safe :: [Word8] -> Maybe String
decodeUtf8Safe = fmap reverse . go []
  where
    go acc [] = Just acc
    go acc (b:rest)
      | b < 0x80 = go (chr (fromIntegral b) : acc) rest
      | b .&. 0xE0 == 0xC0 = twoBytes 0x1F 1 b rest acc
      | b .&. 0xF0 == 0xE0 = twoBytes 0x0F 2 b rest acc
      | b .&. 0xF8 == 0xF0 = twoBytes 0x07 3 b rest acc
      | otherwise = Nothing

    twoBytes mask count first bs acc =
      case splitAt count bs of
        (conts, rest')
          | all isContinuation conts ->
              let code = decodeCodePoint mask first conts
                  char = chr code
               in if code >= 0xD800 && code <= 0xDFFF
                    then Nothing
                    else go (char : acc) rest'
        _ -> Nothing

    isContinuation byte = byte .&. 0xC0 == 0x80

    decodeCodePoint mask first conts =
      foldl'
        (\acc byte -> (acc `shiftL` 6) .|. fromIntegral (byte .&. 0x3F))
        (fromIntegral (first .&. mask))
        conts

data AtlasItem = AtlasItem
  { itemWorkspace :: !WorkspaceId
  , itemWindow :: !Window
  , itemRect :: !Rectangle
  , itemTitle :: !String
  , itemOnCurrent :: !Bool
  , itemCenterX :: !Double
  , itemCenterY :: !Double
  }

data WorkspaceInfo = WorkspaceInfo
  { wiWorkspace :: !WorkspaceId
  , wiLeft :: !Int
  , wiRight :: !Int
  , wiItemIndices :: ![Int]
  }

data AtlasLayout = AtlasLayout
  { layoutWidth :: !Int
  , layoutHeight :: !Int
  , layoutItems :: ![AtlasItem]
  , layoutWorkspaces :: ![WorkspaceInfo]
  }

data Selection
  = SelectionFocus AtlasItem
  | SelectionSwap AtlasItem

data HintMode = HintModeFocus | HintModeSwap deriving (Eq)

data HintState = HintState
  { hsPrefix :: !String
  , hsMode :: !HintMode
  }

data HintSettings = HintSettings
  { hintAlphabet :: !String
  , hintLeader :: !KeySym
  }

type HintItem = (String, AtlasItem)

data OverlayColors = OverlayColors
  { colorBg :: !Pixel
  , colorFg :: !Pixel
  , colorAccent :: !Pixel
  , colorInactive :: !Pixel
  , colorModelineBg :: !Pixel
  , colorModelineFg :: !Pixel
  , colorAllocated :: ![Pixel]
  }

data OverlayEnv = OverlayEnv
  { oeDisplay :: !Display
  , oeWindow :: !Window
  , oeGC :: !GC
  , oeDraw :: !XftDraw
  , oeFont :: !XftFont
  , oeColors :: !OverlayColors
  , oeVisual :: !Visual
  , oeColormap :: !Colormap
  , oeWidth :: !Dimension
  , oeHeight :: !Dimension
  , oePixmap :: !Pixmap
  }

defaultGSConfig :: GSConfig
defaultGSConfig =
  GSConfig
    { bgColor = "#111217"
    , fgColor = "#f8f8f2"
    , accentColor = "#6272a4"
    , inactiveColor = "#44475a"
    , modelineBgColor = "#1d1f27"
    , modelineFgColor = "#f8f8f2"
    , modelineHeight = 32
    , overlayOpacity = 0.95
    , workspaceGap = 48
    , padding = 32
    , fontName = "Sans-10"
    }

geometrySelect :: GSConfig -> X ()
geometrySelect cfg = withWindowSet $ \ws ->
  withDisplay $ \dpy -> do
    rawItems <- collectWindows dpy ws
    case concatWorkspaces cfg rawItems of
      Nothing -> pure ()
      Just layout -> do
        let focusedWorkspace = W.currentTag ws
        let atlasItems = layoutItems layout
        let focused = W.peek ws
            initialIndex =
              fromMaybe 0 $
                focused >>= \w -> findIndex (\it -> itemWindow it == w) atlasItems
        selection <- io $ runOverlay cfg dpy focusedWorkspace layout initialIndex
        case selection of
          Just (SelectionFocus chosen) -> do
            windows (W.view (itemWorkspace chosen))
            windows (W.focusWindow (itemWindow chosen))
          Just (SelectionSwap chosen) -> do
            windows (W.view (itemWorkspace chosen))
            swapWithFocused (itemWindow chosen)
          Nothing -> pure ()

collectWindows :: Display -> WindowSet -> X [RawItem]
collectWindows dpy ws = do
  let orderedWorkspaces =
        W.workspace (W.current ws) :
        map W.workspace (W.visible ws) ++
        W.hidden ws
      currentTag = W.currentTag ws
  fmap concat $
    forM orderedWorkspaces $ \workspace -> do
      let tag = W.tag workspace
      if tag == "NSP"
        then pure []
        else do
          let wins = W.integrate' (W.stack workspace)
              onCurrent = tag == currentTag
          fmap catMaybes $
            forM wins $ \win -> do
              attrsResult <- io $ try (getWindowAttributes dpy win)
              case attrsResult of
                Left (_ :: SomeException) -> pure Nothing
                Right attrs -> do
                  title <- io (getWindowTitle dpy win)
                  let rect = rectFromAttributes attrs
                  pure $
                    Just
                      RawItem
                        { rawWorkspace = tag
                        , rawWindow = win
                        , rawRect = rect
                        , rawTitle = title
                        , rawCurrent = onCurrent
                        }

concatWorkspaces :: GSConfig -> [RawItem] -> Maybe AtlasLayout
concatWorkspaces cfg items
  | null items = Nothing
  | otherwise =
      let grouped =
            filter (not . null) $
              groupBy ((==) `on` rawWorkspace) items
          summaries = map summarise grouped
          (_, _, placed, infos) = foldl' step (0, 0, [], []) summaries
          atlasWidth =
            case infos of
              [] -> 1
              _ -> max 1 (wiRight (last infos))
          atlasHeight =
            max 1 $ maximum (1 : map wsHeight summaries)
       in Just
            AtlasLayout
              { layoutWidth = atlasWidth
              , layoutHeight = atlasHeight
              , layoutItems = placed
              , layoutWorkspaces = infos
              }
  where
    gap = workspaceGap cfg
    summarise :: [RawItem] -> WorkspaceSummary
    summarise [] = WorkspaceSummary "" 1 1 []
    summarise wsItems@(firstItem : _) =
      let rects = map rawRect wsItems
          minX = minimum (map rectLeft rects)
          minY = minimum (map rectTop rects)
          maxX = maximum (map rectRight rects)
          maxY = maximum (map rectBottom rects)
          width = max 1 (maxX - minX)
          height = max 1 (maxY - minY)
          normalised =
            [ raw
                { rawRect =
                    Rectangle
                      { rect_x = fromIntegral (rectLeft (rawRect raw) - minX)
                      , rect_y = fromIntegral (rectTop (rawRect raw) - minY)
                      , rect_width = rect_width (rawRect raw)
                      , rect_height = rect_height (rawRect raw)
                      }
                }
            | raw <- wsItems
            ]
       in WorkspaceSummary
            { wsTag = rawWorkspace firstItem
            , wsWidth = width
            , wsHeight = height
            , wsItems = normalised
            }

    step (offset, nextIndex, accItems, accInfos) summary =
      let (placedItems, indices, nextIndex') = placeWorkspace offset summary nextIndex
          newOffset = offset + wsWidth summary + gap
          info =
            WorkspaceInfo
              { wiWorkspace = wsTag summary
              , wiLeft = offset
              , wiRight = offset + wsWidth summary
              , wiItemIndices = indices
              }
       in (newOffset, nextIndex', accItems ++ placedItems, accInfos ++ [info])

    placeWorkspace :: Int -> WorkspaceSummary -> Int -> ([AtlasItem], [Int], Int)
    placeWorkspace offset summary startIndex =
      let raws = wsItems summary
          count = length raws
          indices = [startIndex .. startIndex + count - 1]
          atlasItems = map (toAtlas offset) raws
          nextIndex = startIndex + count
       in (atlasItems, indices, nextIndex)

    toAtlas :: Int -> RawItem -> AtlasItem
    toAtlas offset raw =
      let shiftedRect =
            Rectangle
              { rect_x = fromIntegral (rectLeft (rawRect raw) + offset)
              , rect_y = rect_y (rawRect raw)
              , rect_width = rect_width (rawRect raw)
              , rect_height = rect_height (rawRect raw)
              }
          (cx, cy) = rectCenter shiftedRect
       in AtlasItem
            { itemWorkspace = rawWorkspace raw
            , itemWindow = rawWindow raw
            , itemRect = shiftedRect
            , itemTitle = rawTitle raw
            , itemOnCurrent = rawCurrent raw
            , itemCenterX = cx
            , itemCenterY = cy
            }

data WorkspaceSummary = WorkspaceSummary
  { wsTag :: !WorkspaceId
  , wsWidth :: !Int
  , wsHeight :: !Int
  , wsItems :: ![RawItem]
  }

rectFromAttributes :: WindowAttributes -> Rectangle
rectFromAttributes attrs =
  Rectangle
    { rect_x = fromIntegral (wa_x attrs)
    , rect_y = fromIntegral (wa_y attrs)
    , rect_width = ensurePositive (wa_width attrs)
    , rect_height = ensurePositive (wa_height attrs)
    }
  where
    ensurePositive v
      | v <= 0 = 1
      | otherwise = fromIntegral v

rectLeft :: Rectangle -> Int
rectLeft = fromIntegral . rect_x

rectTop :: Rectangle -> Int
rectTop = fromIntegral . rect_y

rectRight :: Rectangle -> Int
rectRight rect = rectLeft rect + fromIntegral (rect_width rect)

rectBottom :: Rectangle -> Int
rectBottom rect = rectTop rect + fromIntegral (rect_height rect)

innerRectangle :: Int -> Rectangle -> Rectangle
innerRectangle pad rect =
  let pad2 = pad * 2
      innerW = max 1 (fromIntegral (rect_width rect) - fromIntegral pad2)
      innerH = max 1 (fromIntegral (rect_height rect) - fromIntegral pad2)
      innerX = rectLeft rect + pad
      innerY = rectTop rect + pad
   in Rectangle
        { rect_x = fromIntegral innerX
        , rect_y = fromIntegral innerY
        , rect_width = fromIntegral innerW
        , rect_height = fromIntegral innerH
        }

scaleAtlasItems :: Double -> [AtlasItem] -> [AtlasItem]
scaleAtlasItems factor = map (scaleAtlasItem factor)

scaleAtlasItem :: Double -> AtlasItem -> AtlasItem
scaleAtlasItem factor item =
  let scaledRect = scaleRectangle factor (itemRect item)
      (cx, cy) = rectCenter scaledRect
   in item { itemRect = scaledRect, itemCenterX = cx, itemCenterY = cy }

scaleRectangle :: Double -> Rectangle -> Rectangle
scaleRectangle factor rect =
  Rectangle
    { rect_x = scaleCoordinate factor (rect_x rect)
    , rect_y = scaleCoordinate factor (rect_y rect)
    , rect_width = scaleLength factor (rect_width rect)
    , rect_height = scaleLength factor (rect_height rect)
    }

scaleCoordinate :: (Integral a, Integral b) => Double -> a -> b
scaleCoordinate factor value =
  fromIntegral (round (fromIntegral value * factor :: Double))

scaleLength :: (Integral a, Integral b) => Double -> a -> b
scaleLength factor value =
  let scaled = max 1 (round (fromIntegral value * factor :: Double))
   in fromIntegral scaled

translateAtlasItems :: Int -> Int -> [AtlasItem] -> [AtlasItem]
translateAtlasItems dx dy = map (translateAtlasItem dx dy)

translateAtlasItem :: Int -> Int -> AtlasItem -> AtlasItem
translateAtlasItem dx dy item =
  let rect = itemRect item
      translatedRect =
        rect
          { rect_x = rect_x rect + fromIntegral dx
          , rect_y = rect_y rect + fromIntegral dy
          }
      (cx, cy) = rectCenter translatedRect
   in item { itemRect = translatedRect, itemCenterX = cx, itemCenterY = cy }

rectCenter :: Rectangle -> (Double, Double)
rectCenter rect =
  ( fromIntegral (rect_x rect) + fromIntegral (rect_width rect) / 2
  , fromIntegral (rect_y rect) + fromIntegral (rect_height rect) / 2
  )

scaleWorkspaceInfo :: Double -> Int -> WorkspaceInfo -> WorkspaceInfo
scaleWorkspaceInfo factor pad info =
  info
    { wiLeft = scaleCoordinate factor (wiLeft info) + pad
    , wiRight = scaleCoordinate factor (wiRight info) + pad
    }

runOverlay ::
  GSConfig ->
  Display ->
  WorkspaceId ->
  AtlasLayout ->
  Int ->
  IO (Maybe Selection)
runOverlay cfg dpy focusedWorkspace layout initialIndex = do
  let items = layoutItems layout
      workspaceInfos = layoutWorkspaces layout
      layoutWidth' = max 1 (layoutWidth layout)
      layoutHeight' = max 1 (layoutHeight layout)
      screenNum = defaultScreen dpy
      screenWidth = fromIntegral (displayWidth dpy screenNum) :: Int
      screenHeight = fromIntegral (displayHeight dpy screenNum) :: Int
      maxWidth = max 1 (screenWidth * 3 `div` 4)
      maxHeight = max 1 (screenHeight * 3 `div` 4)
      scaleX = fromIntegral maxWidth / fromIntegral layoutWidth'
      scaleY = fromIntegral maxHeight / fromIntegral layoutHeight'
      scale =
        min 1.0 $
          min scaleX scaleY
      scaledWidth = max 1 (ceiling (fromIntegral layoutWidth' * scale))
      scaledHeight = max 1 (ceiling (fromIntegral layoutHeight' * scale))
      pad = padding cfg
      modelineH = max 1 (modelineHeight cfg)
      overlayWidth = scaledWidth + pad * 2
      overlayHeight = scaledHeight + pad * 2 + modelineH
      offsetItems =
        translateAtlasItems pad pad . scaleAtlasItems scale $ items
      scaledInfos = map (scaleWorkspaceInfo scale pad) workspaceInfos
      scaledLayout =
        layout
          { layoutWidth = overlayWidth
          , layoutHeight = overlayHeight
          , layoutItems = offsetItems
          , layoutWorkspaces = scaledInfos
          }
      posX = max 0 ((screenWidth - overlayWidth) `div` 2)
      posY = max 0 ((screenHeight - overlayHeight) `div` 2)
      overlayWidthDim = fromIntegral overlayWidth
      overlayHeightDim = fromIntegral overlayHeight
      posXPos = fromIntegral posX
      posYPos = fromIntegral posY
  env <- setupOverlay cfg dpy overlayWidthDim overlayHeightDim posXPos posYPos
  let items' = layoutItems scaledLayout
      safeIndex
        | null items' = 0
        | initialIndex < 0 = 0
        | initialIndex >= length items' = 0
        | otherwise = initialIndex
  let initialMode = HintModeFocus
  renderOverlay cfg env scaledLayout focusedWorkspace initialMode safeIndex
  result <-
    finally
      (runLoop cfg env scaledLayout focusedWorkspace initialMode safeIndex)
      (teardownOverlay env)
  pure result

setupOverlay ::
  GSConfig ->
  Display ->
  Dimension ->
  Dimension ->
  Position ->
  Position ->
  IO OverlayEnv
setupOverlay cfg dpy width height posX posY = do
  let screenNum = defaultScreen dpy
      screen = defaultScreenOfDisplay dpy
      visual = defaultVisualOfScreen screen
      depth = defaultDepthOfScreen screen
      cmap = defaultColormap dpy screenNum
  root <- rootWindow dpy screenNum
  colors <- resolveColors cfg dpy screenNum visual cmap
  win <-
    allocaSetWindowAttributes $ \attrs -> do
      set_override_redirect attrs True
      set_event_mask attrs (exposureMask .|. keyPressMask)
      set_colormap attrs cmap
      set_background_pixel attrs (colorBg colors)
      let mask =
            cWOverrideRedirect .|.
            cWEventMask .|.
            cWColormap .|.
            cWBackPixel
      createWindow dpy root posX posY width height 0 depth inputOutput visual mask attrs
  setWindowOpacity cfg dpy win
  pixmap <- createPixmap dpy win width height depth
  gc <- createGC dpy win
  draw <- xftDrawCreate dpy pixmap visual cmap
  font <- loadFont cfg dpy screen
  mapWindow dpy win
  raiseWindow dpy win
  _ <- grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
  pure
    OverlayEnv
      { oeDisplay = dpy
      , oeWindow = win
      , oeGC = gc
      , oeDraw = draw
      , oeFont = font
      , oeColors = colors
      , oeVisual = visual
      , oeColormap = cmap
      , oeWidth = width
      , oeHeight = height
      , oePixmap = pixmap
      }

setWindowOpacity :: GSConfig -> Display -> Window -> IO ()
setWindowOpacity cfg dpy win = do
  opacityAtom <- internAtom dpy "_NET_WM_WINDOW_OPACITY" False
  let clamped = max 0 (min 1 (overlayOpacity cfg))
      maxOpacity = 0xFFFFFFFF :: Word32
      value = round (clamped * fromIntegral maxOpacity) :: Word32
  changeProperty32 dpy win opacityAtom cARDINAL propModeReplace [fromIntegral value]

teardownOverlay :: OverlayEnv -> IO ()
teardownOverlay env = do
  let dpy = oeDisplay env
      win = oeWindow env
      gc = oeGC env
      draw = oeDraw env
      font = oeFont env
      colors = oeColors env
      colormap = oeColormap env
      pixmap = oePixmap env
  ungrabKeyboard dpy currentTime
  xftDrawDestroy draw
  freeGC dpy gc
  freePixmap dpy pixmap
  destroyWindow dpy win
  xftFontClose dpy font
  let owned = colorAllocated colors
  when (not (null owned)) $
    freeColors dpy colormap owned 0
  sync dpy False

resolveColors ::
  GSConfig ->
  Display ->
  ScreenNumber ->
  Visual ->
  Colormap ->
  IO OverlayColors
resolveColors cfg dpy screenNum _ cmap = do
  (bg, bgOwned) <- resolvePixel dpy screenNum cmap (bgColor cfg) (blackPixel dpy screenNum)
  (fg, fgOwned) <- resolvePixel dpy screenNum cmap (fgColor cfg) (whitePixel dpy screenNum)
  (accent, accentOwned) <- resolvePixel dpy screenNum cmap (accentColor cfg) fg
  (inactive, inactiveOwned) <- resolvePixel dpy screenNum cmap (inactiveColor cfg) fg
  (modelineBg, modelineBgOwned) <- resolvePixel dpy screenNum cmap (modelineBgColor cfg) bg
  (modelineFg, modelineFgOwned) <- resolvePixel dpy screenNum cmap (modelineFgColor cfg) fg
  let ownedPixels =
        concat
          [ if bgOwned then [bg] else []
          , if fgOwned then [fg] else []
          , if accentOwned then [accent] else []
          , if inactiveOwned then [inactive] else []
          , if modelineBgOwned then [modelineBg] else []
          , if modelineFgOwned then [modelineFg] else []
          ]
  pure
    OverlayColors
      { colorBg = bg
      , colorFg = fg
      , colorAccent = accent
      , colorInactive = inactive
      , colorModelineBg = modelineBg
      , colorModelineFg = modelineFg
      , colorAllocated = ownedPixels
      }
resolvePixel :: Display -> ScreenNumber -> Colormap -> String -> Pixel -> IO (Pixel, Bool)
resolvePixel dpy _ cmap colorName fallback = do
  result <- try (allocNamedColor dpy cmap colorName) :: IO (Either SomeException (Color, Color))
  pure $
    case result of
      Right (allocColor, _) -> (color_pixel allocColor, True)
      Left _ -> (fallback, False)

loadFont :: GSConfig -> Display -> Screen -> IO XftFont
loadFont cfg dpy screenObj = do
  result <- try (xftFontOpen dpy screenObj (fontName cfg))
  case result of
    Right font -> pure font
    Left (_ :: SomeException) -> xftFontOpen dpy screenObj "fixed"

renderOverlay :: GSConfig -> OverlayEnv -> AtlasLayout -> WorkspaceId -> HintMode -> Int -> IO ()
renderOverlay cfg env layout focusedWorkspace _currentMode selectedIndex = do
  let items = layoutItems layout
      hoverTag = fromMaybe "-" (selectedWorkspace items selectedIndex)
      segments =
        [ "Hover: " ++ hoverTag
        , "Focused: " ++ focusedWorkspace
        ]
      modelineText = intercalate "  |  " segments
  drawAtlas cfg env items selectedIndex
  drawModeline cfg env modelineText
  presentBuffer env

drawAtlas :: GSConfig -> OverlayEnv -> [AtlasItem] -> Int -> IO ()
drawAtlas cfg env items selectedIndex = do
  let dpy = oeDisplay env
      gc = oeGC env
      draw = oeDraw env
      font = oeFont env
      colors = oeColors env
      visual = oeVisual env
      cmap = oeColormap env
      pad = padding cfg
      borderPixel = colorFg colors
      pix = oePixmap env
      width = oeWidth env
      height = oeHeight env
  ascent <- xftfont_ascent font
  descent <- xftfont_descent font
  ellipsisGlyph <- xftTextExtents dpy font "..."
  let ellipsisWidth = fromIntegral (xglyphinfo_xOff ellipsisGlyph)
      contentInset = max 8 (pad `div` 4)
      textPadding = max 6 (pad `div` 6)
      fontHeight = ascent + descent
      minContentWidth = max 16 (textPadding * 2 + ellipsisWidth)
      minContentHeight = max 16 (textPadding * 2 + fontHeight)
  setForeground dpy gc (colorBg colors)
  fillRectangle dpy pix gc 0 0 width height
  withXftColorName dpy visual cmap (fgColor cfg) $ \txtColor -> do
    mapM_ (uncurry (drawItem dpy pix gc draw font colors cfg ascent txtColor contentInset textPadding minContentWidth minContentHeight borderPixel selectedIndex)) (zip [0 ..] items)
  where
    drawItem dpy pix gc draw font colors cfg ascent txtColor contentInset textPadding minContentWidth minContentHeight borderPixel selectedIdx idx item = do
      let rect = itemRect item
          fillRectBase = innerRectangle contentInset rect
          baseW = fromIntegral (rect_width fillRectBase) :: Int
          baseH = fromIntegral (rect_height fillRectBase) :: Int
          contentW = max minContentWidth baseW
          contentH = max minContentHeight baseH
          adjustedRect =
            fillRectBase
              { rect_width = fromIntegral contentW
              , rect_height = fromIntegral contentH
              }
          originX = rectLeft adjustedRect
          originY = rectTop adjustedRect
          availableWidth = max 1 (contentW - textPadding * 2)
          maxBaseline = originY + contentH - textPadding
          textBaseline = min maxBaseline (originY + textPadding + ascent)
          textX = originX + textPadding
          fillPixel =
            if itemOnCurrent item
              then colorAccent colors
              else colorInactive colors
          rectX = fromIntegral (rect_x adjustedRect) :: Position
          rectY = fromIntegral (rect_y adjustedRect) :: Position
          rectW = rect_width adjustedRect
          rectH = rect_height adjustedRect
          borderWidthDim = fromIntegral (max 1 (contentW - 1)) :: Dimension
          borderHeightDim = fromIntegral (max 1 (contentH - 1)) :: Dimension
      setForeground dpy gc fillPixel
      fillRectangle dpy pix gc
        rectX
        rectY
        rectW
        rectH
      when (idx == selectedIdx) $ do
        setForeground dpy gc borderPixel
        setLineAttributes dpy gc 2 lineSolid capButt joinMiter
        drawRectangle dpy pix gc rectX rectY borderWidthDim borderHeightDim
        setLineAttributes dpy gc 0 lineSolid capButt joinMiter
      title <- prepareTitle dpy font availableWidth (itemTitle item)
      xftDrawString
        draw
        txtColor
        font
        (fromIntegral textX :: Int)
        (fromIntegral textBaseline :: Int)
        title

selectedWorkspace :: [AtlasItem] -> Int -> Maybe WorkspaceId
selectedWorkspace items idx
  | idx < 0 = Nothing
  | idx >= length items = Nothing
  | otherwise = Just (itemWorkspace (items !! idx))

drawModeline :: GSConfig -> OverlayEnv -> String -> IO ()
drawModeline cfg env text = do
  let dpy = oeDisplay env
      gc = oeGC env
      draw = oeDraw env
      font = oeFont env
      colors = oeColors env
      visual = oeVisual env
      cmap = oeColormap env
      widthDim = oeWidth env
      heightDim = oeHeight env
      modelineH = max 1 (modelineHeight cfg)
      widthInt = fromIntegral widthDim :: Int
      heightInt = fromIntegral heightDim :: Int
      modelineTop = max 0 (heightInt - modelineH)
      horizontalPad = max 10 (padding cfg `div` 3)
  ascent <- xftfont_ascent font
  descent <- xftfont_descent font
  let contentHeight = ascent + descent
      verticalSpace = max 0 (modelineH - contentHeight)
      padYLimit = max 0 (modelineH - contentHeight)
      padYCandidate = max 2 (verticalSpace `div` 2)
      padY = min padYCandidate padYLimit
      baselineRaw = modelineTop + padY + ascent
      minBaseline = modelineTop + ascent
      maxBaseline = modelineTop + modelineH - max 1 (descent + padY)
      baseline = max minBaseline (min baselineRaw maxBaseline)
      availableWidth = max 1 (widthInt - horizontalPad * 2)
      pix = oePixmap env
      modelineTopPos = fromIntegral modelineTop :: Position
      modelineHeightDim = fromIntegral modelineH :: Dimension
  setForeground dpy gc (colorModelineBg colors)
  fillRectangle dpy pix gc 0 modelineTopPos widthDim modelineHeightDim
  let widthPos = fromIntegral (max 0 (widthInt - 1)) :: Position
  setForeground dpy gc (colorModelineFg colors)
  drawLine dpy pix gc 0 modelineTopPos widthPos modelineTopPos
  prepared <- prepareTitle dpy font availableWidth text
  withXftColorName dpy visual cmap (modelineFgColor cfg) $ \modelineFg ->
    xftDrawString
      draw
      modelineFg
      font
      (fromIntegral horizontalPad :: Int)
      (fromIntegral baseline :: Int)
      prepared

presentBuffer :: OverlayEnv -> IO ()
presentBuffer env = do
  let dpy = oeDisplay env
      win = oeWindow env
      pix = oePixmap env
      gc = oeGC env
      width = oeWidth env
      height = oeHeight env
  copyArea dpy pix win gc 0 0 width height 0 0
  sync dpy False

prepareTitle :: Display -> XftFont -> Int -> String -> IO String
prepareTitle _ _ availableWidth _
  | availableWidth <= 0 = pure ""
prepareTitle dpy font availableWidth title = do
  glyph <- xftTextExtents dpy font title
  let width = fromIntegral (xglyphinfo_xOff glyph)
  if width <= availableWidth
    then pure title
    else do
      ellipsisGlyph <- xftTextExtents dpy font ellipsis
      let ellipsisWidth = fromIntegral (xglyphinfo_xOff ellipsisGlyph)
      if ellipsisWidth >= availableWidth
        then pure ""
        else shrink title
  where
    ellipsis = "..."
    shrink [] = pure ellipsis
    shrink txt =
      let trimmed = take (length txt - 1) txt
       in if null trimmed
            then pure ellipsis
            else do
              glyph <- xftTextExtents dpy font (trimmed ++ ellipsis)
              let width = fromIntegral (xglyphinfo_xOff glyph)
              if width <= availableWidth
                then pure (trimmed ++ ellipsis)
                else shrink trimmed

runLoop ::
  GSConfig ->
  OverlayEnv ->
  AtlasLayout ->
  WorkspaceId ->
  HintMode ->
  Int ->
  IO (Maybe Selection)
runLoop cfg env layout focusedWorkspace startMode startIndex =
  allocaXEvent $ \ev -> loop startIndex startMode ev
  where
    dpy = oeDisplay env
    items = layoutItems layout
    loop currentIndex currentMode evPtr = do
      nextEvent dpy evPtr
      event <- getEvent evPtr
      case event of
        ExposeEvent {} -> do
          renderOverlay cfg env layout focusedWorkspace currentMode currentIndex
          loop currentIndex currentMode evPtr
        KeyEvent {ev_event_type = t, ev_keycode = code}
          | t == keyPress -> do
              keysym <- keycodeToKeysym dpy code 0
              handleKey keysym currentIndex currentMode evPtr
          | otherwise -> loop currentIndex currentMode evPtr
        _ -> loop currentIndex currentMode evPtr

    handleKey keysym currentIndex currentMode evPtr
      | keysym == xK_Escape = pure Nothing
      | keysym == xK_Return || keysym == xK_KP_Enter =
          pure $
            if null items
              then Nothing
              else Just (SelectionFocus (items !! currentIndex))
      | keysym == xK_g = do
          (hintSelection, newMode) <- runHintSession cfg env layout focusedWorkspace currentIndex currentMode atlasHintSettings
          case hintSelection of
            Just selection -> pure (Just selection)
            Nothing -> do
              renderOverlay cfg env layout focusedWorkspace newMode currentIndex
              loop currentIndex newMode evPtr
      | keysym == xK_h = move DirLeft currentIndex currentMode
      | keysym == xK_l = move DirRight currentIndex currentMode
      | keysym == xK_k = move DirUp currentIndex currentMode
      | keysym == xK_j = move DirDown currentIndex currentMode
      | otherwise = loop currentIndex currentMode evPtr
      where
        move dir idx modeState = do
          let nextIdx = navigate dir layout idx
          renderOverlay cfg env layout focusedWorkspace modeState nextIdx
          loop nextIdx modeState evPtr

data Direction = DirLeft | DirRight | DirUp | DirDown

atlasHintSettings :: HintSettings
atlasHintSettings =
  HintSettings
    { hintAlphabet = defaultHintAlphabet
    , hintLeader = xK_space
    }

defaultHintAlphabet :: String
defaultHintAlphabet = "asdfghjklqwertyuiopzxcvbnm1234567890"

toggleHintMode :: HintMode -> HintMode
toggleHintMode HintModeFocus = HintModeSwap
toggleHintMode HintModeSwap = HintModeFocus

assignHints :: String -> [AtlasItem] -> [HintItem]
assignHints alphabet items =
  let usableAlphabet = if null alphabet then defaultHintAlphabet else alphabet
   in zip (map (hintLabel usableAlphabet) [0 ..]) items

hintLabel :: String -> Int -> String
hintLabel alphabet idx = encode (idx + 1)
  where
    base = length alphabet
    encode n =
      let (q, r) = (n - 1) `divMod` base
          char = alphabet !! r
       in if q == 0
            then [char]
            else encode q ++ [char]

runHintSession ::
  GSConfig ->
  OverlayEnv ->
  AtlasLayout ->
  WorkspaceId ->
  Int ->
  HintMode ->
  HintSettings ->
  IO (Maybe Selection, HintMode)
runHintSession cfg env layout focusedWorkspace currentIndex initialMode settings
  | null hintItems = pure (Nothing, initialMode)
  | otherwise = allocaXEvent $ \ev -> loop initialState ev
  where
    dpy = oeDisplay env
    leader = hintLeader settings
    rawAlphabet = map toLower (hintAlphabet settings)
    alphabet = if null rawAlphabet then map toLower defaultHintAlphabet else rawAlphabet
    alphabetChars = alphabet
    hintItems = assignHints alphabet (layoutItems layout)
    initialState = HintState "" initialMode

    loop state evPtr = do
      renderHints cfg env layout focusedWorkspace currentIndex hintItems state
      nextEvent dpy evPtr
      event <- getEvent evPtr
      case event of
        ExposeEvent {} -> loop state evPtr
        KeyEvent {ev_event_type = t, ev_keycode = code}
          | t == keyPress -> do
              keysym <- keycodeToKeysym dpy code 0
              case stepHint state keysym of
                HintResultCancel -> pure (Nothing, hsMode state)
                HintResultFinish sel -> pure (Just sel, hsMode state)
                HintResultContinue newState -> loop newState evPtr
          | otherwise -> loop state evPtr
        _ -> loop state evPtr

    stepHint :: HintState -> KeySym -> HintResult
    stepHint state keysym
      | keysym == xK_Escape = HintResultCancel
      | keysym == leader = HintResultContinue state { hsMode = toggleHintMode (hsMode state) }
      | keysym == xK_BackSpace =
          let prefix = hsPrefix state
           in HintResultContinue state { hsPrefix = if null prefix then prefix else init prefix }
      | keysym == xK_Return =
          let prefix = hsPrefix state
              matches = filter (isPrefixOf prefix . fst) hintItems
              exact = filter ((== prefix) . fst) matches
           in case (null prefix, exact, matches) of
                (True, _, _) -> HintResultContinue state
                (_, (label, item) : _, _) | label == prefix -> HintResultFinish (selectionFromMode item (hsMode state))
                (_, [], [(_, item)]) -> HintResultFinish (selectionFromMode item (hsMode state))
                _ -> HintResultContinue state
      | otherwise =
          case keysymToHintChar keysym of
            Just c
              | c `elem` alphabetChars ->
                  let prefix' = hsPrefix state ++ [c]
                      matches = filter (isPrefixOf prefix' . fst) hintItems
                   in if null matches
                        then HintResultContinue state
                        else
                          let exact = filter ((== prefix') . fst) matches
                           in case exact of
                                ((label, item) : _) | label == prefix' -> HintResultFinish (selectionFromMode item (hsMode state))
                                _ -> HintResultContinue state { hsPrefix = prefix' }
            _ -> HintResultContinue state

    selectionFromMode :: AtlasItem -> HintMode -> Selection
    selectionFromMode item mode =
      case mode of
        HintModeFocus -> SelectionFocus item
        HintModeSwap -> SelectionSwap item

data HintResult
  = HintResultContinue HintState
  | HintResultFinish Selection
  | HintResultCancel

renderHints ::
  GSConfig ->
  OverlayEnv ->
  AtlasLayout ->
  WorkspaceId ->
  Int ->
  [HintItem] ->
  HintState ->
  IO ()
renderHints cfg env layout focusedWorkspace currentIndex hintItems state = do
  let items = layoutItems layout
      dpy = oeDisplay env
      gc = oeGC env
      pix = oePixmap env
      draw = oeDraw env
      font = oeFont env
      visual = oeVisual env
      cmap = oeColormap env
      colors = oeColors env
      pad = padding cfg
      prefix = hsPrefix state
      mode = hsMode state
  let hoverTag = fromMaybe "-" (selectedWorkspace items currentIndex)
      baseSegments =
        [ "Hover: " ++ hoverTag
        , "Focused: " ++ focusedWorkspace
        ]
      extraSegments =
        if null prefix
          then []
          else ["Keys: " ++ prefix]
      modelineText = intercalate "  |  " (baseSegments ++ extraSegments)
      matches = filter (isPrefixOf prefix . fst) hintItems
      matchLabels = map fst matches
      exactLabels = filter (== prefix) matchLabels
  drawAtlas cfg env items currentIndex
  ascent <- xftfont_ascent font
  descent <- xftfont_descent font
  let hintPadding = max 4 (pad `div` 6)
      hintBorderWidth = 2
      baseHintHeight = ascent + descent + hintPadding * 2
      minHintWidth = hintPadding * 2 + 12
  withXftColorName dpy visual cmap (fgColor cfg) $ \fgText ->
    withXftColorName dpy visual cmap (accentColor cfg) $ \accentText ->
      withXftColorName dpy visual cmap (inactiveColor cfg) $ \inactiveText -> do
        let drawHint (label, item) = do
              let rect = itemRect item
                  originX = rectLeft rect + pad
                  originY = rectTop rect + pad
                  textColor
                    | label `elem` exactLabels = accentText
                    | label `elem` matchLabels = fgText
                    | null prefix = fgText
                    | otherwise = inactiveText
              glyph <- xftTextExtents dpy font label
              let textWidth = fromIntegral (xglyphinfo_xOff glyph)
                  boxWidth = max minHintWidth (textWidth + hintPadding * 2)
                  boxHeight = baseHintHeight
                  boxX = fromIntegral originX :: Position
                  boxY = fromIntegral originY :: Position
                  boxWidthDim = fromIntegral boxWidth :: Dimension
                  boxHeightDim = fromIntegral boxHeight :: Dimension
                  textX = originX + hintPadding
                  textBaseline = originY + hintPadding + ascent
                  fillPixel
                    | label `elem` exactLabels = colorAccent colors
                    | label `elem` matchLabels = colorBg colors
                    | otherwise = colorInactive colors
                  borderWidthDim = fromIntegral (max 1 (boxWidth - 1)) :: Dimension
                  borderHeightDim = fromIntegral (max 1 (boxHeight - 1)) :: Dimension
              setForeground dpy gc fillPixel
              fillRectangle dpy pix gc boxX boxY boxWidthDim boxHeightDim
              setLineAttributes dpy gc hintBorderWidth lineSolid capButt joinMiter
              setForeground dpy gc (colorFg colors)
              drawRectangle dpy pix gc boxX boxY borderWidthDim borderHeightDim
              setLineAttributes dpy gc 0 lineSolid capButt joinMiter
              xftDrawString draw textColor font (fromIntegral textX) (fromIntegral textBaseline) label
        mapM_ drawHint hintItems
        drawModeline cfg env modelineText
  presentBuffer env
  where
keysymToHintChar :: KeySym -> Maybe Char
keysymToHintChar sym =
  case keysymToString sym of
    [c]
      | isPrint c -> Just (toLower c)
    _ -> Nothing

navigate :: Direction -> AtlasLayout -> Int -> Int
navigate _ layout idx | null (layoutItems layout) = idx
navigate dir layout idx =
  case dir of
    DirUp -> moveVertical DirUp
    DirDown -> moveVertical DirDown
    DirLeft -> moveHorizontal (-1)
    DirRight -> moveHorizontal 1
  where
    items = layoutItems layout
    infos = layoutWorkspaces layout
    origin = items !! idx
    wsId = itemWorkspace origin
    originX = itemCenterX origin
    originY = itemCenterY origin
    eps = 1e-3

    findWorkspaceInfo w =
      fromMaybe
        WorkspaceInfo { wiWorkspace = w, wiLeft = 0, wiRight = 0, wiItemIndices = [idx] }
        (find ((== w) . wiWorkspace) infos)

    moveVertical direction =
      let info = findWorkspaceInfo wsId
          annotated =
            [ (i, candidate, abs (itemCenterX candidate - originX), itemCenterY candidate - originY)
            | i <- wiItemIndices info
            , i /= idx
            , let candidate = items !! i
            ]
          near =
            case annotated of
              [] -> []
              _ ->
                let minDx = minimum (map thirdH annotated)
                 in filter (\(_, _, dx, _) -> dx <= minDx + horizontalTolerance) annotated
          forward = filter (\t -> matches direction (verticalDisplacement t)) near
       in case near of
            [] -> idx
            _ ->
              case forward of
                [] -> choose (fallback direction near)
                xs -> choose (pickForward xs)

    horizontalTolerance = 10.0

    matches DirUp dy = dy < -eps
    matches DirDown dy = dy > eps
    matches _ _ = False

    pickForward :: [Annotated] -> Annotated
    pickForward = minimumBy (comparing forwardKey)

    forwardKey :: Annotated -> (Double, Double, Double)
    forwardKey (_, _, dx, dy) = (dx, abs dy, abs dy + dx)

    fallback :: Direction -> [Annotated] -> Annotated
    fallback dir candidates =
      case dir of
        DirUp -> maximumBy (comparing verticalDisplacement) candidates
        DirDown -> minimumBy (comparing verticalDisplacement) candidates
        _ -> fallbackDefault candidates

    fallbackDefault :: [Annotated] -> Annotated
    fallbackDefault (x : _) = x
    fallbackDefault [] = (idx, origin, 0, 0)

    verticalDisplacement :: Annotated -> Double
    verticalDisplacement (_, _, _, dy) = dy

    thirdH :: Annotated -> Double
    thirdH (_, _, dx, _) = dx

    choose :: Annotated -> Int
    choose (i, _, _, _) = i

    moveHorizontal delta =
      let annotated =
            [ (i, candidate, abs (itemCenterY candidate - originY), itemCenterX candidate - originX)
            | (i, candidate) <- zip [0 ..] items
            , i /= idx
            ]
          forward = filter (matchesHorizontal delta . fourthH) annotated
       in case annotated of
            [] -> idx
            _ ->
              case forward of
                [] -> chooseH (wrapHorizontal delta annotated)
                xs -> chooseH (minimumBy (comparing horizontalKey) xs)

    matchesHorizontal dirSign dx
      | dirSign < 0 = dx < -eps
      | dirSign > 0 = dx > eps
      | otherwise = False

    horizontalKey (_, _, dy, dx) = (abs dx, dy, abs dx + dy)

    wrapHorizontal dirSign candidates =
      let comparator = comparing fourthH
       in if dirSign < 0
            then maximumBy comparator candidates
            else minimumBy comparator candidates

    fourthH (_, _, _, dx) = dx

    chooseH (i, _, _, _) = i

swapWithFocused :: Window -> X ()
swapWithFocused win =
  withWindowSet $ \ws ->
    case W.peek ws of
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

type Annotated = (Int, AtlasItem, Double, Double)
