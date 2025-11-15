{-# LANGUAGE ForeignFunctionInterface #-}

module MyKeyBindings where
{- base -}
import Control.Arrow (second)
import Control.Monad (void, when, (>=>))
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Data.Bits ((.|.))
import Data.Dynamic (Typeable)
import Data.List (elemIndex, find, intercalate, isInfixOf, nub)
import qualified Data.Map.Strict as M
import Data.Monoid (All)
import Data.Char (isSpace)
import System.Directory (getHomeDirectory)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO (Handle)
import XMonad.Hooks.RefocusLast
  ( RefocusLastLayoutHook, isFloat, refocusLastLayoutHook, refocusLastWhen,
    shiftRLWhen, swapWithLast, toggleFocus,
  )

import XMonad

import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.Warp
import qualified XMonad.StackSet as W

import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.IndependentScreens


import XMonad.Util.CustomKeys
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.Hidden
import XMonad.Layout.Spacing
import XMonad.Actions.OnScreen
import XMonad.Actions.GeometrySelect --(geometrySelect, defaultGSConfig, GSConfig)
--import MyPywal (pywalPrepareWorkspace)
import MyWindowHints
  ( ColorSpec(..)
  , HintConfig(..)
  , ModeColorSpec(..)
  , defaultHintConfig
  , windowHints
  )

import qualified XMonad.Util.ExtensibleState as XS
import qualified Graphics.X11.Xlib as X
import Graphics.X11.Xlib.Extras
  ( changeProperty32
  , getWindowProperty32
  , xDeleteProperty
  , propModeReplace
  )

-- Track whether gaps are currently enabled
newtype GapsOn = GapsOn { unGapsOn :: Bool }
        deriving (Read, Show, Typeable)
instance ExtensionClass GapsOn where
  initialValue = GapsOn True
  extensionType = PersistentExtension

picomFlagAtomName :: String
picomFlagAtomName = "_XMONAD_NOGAPS"   -- 1 = no gaps (disable shadows), 0 = gaps on (allow them)

setPropOn :: X.Window -> Bool -> X ()
setPropOn w gapsOn = withDisplay $ \d -> io $ do
  a <- X.internAtom d picomFlagAtomName False
  c <- X.internAtom d "CARDINAL" False
  let v = if gapsOn then 0 else 1   -- gaps on  -> 0
                    -- gaps off -> 1
  changeProperty32 d w a c propModeReplace [fromIntegral v]

applyPropToAll :: Bool -> X ()
applyPropToAll gapsOn = withWindowSet $ \ws ->
  mapM_ (\w -> setPropOn w gapsOn) (W.allWindows ws)

setSpacingEnabled bool = do
  setWindowSpacingEnabled bool
  setScreenSpacingEnabled bool

toggleGapsAndPicom = do
  GapsOn cur <- XS.get
  let newState = not cur   -- flip our state

  -- Set spacing based on explicit true/false
  setSpacingEnabled newState

  -- Tag/untag windows for picom (you already have this)
  applyPropToAll newState

  -- commit the new state
  XS.put (GapsOn newState)

gapsOnManageHook :: ManageHook
gapsOnManageHook =  do
                        w <- ask
                        liftX $ XS.get >>= \(GapsOn cur) -> setPropOn w cur
                        idHook

myKeys = customKeys removedKeys addedKeys

-- Quake-style dropdown terminal (Alacritty)
myScratchpads :: [NamedScratchpad]
myScratchpads =
  [ NS "quake"
       spawnQ
       findQ
       manageQ
  ]
  where
    -- Give it a unique X class so we can match it cleanly
    -- NOTE: Alacritty uses --class INSTANCE,CLASS
    spawnQ = "alacrittyc --class dropdown,dropdown -o window.dimensions.columns=120 -o window.dimensions.lines=34"
    findQ  = resource =? "dropdown"
    manageQ = customFloating $ W.RationalRect l t w h
      where
        w = 0.38   -- 38% width
        h = 0.28   -- 38% height (thin “quake bar”)
        l = (1 - w)/2
        t = 0.38   -- top of the screen

windowHintConfig :: HintConfig
windowHintConfig =
  -- Tweak the leader, font, or colors here if you want a different look.
  defaultHintConfig
    { hcLeaderKey = xK_space
    , hcFocusColors = ModeColorSpec (ColorName "#282a36") (ColorName "#bd93f9")
    , hcSwapColors = ModeColorSpec (ColorName "#bd93f9") (ColorName "#f8f8f2")
    , hcFontName = Just "-misc-fixed-bold-r-normal--18-120-100-100-c-90-iso8859-1"
    }

myGSConfig = defaultGSConfig
  { bgColor =  "#282a36"
  , fgColor =  "#ff79c6"
  , accentColor = "#584b83"
  , inactiveColor = "#44475a"
  , modelineHeight = 30
  , modelineFgColor = "#ff79c6"
  , modelineBgColor = "#282a36"
  , overlayOpacity = 0.85
  }

removedKeys :: XConfig l -> [(KeyMask, KeySym)]
removedKeys XConfig {modMask = modm} =
    [--(modm              , xK_space)  -- Default for layout switching
      (modm .|. shiftMask, xK_Return) -- Default for opening a terminal
    , (modm .|. shiftMask, xK_c)      -- Default for closing the focused window
    ]

addedKeys :: XConfig l -> [((KeyMask, KeySym), X ())]
addedKeys conf@XConfig {modMask = modm} =
  [ -- Application launcher
    ((0, 0xff61) , spawn "rofi -combi-modi window,drun -show combi -modi combi")

    -- Terminal
  , ((modm, xK_Return), spawn $ XMonad.terminal conf)
    -- Emacs
  , ((modm, xK_e), spawn "emacsclient -c -n -e '(switch-to-buffer nil)'")
    -- Close application
  , ((modm, xK_w), kill)

    -- Modify spacing
  , ((modm, xK_Right), incScreenWindowSpacing 10)
  , ((modm, xK_Left), decScreenWindowSpacing 10)
  , ((modm, xK_Up), toggleGapsAndPicom)

  -- Window hints
  , ((modm, xK_g), windowHints windowHintConfig)


    -- Switch to last workspace
  , ((modm, xK_Tab), geometrySelect myGSConfig)
  , ((modm, xK_u), toggleHidden)
  , ((modm, xK_b), withFocused hideWindow)
  , ((modm, xK_n), popNewestHiddenWindow)
  , ((modm, xK_m), popOldestHiddenWindow)
    -- Rotate windows
  , ((modm, xK_r), sendMessage Rotate)

    -- Swap windows
  -- , ((modm, xK_t), sendMessage Swap)
    -- Open quake terminal dropdown
  , ((modm, xK_f), namedScratchpadAction myScratchpads "quake")

    -- Open rofi-pass, password selector
  , ((modm, xK_p), spawn "rofi-pass")
    -- Layout switching
  --, ((modm .|. shiftMask, xK_t), sendMessage NextLayout)
  -- wow autojump
  -- , ((modm, xK_o), spawn "sleep 0.2 && xdotool keydown period keyup period")
    -- Directional navigation of windows
  , ((modm, xK_l), windowGo R False)
  , ((modm, xK_h), windowGo L False)
  , ((modm, xK_k), windowGo U False)
  , ((modm, xK_j), windowGo D False)

    -- Go to workspace, show which one
  , ((modm, xK_1), goToWorkspace "browse" "browse")
  , ((modm, xK_2), goToWorkspace "code"   "code")
  , ((modm, xK_3), goToWorkspace "read"   "read")
  , ((modm, xK_4), goToWorkspace "chat"   "chat")
  , ((modm, xK_5), goToWorkspace "etc"    "etc")

  -- Cycle screens

    -- Expand and shrink windows
  , ((modm .|. shiftMask,                xK_l), sendMessage $ ExpandTowards R)
  , ((modm .|. shiftMask,                xK_h), sendMessage $ ExpandTowards L)
  , ((modm .|. shiftMask,                xK_j), sendMessage $ ExpandTowards D)
  , ((modm .|. shiftMask,                xK_k), sendMessage $ ExpandTowards U)
  , ((modm .|. controlMask , xK_l), sendMessage $ ShrinkFrom R)
  , ((modm .|. controlMask , xK_h), sendMessage $ ShrinkFrom L)
  , ((modm .|. controlMask , xK_j), sendMessage $ ShrinkFrom D)
  , ((modm .|. controlMask , xK_k), sendMessage $ ShrinkFrom U)

    -- Toggle keyboard layouts
 , ((0, xF86XK_Search), toggleLanguage)
 , ((modm, xK_i), toggleLanguage)
    -- Brightness control
  , ((0, xF86XK_MonBrightnessUp), incLight)
  , ((0, xF86XK_MonBrightnessDown), decLight)
  , ((0, xF86XK_Display), toggleLight)

    --XF86AudioMicMute
  , ((0, xF86XK_AudioMicMute), toggleMic)
    -- XF86AudioMute
  , ((0, xF86XK_AudioMute), toggleAudio)

    -- XF86AudioRaiseVolume
  , ((0, xF86XK_AudioRaiseVolume), spawn "pamixer -i 5 && notify-send -h int:value:$(pamixer --get-volume) Volume")

    -- XF86AudioLowerVolume
  , ((0, xF86XK_AudioLowerVolume), spawn "pamixer -d 5 && notify-send -h int:value:$(pamixer --get-volume) Volume")

    -- Show date
  , ((modm, xK_a), spawn "notify-send \"$(date +%A\\,\\ %d\\ %B\\,\\ %R)\"")

    -- Show battery
  , ((modm, xK_s), spawn "notify-send \"$(acpi)\"")

    -- Screenshots
  , ((modm, xF86XK_Favorites ), spawn "maim ~/Pictures/$(date +%s).png")
  , ((0, xF86XK_Favorites ), spawn "maim -s ~/Pictures/$(date +%s).png")

  ] ++ screenKeys
  where
    action key sc = do
      screenWorkspace sc
      warpToScreen sc (0.5) (0.5)
    goToWorkspace name message = do
      toggleOrView name
      spawn ("notify-send \"" ++ message ++ "\"")
    screenKeys =
      [
      ((modm .|. controlMask, key),
      (action key sc ))
      | (key,sc) <- zip [xK_Left, xK_Right] [0..]
      -- (f, m) <- [ (pure . W.view, noModMask),
      --             (shiftRLWhen isFloat, shiftMask)
      --           ]
      ]
    -- screenKeys =
    --   [ ( (modm .|. controlMask, key),

    --   screenWorkspace sc >>= flip whenJust (f >=> windows)
    --   | (key, sc) <- zip [xK_1, xK_2, xK_3] [0..],
    --   (f, m) <- [ (pure . W.view, noModMask),
    --               (shiftRLWhen isFloat, shiftMask)
    --             ]
    --   ]



-- some help functions

toggleMic = do status <- runProcessWithInput "pamixer" ["--get-mute", "--default-source", "-t"] ""
               let status_filtered = filter (not . isSpace) status
               let notification = if status_filtered == "true" then "muted" else "unmuted"
               safeSpawn "notify-send" ["mic", notification]

toggleAudio = do status <- runProcessWithInput "pamixer" ["--get-mute", "-t"] ""
                 let status_filtered = filter (not . isSpace) status
                 let notification = if status_filtered == "true" then "muted" else "unmuted"
                 safeSpawn "notify-send" ["audio", notification]
              

setLight setStr = do spawn $  "brightnessctl -m set " ++ setStr--status <- runProcessWithInput "brightnessctl" ["-m"] $ "set " ++  setStr
                     --let status = "amdgpu_bl1,backlight,32382,50%,64764"
                     status <- runProcessWithInput "brightnessctl" ["-m"] "s 0%+"
                     let percentage = reverse $ takeWhile (\x -> not $ x == ',') $ reverse $ takeWhile (\x -> not $ x == '%') status
                     safeSpawn "notify-send" ["-h", "int:value:" ++ percentage, " Brightness"]
incLight = setLight "5%+"
decLight = setLight "5%-"

switchLight :: Int -> String
switchLight percent = if percent > 0 then "0" else "100"
toggleLight = do brightness <- runProcessWithInput "brightnessctl" ["get"] ""
                 max <- runProcessWithInput "brightnessctl" ["m"] ""
                 let currentPercentage = 100*(read brightness)/(read max)
                 let toggle = switchLight $ read brightness
                 let cmd = "brightnessctl set " ++ toggle ++ "%"
                 let notification = "notify-send -h int:value:" ++ toggle ++ " Brightness"
                 (spawn cmd) >> (spawn notification)


instance ExtensionClass Bool where
  initialValue = False
toggleHidden' state = if state then (popNewestHiddenWindow) >> return False  else (withFocused hideWindow) >> return True
toggleHidden = do state <- XS.get
                  state' <- toggleHidden' state
                  XS.put state'

languages = ["se", "us"]
toggleLanguage = do status <- runProcessWithInput "setxkbmap" ["-query"] ""
                    let _:currentLanguage:_ = words . head . drop 2 $ lines status
                    let language:_ = filter (\x -> x /= currentLanguage) languages
                    spawn $ "setxkbmap " ++ language ++  " && notify-send \"" ++ language ++ "\""
