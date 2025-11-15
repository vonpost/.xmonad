import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.BinarySpacePartition
import XMonad.Actions.SpawnOn
import MyKeyBindings
-- import MyPywal
import XMonad.Hooks.EwmhDesktops 
import XMonad.Layout.Tabbed
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.Layout.Hidden
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.SimplestFloat






main :: IO ()
main =
  xmonad
    $ withNavigation2DConfig def { defaultTiledNavigation = hybridOf lineNavigation centerNavigation }
    $ myConfig

myDefaultLayout = limitWindows 4 $ spacingRaw True (Border 30 30 30 30) True (Border 30 30 30 30) True
                         $ hiddenWindows $ smartBorders emptyBSP
myFloatLayout = simplestFloat
myConfig = ewmhFullscreen $ ewmh def
  { borderWidth        = 5
  , focusedBorderColor = "#000000" --"#C0C0C0"
  , normalBorderColor =  "#000000" --"#C0C0C0"
  , startupHook = startupHook def <+> setWMName "LG3D" -- <+> pywalStartupHook
  , logHook = logHook def -- <+> pywalLogHook
  , handleEventHook = handleEventHook def
  , focusFollowsMouse  = False
  , keys               = myKeys
  , layoutHook         = onWorkspace "etc" myFloatLayout  $ myDefaultLayout


  , modMask            = mod4Mask
  , manageHook         = manageSpawn <+> manageHook def <+> (namedScratchpadManageHook myScratchpads) <+> fullscreenManageHook <+> gapsOnManageHook
  , terminal           = "alacrittyc"
  , workspaces         =  [ "browse", "code", "read", "chat", "etc"]
  }
