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
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders


main :: IO ()
main =
  xmonad
    $ withNavigation2DConfig def { defaultTiledNavigation = hybridOf lineNavigation centerNavigation }
    $ myConfig

myConfig = ewmhFullscreen $ ewmh def
  { borderWidth        = 3
  , focusedBorderColor = "#000000"
  , normalBorderColor = "#000000"
  , startupHook = startupHook def <+> setWMName "LG3D" -- <+> pywalStartupHook
  , logHook = logHook def -- <+> pywalLogHook
  , handleEventHook = handleEventHook def
  , focusFollowsMouse  = False
  , keys               = myKeys
  , layoutHook         = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
                         $ hiddenWindows $ smartBorders emptyBSP
  , modMask            = mod4Mask
  , manageHook         = manageSpawn <+> manageHook def <+> (namedScratchpadManageHook myScratchpads) <+> fullscreenManageHook
  , terminal           = "alacrittyc"
  , workspaces         = withScreens 1 [ "browse", "code", "read", "chat", "etc"]
  }
