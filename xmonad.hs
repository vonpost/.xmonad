import XMonad

import XMonad.Actions.Navigation2D
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Spacing
import XMonad.Actions.SpawnOn
import MyKeyBindings
import XMonad.Hooks.EwmhDesktops 
import XMonad.Layout.Tabbed
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Scratchpad
import XMonad.Layout.Hidden
import XMonad.Layout.Spacing

main :: IO ()
main =
  xmonad
    $ withNavigation2DConfig def { defaultTiledNavigation = hybridOf lineNavigation centerNavigation }
    $ myConfig

myConfig = ewmh def
  { borderWidth        = 0
  , startupHook = startupHook def <+> setWMName "LG3D"
  , handleEventHook = handleEventHook def <+> fullscreenEventHook
  , focusFollowsMouse  = False
  , keys               = myKeys
  , layoutHook         = spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
                         $ hiddenWindows emptyBSP 
  , modMask            = mod4Mask
  , manageHook         = manageSpawn <+> manageHook def <+> scratchpadManageHookDefault <+> fullscreenManageHook  
  , terminal           = "urxvtc"
  , workspaces         = [ "browse", "code", "read", "chat", "etc"]
  }
