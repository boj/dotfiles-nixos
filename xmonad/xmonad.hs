import Data.Monoid ((<>))
import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)    

myStartupHook = setDefaultCursor xC_pirate

myWorkspaces = ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι"]

myManageHook =
  composeAll [ resource =? "firefox" --> doShift (myWorkspaces !! 1)
             , resource =? "emacs"   --> doShift (myWorkspaces !! 2)
             , resource =? "weechat" --> doShift (myWorkspaces !! 9)
             , manageDocks
             ]

myLogHook xmproc = 
  dynamicLogWithPP $ xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = xmobarColor "#268bd2" "" . shorten 50
    , ppCurrent = xmobarColor "#c0c0c0" "" . wrap "" ""
    , ppSep     = xmobarColor "#dc322f" "" " : "
    , ppUrgent  = xmobarColor "#dc322f" ""
    , ppLayout  = const ""
    }

myLayoutHook = avoidStruts $ layoutHook defaultConfig

myKeys = 
  [ ((mod1Mask .|. controlMask, xK_j), spawn "amixer -q set Master 10%-")
  , ((mod1Mask .|. controlMask, xK_k), spawn "amixer -q set Master 10%+")
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ defaultConfig
    { terminal           = "urxvt"
    , borderWidth        = 1
    , normalBorderColor  = "#7c7c7c"
    , focusedBorderColor = "#ffb6b0"
    , handleEventHook    = docksEventHook <> handleEventHook defaultConfig
    , manageHook         = manageHook defaultConfig <+> myManageHook
    , startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , layoutHook         = myLayoutHook
    , logHook            = myLogHook xmproc
    }
    `additionalKeys`
    myKeys
