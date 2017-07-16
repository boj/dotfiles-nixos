import Data.Monoid ((<>))
import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import XMonad.Layout.Circle
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing

import XMonad.Util.Cursor
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe)    

myStartupHook = setDefaultCursor xC_pirate

myWorkspaces = ["\xf22d", "\xf224", "\xf222", "\xf227", "\xf22b", "\xf223", "\xf22c", "\xf225", "\xf221"]

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

myLayoutHook = noBorders Full ||| Circle ||| mosaic 2 [3,2]

myKeys = 
  [ ((mod1Mask .|. controlMask, xK_j), spawn "amixer -q set Master 5%-")
  , ((mod1Mask .|. controlMask, xK_k), spawn "amixer -q set Master 5%+")
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
    , layoutHook         = avoidStruts $ spacing 10 $ myLayoutHook
    , logHook            = myLogHook xmproc
    }
    `additionalKeys`
    myKeys
