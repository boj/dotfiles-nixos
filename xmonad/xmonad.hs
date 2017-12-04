import Data.Monoid ((<>))
import System.IO

import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Circle
import XMonad.Layout.Grid
import XMonad.Layout.Mosaic
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane

import qualified XMonad.StackSet as W

import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Run (spawnPipe, safeSpawn)    

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (W.findTag w) $ gets windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

myStartupHook = do
  setDefaultCursor xC_pirate
  spawn "sh .fehbg"

myWorkspaces = fmap show [1..9]

myManageHook =
  composeAll [ resource =? "firefox" --> doShift (myWorkspaces !! 1)
             , resource =? "emacs"   --> doShift (myWorkspaces !! 2)
             , resource =? "weechat" --> doShift (myWorkspaces !! 9)
             , manageDocks
             ]

myLogHook xmproc = 
  dynamicLogWithPP $ xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = xmobarColor "#f2361e" "" . shorten 50
    , ppCurrent = xmobarColor "#df9767" "" . wrap "" ""
    , ppSep     = xmobarColor "#ffffff" "" " : "
    , ppUrgent  = xmobarColor "#dc322f" ""
    , ppLayout  = const ""
    }

myLayoutHook = noBorders Full ||| Grid ||| TwoPane (3/100) (1/2) ||| Circle |||
               mosaic 2 [3,2] ||| ThreeCol 2 (3 / 100) (1 / 3)

myKeys = 
  [ ((mod4Mask .|. controlMask, xK_j), spawn "amixer -q set Master 5%-")
  , ((mod4Mask .|. controlMask, xK_k), spawn "amixer -q set Master 5%+")
  ] ++
  [ ((mod4Mask .|. controlMask, xK_y), spawn "xbacklight -set 10")
  , ((mod4Mask .|. controlMask, xK_u), spawn "xbacklight -dec 10")
  , ((mod4Mask .|. controlMask, xK_i), spawn "xbacklight -inc 10")
  , ((mod4Mask .|. controlMask, xK_o), spawn "xbacklight -set 100")
  ] ++
  [ ((mod4Mask .|. shiftMask, xK_3), spawn "scrot")
  , ((mod4Mask .|. shiftMask, xK_4), spawn "sleep 1;scrot -s")
  ] ++
  [ ((mod4Mask .|. controlMask, xK_p), spawn "systemctl suspend")
  ]

main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad
    $ withUrgencyHook LibNotifyUrgencyHook
    $ defaultConfig
    { terminal           = "urxvt"
    , borderWidth        = 2
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#df9767"
    , handleEventHook    = docksEventHook <> handleEventHook defaultConfig
    , manageHook         = manageHook defaultConfig <+> myManageHook
    , startupHook        = myStartupHook
    , workspaces         = myWorkspaces
    , layoutHook         = avoidStruts $ spacing 10 $ myLayoutHook
    , logHook            = myLogHook xmproc
    , modMask            = mod4Mask
    }
    `additionalKeys`
    myKeys
