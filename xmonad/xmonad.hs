import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger

import XMonad.Util.ClickableWorkspaces
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import Colors.Nord


main :: IO ()
main = xmonad
    . ewmhFullscreen 
    . ewmh 
    . withEasySB (statusBarProp myBar (clickablePP myPP)) defToggleStrutsKey
    $ myConfig


myConfig = def
    { modMask = myModMask
    , terminal = myTerminal
    , borderWidth = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces = myWorkspaces
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , logHook = myLogHook
    , manageHook = myManageHook
    } `additionalKeysP` myKeys


myModMask :: KeyMask
myModMask = mod4Mask            -- set modkey to super key

myBar :: String
myBar = "xmobar ~/.config/xmobar/xmobarrc"          -- xmobar configuration file

myTerminal :: String
myTerminal = "kitty"            -- set default terminal

myBorderWidth :: Dimension
myBorderWidth = 2               -- set border width for windows

myNormalBorderColor :: String
myNormalBorderColor = "#15181E"   -- set border color of normal windows 

myFocusedBorderColor :: String
myFocusedBorderColor = "#3F485A"  -- set border color of focused windows


-- workspaces -----------------------------------------------------------------
myWorkspaces = 
    [ " 1 "
    , " 2 "
    , " 3 "
    , " 4 "
    ]


-- startup programs -----------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.config/.fehbg &"      -- set last saved feh wallpaper
    spawnOnce "picom"
    setWMName "LG3D"


-- layouts --------------------------------------------------------------------
tall = renamed [Replace "tall"]
    $ smartBorders
    $ Tall 1 (3/100) (1/2)

full = renamed [Replace "full"]
    $ smartBorders
    $ Full

myLayoutHook = avoidStruts $ windowArrange $ spacingWithEdge 4 $ myDefaultLayout
    where
        myDefaultLayout = withBorder myBorderWidth tall
            ||| Mirror tall
            ||| full


-- formatting tools -----------------------------------------------------------
myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.8


-- managing newly created windows ---------------------------------------------
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "gimp" --> doFloat
--    , className =? "discord" --> doShift ( myWorkspaces !! 5 )
    , isDialog --> doFloat
    , isFullscreen --> doFullFloat
    ]


-- xmobar settings ------------------------------------------------------------



myPP :: PP
myPP = def 
    { ppCurrent = xmobarColor "#DEE2ED" "#3F485A" . xmobarBorder "Top" "#3F485A" 2
    , ppHidden = xmobarColor "#DEE2ED" "#2A303C" . xmobarBorder "Top" "#3F485A" 2
    , ppHiddenNoWindows = xmobarColor "#DEE2ED" "#2A303C"
    , ppOrder = \(ws:_:t:_) -> [ws,t]
    , ppSep = "  "
    , ppTitle = xmobarColor color05 "" . shorten 120 }


-- keybindings ----------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 10")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 10")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<Print>", spawn "flameshot gui")
    , ("M-r", spawn "rofi -show drun")
    ]
