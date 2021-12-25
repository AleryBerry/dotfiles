import XMonad


-- XMONAD + POLYBAR
import XMonad.Hooks.ManageDocks
import qualified XMonad.DBus as D
import qualified DBus.Client as DC
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

-- ManageHook
import XMonad.Hooks.ManageHelpers
-- Ewmh Screen
import XMonad.Hooks.EwmhDesktops
-- Additional Keys
import XMonad.Util.EZConfig(additionalKeysP)
-- Layouts
import XMonad.Layout.NoBorders

myTerminal           = "alacritty" 
myClickJustFocuses :: Bool
myClickJustFocuses   = True
myBorderWidth        = 4
myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#1414ff"

myWorkspaces       :: [String]
myWorkspaces         = ["I","II","III", "IV","V","VI","VII","VII","IX"]

myLayoutHook = avoidStruts (
    tiled ||| Mirror tiled ||| Full
    )
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100


myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? c --> doShift "I" | c <- myBrowsers   ]
    , [ className =? b --> doShift "V" | b <- myGames      ]
    , [ className =? "telegram-desktop" --> doShift "II"   ]
    , [ isFullscreen   --> doFullFloat <+> hasBorder False ]
    , [ isDialog       --> doFloat                         ]
    ]
  where
      myBrowsers = [ "qutebrowser", "falkon", "vivaldi-bin" ]
      myGames    = [ "dota2", "clonehero", "Dwarf_Fortress" ]

myLogHook :: DC.Client -> PP
myLogHook dbus = def {
    ppOutput = D.send dbus
,   ppHiddenNoWindows = wrap "<" ">"
}

main :: IO ()
main = do
     -- Connect to DBus
     dbus <- D.connect
     -- Request Access (needed when sending messages)
     D.requestAccess dbus
     -- Start XMonad
     xmonad . ewmhFullscreen . ewmh $ myConfig { logHook = myLogHook dbus }    

myConfig = def {
  terminal = myTerminal
, clickJustFocuses   = myClickJustFocuses
, modMask            = mod4Mask
, workspaces         = myWorkspaces
, layoutHook         = smartBorders $ myLayoutHook
, manageHook         = manageDocks <+> myManageHook 
, borderWidth        = myBorderWidth
, normalBorderColor  = myNormalBorderColor
, focusedBorderColor = myFocusedBorderColor
}

