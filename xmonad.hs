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
-- Key Binding
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Actions.Navigation2D
-- Layouts
import XMonad.Layout.NoBorders

myTerminal           = "alacritty" 
myClickJustFocuses :: Bool
myClickJustFocuses   = True
myBorderWidth        = 4
myNormalBorderColor  = "#ffffff"
myFocusedBorderColor = "#1414ff"
myWorkspaces       :: [String]
myWorkspaces         = ["I","II","III", "IV","V","VI","VII","VIII","IX"]

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

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modm,               xK_p     ), spawn "rofi -show drun")
    -- launch clipmenu
    , ((modm .|. shiftMask ,xK_p     ), spawn "clipmenu")
    -- close focused window
    , ((modm,               xK_q     ), kill)
    -- next layout
    , ((modm,               xK_space ), sendMessage NextLayout)
    -- move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- increment the number of windows in the master area
    , ((modm,               xK_comma ), sendMessage (IncMasterN 1))
    -- deincrement the number of windows in the master area
    , ((modm,               xK_period), sendMessage (IncMasterN (-1)))
    -- toggle the status bar gap
    , ((modm,               xK_b     ), sendMessage ToggleStruts)
    -- restart xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    -- Swap adjacent windows | no arrowkeys
    , ((modm,               xK_l     ), windowSwap R True)
    , ((modm,               xK_h     ), windowSwap L True)
    , ((modm,               xK_k     ), windowSwap U True)
    , ((modm,               xK_j     ), windowSwap D True)
    -- Directional navigation of windows | no arrowkeys   
    , ((modm .|. shiftMask, xK_l     ), windowGo R True)
    , ((modm .|. shiftMask, xK_h     ), windowGo L True)
    , ((modm .|. shiftMask, xK_k     ), windowGo U True)
    , ((modm .|. shiftMask, xK_j     ), windowGo D True)
    -- Swap adjacent windows
    , ((modm,               xK_Right), windowSwap R True)
    , ((modm,               xK_Left  ), windowSwap L True)
    , ((modm,               xK_Up    ), windowSwap U True)
    , ((modm,               xK_Down  ), windowSwap D True)
    -- Directional navigation of windows
    , ((modm .|. shiftMask, xK_Right ), windowGo R True)
    , ((modm .|. shiftMask, xK_Left  ), windowGo L True)
    , ((modm .|. shiftMask, xK_Up    ), windowGo U True)
    , ((modm .|. shiftMask, xK_Down  ), windowGo D True)
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button3), \w -> 
        focus w >>
        mouseMoveWindow w >>
        windows W.shiftMaster)
    , ((modm .|. shiftMask, button1), (\w -> XMonad.focus w >> mouseMoveWindow w
        >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> XMonad.focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button1), (\w -> XMonad.focus w >> mouseResizeWindow w
        >> windows W.shiftMaster))
    ] 

main :: IO ()
main = do
     -- Connect to DBus
     dbus <- D.connect
     -- Request Access (needed when sending messages)
     D.requestAccess dbus
     -- Start XMonad
     xmonad . ewmhFullscreen . ewmh $ myConfig { logHook = dynamicLogWithPP (myLogHook dbus) }
myConfig = def {
  terminal = myTerminal
, clickJustFocuses   = myClickJustFocuses
, modMask            = mod4Mask
, workspaces         = myWorkspaces
, keys               = myKeys
-- Hooks
, layoutHook         = smartBorders $ myLayoutHook
, manageHook         = manageDocks <+> myManageHook 
, borderWidth        = myBorderWidth
, normalBorderColor  = myNormalBorderColor
, focusedBorderColor = myFocusedBorderColor
}

