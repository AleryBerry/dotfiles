-- Ewmh Screen

-- ManageHook

-- Key Binding

-- Layout

-- Xmonad + PolyBAR

-- Scratchpad

-- Toggle Fullscreen

-- Focus Hooks
import Control.Monad
import DBus.Client qualified as DC
import Data.List
import Data.List.Split
import Data.Map qualified as M
import Data.Ratio
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.NoBorders
import XMonad.DBus qualified as D
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.EqualSpacing
import XMonad.Layout.NoBorders
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare

myBorderWidth = 1

myClickJustFocuses :: Bool
myClickJustFocuses = True

myTerminal = "kitty"

myWorkspaces :: [String]
myWorkspaces = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII"]

-- myWorkspaces         = ["1","2","3","4","5","6","7","8"]

-- Colours
myFocusedBorderColor = "#ffbe3c"

myNormalBorderColor = "#6699cc"

gray = "#6e6e6e"

yellow = "#ffbe3c"

yellow2 = "#6699cc"

otherMonitors = "#d09cea"

myLayoutHook =
  avoidStruts $
    smartBorders $
      equalSpacing
        10
        4
        0
        20
        ( tiled ||| Mirror tiled ||| Full
        )
 where
  tiled = Tall nmaster delta ratio
  nmaster = 1
  ratio = 1 / 2
  delta = 3 / 100

myManageHook :: ManageHook
myManageHook =
  composeAll . concat $
    [ [className =? b --> viewShift "I" | b <- myBrowsers]
    , [className =? c --> viewShift "II" | c <- myComs]
    , [className =? f --> viewShift "III" | f <- ["Alacritty", "kitty"]]
    , [className =? "Deadbeef" --> viewShift "IV"]
    , [className =? d --> viewShift "V" | d <- myGames]
    , [className =? e --> doShift "VI" | e <- myGameLaunchers]
    , [className =? "Github Desktop" --> viewShift "VII"]
    , [className =? "Bitwarden" --> viewShift "VIII"]
    , [isFullscreen --> doF W.focusDown <+> doFullFloat <+> hasBorder False]
    , [isDialog --> doFloat]
    , [appName =? g --> doRectFloat (W.RationalRect (1 % 8) (1 % 8) (3 % 4) (3 % 4)) | g <- myFloats]
    ]
 where
  myBrowsers = ["qutebrowser", "Falkon", "Vivaldi-stable", "firefox"]
  myGames = ["dota2", "clonehero", "Dwarf_Fortress", "Blender"]
  myComs = ["TelegramDesktop", "Element", "discord"]
  myFloats = ["ranger", "lfrun", "Godot_Engine", "gl"]
  myGameLaunchers = ["Steam", "heroic"]
  viewShift = doF . liftM2 (.) W.greedyView W.shift

toggleFull =
  withFocused
    ( \windowId -> do
        floats <- gets (W.floating . windowset)
        if windowId `M.member` floats
          then do
            withFocused toggleBorder
            withFocused $ windows . W.sink
          else do
            withFocused toggleBorder
            withFocused $ windows . flip W.float (W.RationalRect 0 0 1 1)
    )

scratchpads =
  [ NS
      "ncmpcpp"
      "alacritty --title 'ncmpcpp' -e gomp"
      (title =? "ncmpcpp")
      (doRectFloat (W.RationalRect (1 % 8) (1 % 8) (3 % 4) (3 % 4)))
  , NS "neovim" "neovide" (className =? "neovide") doCenterFloat
  , NS
      "monero_node"
      "alacritty --title 'monero_node' -e monerod --data-dir ~/External/Monero/BlockChain --max-concurrency 1 --block-sync-size 10 --limit-rate 500"
      (title =? "monero_node")
      doCenterFloat
  ]

myKeys conf@XConfig{XMonad.modMask = modm} =
  M.fromList $
    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , -- launch rofi drun
      ((modm, xK_p), spawn "rofi -show drun")
    , -- launch rofi -show
      ((modm .|. controlMask, xK_p), spawn "rofi -show")
    , -- launch clipmenu
      ((modm, xK_v), spawn "clipmenu")
    , -- close focused window
      ((modm, xK_q), kill)
    , -- next layout
      ((modm, xK_space), sendMessage NextLayout)
    , -- move focus to the next window
      ((modm, xK_Tab), windows W.focusDown)
    , -- push window back into tiling
      ((modm, xK_t), withFocused $ windows . W.sink)
    , -- increment the number of windows in the master area
      ((modm, xK_comma), sendMessage (IncMasterN 1))
    , -- deincrement the number of windows in the master area
      ((modm, xK_period), sendMessage (IncMasterN (-1)))
    , -- toggle the status bar gap
      ((modm, xK_b), spawn "polybar-msg cmd toggle")
    , -- restart xmonad
      ((modm .|. shiftMask, xK_q), spawn "xmonad --recompile; xmonad --restart")
    , -- logout
      ((modm .|. controlMask, xK_q), spawn "loginctl terminate-user hibiscus-tea")
    , -- toggle fullscreen
      ((modm .|. shiftMask, xK_m), toggleFull)
    , -- swap adjacent windows             | no arrowkeys
      ((modm, xK_l), windowGo R True)
    , ((modm, xK_h), windowGo L True)
    , ((modm, xK_k), windowGo U True)
    , ((modm, xK_j), windowGo D True)
    , -- scratchpads
      ((modm, xK_m), namedScratchpadAction scratchpads "ncmpcpp")
    , ((modm, xK_n), namedScratchpadAction scratchpads "neovim")
    , ((modm, xK_o), namedScratchpadAction scratchpads "monero_node")
    , -- directional navigation of windows | no arrowkeys
      ((modm .|. shiftMask, xK_l), windowSwap R True)
    , ((modm .|. shiftMask, xK_h), windowSwap L True)
    , ((modm .|. shiftMask, xK_k), windowSwap U True)
    , ((modm .|. shiftMask, xK_j), windowSwap D True)
    , -- swap adjacent windows
      ((modm, xK_Right), windowGo R True)
    , ((modm, xK_Left), windowGo L True)
    , ((modm, xK_Up), windowGo U True)
    , ((modm, xK_Down), windowGo D True)
    , -- directional navigation of windows
      ((modm .|. shiftMask, xK_Right), windowSwap R True)
    , ((modm .|. shiftMask, xK_Left), windowSwap L True)
    , ((modm .|. shiftMask, xK_Up), windowSwap U True)
    , ((modm .|. shiftMask, xK_Down), windowSwap D True)
    ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_F1, xK_F2, xK_F3, xK_F4]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
      [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_a, xK_s, xK_d] [0 ..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

myMouseBindings XConfig{XMonad.modMask = modm} =
  M.fromList
    [ -- mod-button1, Set the window to floating mode and resize by dragging

      ( (modm, button1)
      , \w ->
          XMonad.focus w
            >> mouseResizeWindow w
            >> windows W.shiftMaster
      )
    , -- mod-button2, Raise the window to the top of the stack
      ((modm, button2), \w -> XMonad.focus w >> windows W.shiftMaster)
    , -- mod-button3, Set the window to floating mode and move by dragging

      ( (modm, button3)
      , \w ->
          focus w
            >> mouseMoveWindow w
            >> windows W.shiftMaster
      )
    ]

myLogHook :: DC.Client -> ScreenId -> String -> PP
myLogHook dbus i s =
  def
    { ppCurrent = formatUnfocused
    , ppVisible = formatUnfocused
    , ppHidden = formatOther
    , ppHiddenNoWindows = wrap ("%{F" ++ gray ++ "}") "%{F-}"
    , ppOutput = D.sendToPath dbus s
    , ppSep = "  "
    , ppOrder = \(ws : _ : _ : wins : cs) -> fixWorkspaces (head cs) (words ws) <> [wins]
    , ppExtras = [titlesOnScreen, currentOnScreen]
    }
 where
  titlesOnScreen = logDefault (shortenL 70 $ logTitlesOnScreen i formatFocused formatUnfocused) (logConst "Hey, you, you're finally awake.")
  currentOnScreen = wrapL ("%{F" ++ otherMonitors ++ "}") "%{F-}" $ logCurrentOnScreen i
  formatFocused = wrap ("%{F" ++ yellow ++ "}") "%{F-}"
  formatOther = wrap ("%{F" ++ yellow2 ++ "}") "%{F-}"
  formatUnfocused = wrap ("%{F" ++ otherMonitors ++ "}") "%{F-}"
  fixWorkspaces cs = foldr (\x xs -> if x /= cs then x : xs else reformat x : xs) []
  reformat x = wrap ("%{F" ++ yellow ++ "}") "%{F-}" (unwrap x !! 2)
  unwrap = split (dropFinalBlank . dropDelims . oneOf $ "}%")

main :: IO ()
main = do
  -- Connect to DBus
  dbus <- D.connect
  -- Request Access (needed when sending messages)
  D.requestAccess dbus
  -- Start XMonad
  xmonad . docks . ewmhFullscreen . ewmh $
    myConfig
      { logHook =
          (dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ myLogHook dbus 0 "DVI")
            >> (dynamicLogWithPP . filterOutWsPP [scratchpadWorkspaceTag] $ myLogHook dbus 1 "HDMI")
      }

myConfig =
  def
    { terminal = myTerminal
    , clickJustFocuses = myClickJustFocuses
    , modMask = mod4Mask
    , workspaces = myWorkspaces
    , keys = myKeys
    , layoutHook = myLayoutHook
    , manageHook = manageDocks <+> myManageHook <+> namedScratchpadManageHook scratchpads
    , borderWidth = myBorderWidth
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , mouseBindings = myMouseBindings
    }
