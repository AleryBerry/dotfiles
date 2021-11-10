--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import XMonad.Layout.Spacing
import Data.Monoid
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Config.Kde
import XMonad.Layout.Grid
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen( fullscreenEventHook, fullscreenManageHook, fullscreenSupport, fullscreenFull )
import XMonad.Util.SpawnOnce 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import XMonad.Actions.Navigation2D
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Util.Scratchpad
import XMonad.StackSet(RationalRect(RationalRect))
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Fullscreen
import XMonad.Layout.Minimize
import XMonad.Hooks.Minimize
import XMonad.Layout.NoBorders
 
import XMonad.Layout.Minimize
import XMonad.Actions.Minimize
import XMonad.Layout.IM
import Data.Ratio ((%))

import XMonad.Layout.SimpleDecoration
import           Control.Monad   (when)

import XMonad.Hooks.FadeInactive

manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (RationalRect l t w h)
  where
    h = 0.7
    w = 1
    t = 0
    l = 0

myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True 

myClickJustFocuses :: Bool
myClickJustFocuses = True

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["1","2"]

scratchPad = scratchpadSpawnActionTerminal myTerminal

myNormalBorderColor  = "#171322"
myFocusedBorderColor = "#c50df3"

--

coreLayoutHook = modifiers (spacing 5 $ all)
        where tiled = Tall nmaster delta ratio
              tiled3  = ThreeColMid nmaster delta ratio
              nmaster = 1
              delta = 1/100
              ratio = 2/3
              all = Full ||| tiled3 ||| tiled ||| Grid ||| centerMaster Grid ||| spiral (0.857142857143)
              modifiers x = minimize(x)
myLayout = avoidStruts $ coreLayoutHook

customManageHook = composeAll . concat $
  [ [ className   =? c --> doFloat           | c <- myFloats]
  , [ className   =? c --> doF (W.shift "9") | c <- mailIrcApps]
  , [ className   =? "plasmashell" --> doIgnore ]
  ]
  where
    myFloats      = [
       "krunner"
     , "Klipper"
     , "Keepassx"
     ]
    mailIrcApps   = [
       "Thunderbird"
     , "konversation"
     ]

myManageHook = fullscreenManageHook <+> manageDocks <+> manageScratchPad <+> manageHook kdeConfig <+> customManageHook <+> (isFullscreen --> doFullFloat)

myEventHook = handleEventHook kdeConfig <+> docksEventHook <+> fullscreenEventHook <+> minimizeEventHook

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 0.9

myStartupHook = do
  startupHook kdeConfig
  spawnOnce "compton &"
  
main = do
  xmonad $ fullscreenSupport . ewmh $ docks kdeConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        XMonad.workspaces  = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,               xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- close focused winingdow
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    -- Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_m ), sendMessage FirstLayout)

    -- Launch Telegram
    , ((modm .|. shiftMask, xK_t     ), spawn "telegram-desktop")

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), withFocused minimizeWindow  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm              , xK_q     ), kill)

    -- Restart xmonad
    , ((modm .|. shiftMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Swap adjacent windows
    , ((modm,              xK_Right), windowSwap R False)
    , ((modm,              xK_Left ), windowSwap L False)
    , ((modm,              xK_Up   ), windowSwap U False)
    , ((modm,              xK_Down ), windowSwap D False)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
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

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]