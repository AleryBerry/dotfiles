{-# LANGUAGE TupleSections #-}

import Data.Map (Map)
import qualified Data.Map as M
import Data.Ratio ((%))
import qualified Data.Set as S
import Data.Time (defaultTimeLocale, getCurrentTime)
import Data.Time.Format (formatTime)
import GHC.Utils.Misc (holes)
import XMonad
import XMonad.Actions.Submap (submap)
import XMonad.Actions.ToggleFullFloat (toggleFullFloat, toggleFullFloatEwmhFullscreen)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, setEwmhActivateHook)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks, manageDocks)
import XMonad.Hooks.ManageHelpers (composeOne, doCenterFloat, doFullFloat, doRectFloat, isFullscreen, (-?>))
import XMonad.Hooks.UrgencyHook (doAskUrgent)
import XMonad.Layout
import XMonad.Layout.Gaps (Direction2D (D, L, R, U), gaps)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (Ambiguity (OnlyScreenFloat), ConfigurableBorder, WithBorder, lessBorders, noBorders)
import XMonad.Layout.Spacing (Border (Border), Spacing, spacingRaw, spacingWithEdge)
import XMonad.Prelude (elemIndex, isJust, void)
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor (setDefaultCursor)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.SpawnOnce (spawnOnce)

type KeyCombination = (KeyMask, KeySym)

myStartup :: X ()
myStartup = do
  spawnOnce "feh --bg-fill ~/Pictures/girl-reading-book.png"
  spawnOnce "picom"
  spawnOnce "polybar"
  setDefaultCursor xC_left_ptr

vimAnywhere :: X ()
vimAnywhere = do
  time <- liftIO getCurrentTime
  let tempFile = "/tmp/vim-anywhere/" ++ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" time
  spawnOnce $ "neovide " ++ tempFile ++ " --x11-wm-class vimAnywhere && cat " ++ tempFile ++ " | xclip -selection clipboard"

createKeyChord :: [KeyCombination] -> X () -> [(KeyCombination, X ())]
createKeyChord [] f = []
createKeyChord x f = (\(i, xs) -> (i, submap (maps xs))) <$> holes x
 where
  maps :: [KeyCombination] -> Map KeyCombination (X ())
  maps xs = M.fromList $ (,f) <$> xs

myKeyChords :: [(KeyCombination, X ())]
myKeyChords =
  concat
    [ createKeyChord [(mod4Mask, xK_w), (mod4Mask, xK_u)] (spawn "ayugram-desktop")
    , createKeyChord [(mod4Mask, xK_w), (mod4Mask, xK_r)] (spawn "ferdium")
    , createKeyChord [(mod4Mask, xK_o), (mod4Mask, xK_p)] (spawn "ghostty")
    ]

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "rmpc" "kitty --title 'rmpc' -e rmpc" (title =? "rmpc") doCenterFloat
  , NS "neovim" "neovide --x11-wm-class code-editor" (className =? "code-editor") doFullFloat
  ]

myLayoutHook :: Choose (ModifiedLayout (ConfigurableBorder Ambiguity) (ModifiedLayout AvoidStruts (ModifiedLayout Spacing XMonad.Layout.Tall))) (ModifiedLayout WithBorder Full) Window
myLayoutHook = mainLayout (Tall 1 (3 / 100) (1 / 2)) ||| noBorders Full
 where
  mainLayout = noFullBorders . avoidStruts . spacingWithEdge 10
  noFullBorders = lessBorders OnlyScreenFloat

toggleFloat :: Window -> X ()
toggleFloat w = ifM (isFloat w) (windows $ W.sink w) (float w)
 where
  isFloat :: Window -> X Bool
  isFloat w = gets $ isJust . M.lookup w . W.floating . windowset

forceKill :: Window -> X ()
forceKill w = withDisplay $ \d -> do io $ void (killClient d w)

myKeys conf@XConfig{XMonad.modMask = modm, XMonad.terminal = term} =
  M.fromList $
    myKeyChords
      ++ [ ((modm, xK_Return), spawn term)
         , ((modm, xK_p), spawn "dmenu_run")
         , ((modm, xK_b), spawn "polybar-msg cmd toggle")
         , ((modm, xK_q), withFocused killWindow)
         , ((modm .|. shiftMask, xK_q), withFocused forceKill)
         , ((modm, xK_Tab), windows W.focusDown)
         , ((modm, xK_t), withFocused toggleFloat)
         , ((modm .|. shiftMask, xK_m), withFocused toggleFullFloat)
         , ((modm, xK_space), sendMessage NextLayout)
         , ((modm, xK_v), vimAnywhere)
         , -- scratchpads
           ((modm, xK_m), namedScratchpadAction scratchpads "rmpc")
         , ((modm, xK_n), namedScratchpadAction scratchpads "neovim")
         , ((modm, xK_Print), spawn "flameshot screen -r | xclip -selection clipboard -t image/png")
         , ((modm, xK_Print), spawn "flameshot gui -r -s | xclip -selection clipboard -t image/png")
         ]
      ++
      -- mod-[1..9], Switch to workspace N
      -- mod-shift-[1..9], Move client to workspace N
      [ ((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1, xK_2, xK_3, xK_4, xK_F1, xK_F2, xK_F3, xK_F4]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

myManageHook :: ManageHook
myManageHook =
  composeOne
    [ isFullscreen -?> doFullFloat
    , className =? "vimAnywhere" -?> doRectFloat (W.RationalRect (1 % 8) (1 % 6) (3 % 4) (2 % 3))
    ]

main :: IO ()
main = do
  spawn $ "echo \"" ++ show (fst <$> myKeyChords) ++ "\" >> /home/aleryberry/mytext.txt"
  xmonad . docks . toggleFullFloatEwmhFullscreen . setEwmhActivateHook doAskUrgent . ewmhFullscreen . ewmh $
    def
      { terminal = "ghostty"
      , manageHook = manageDocks <+> namedScratchpadManageHook scratchpads <+> myManageHook
      , modMask = mod4Mask
      , startupHook = myStartup
      , layoutHook = myLayoutHook
      , focusedBorderColor = "#FABD2F"
      , keys = myKeys
      }
