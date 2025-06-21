{-# LANGUAGE TupleSections #-}

import Data.Function ((&))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Utils.Misc (holes)
import XMonad
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Prelude (elemIndex)
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import XMonad.Util.SpawnOnce (spawnOnce)

type KeyCombination = (KeyMask, KeySym)

myStartup :: X ()
myStartup = do
  spawnOnce "feh --bg-fill ~/Pictures/girl-reading-book.png"
  spawnOnce "picom"

createKeyChord :: [KeyCombination] -> X () -> [(KeyCombination, X ())]
createKeyChord [] f = []
createKeyChord x f = (\(i, xs) -> (i, submap (maps xs))) <$> holes x
 where
  maps :: [KeyCombination] -> Map KeyCombination (X ())
  maps xs = M.fromList $ (,f) <$> xs

myKeyChords :: [(KeyCombination, X ())]
myKeyChords =
  concat
    [ createKeyChord [(mod4Mask, xK_a), (mod4Mask, xK_s)] (spawn "ghostty")
    , createKeyChord [(mod4Mask, xK_a), (mod4Mask, xK_s)] (spawn "alacritty")
    , createKeyChord [(mod4Mask, xK_a), (mod4Mask, xK_s)] (spawn "alacritty")
    ]

main :: IO ()
main =
  xmonad . ewmh $
    def
      { terminal = "ghostty"
      , modMask = mod4Mask
      , startupHook = myStartup
      }
      & flip
        additionalKeys
        ( [ ((noModMask, xK_Print), spawn "flameshot screen -r | xclip -selection clipboard -t image/png")
          , ((mod4Mask, xK_Print), spawn "flameshot gui -r -s | xclip -selection clipboard -t image/png")
          ]
            ++ myKeyChords
        )
        . flip
          additionalKeysP
          [ ("M-S-z", spawn "xscreensaver-command -lock")
          , ("M-C-s", unGrab *> spawn "scrot -s")
          , ("M-f", spawn "flameshot screen -r | xclip -selection clipboard -t image/png")
          , ("M-c M-a", spawn "firefox")
          ]
