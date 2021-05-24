--------------------------------------------------------------------------------
-- | Example.hs
--
-- Example configuration file for xmonad using the latest recommended
-- features (e.g., 'desktopConfig').
module Main (main) where

--------------------------------------------------------------------------------
import System.Exit
import XMonad
import XMonad.Config.Desktop
import Graphics.X11.ExtraTypes.XF86

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops

import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Gaps

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig
import XMonad.Util.Cursor

--------------------------------------------------------------------------------
main :: IO ()
main = xmonad =<< statusBar "xmobar" myXmobarPP toggleStrutsKey myConfig
    where
      toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
      toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myConfig = def
    { modMask    = mod4Mask -- Use the "Win" key for the mod key
    , startupHook = setDefaultCursor xC_left_ptr
    , manageHook = myManageHook <+> manageHook desktopConfig
    , layoutHook = desktopLayoutModifiers $ myLayout
    , logHook    = (dynamicLogString def >>= xmonadPropLog)
                    <+> logHook desktopConfig
    , terminal   = "kitty"
    , normalBorderColor = "#ebdbb2"
    , focusedBorderColor = "#458588"
    }

    `additionalKeysP` -- Add some extra key bindings:
      [ ("<XF86AudioLowerVolume>", spawn "pamixer --decrease 5")
      ,	("<XF86AudioRaiseVolume>", spawn "pamixer --increase 5")
      ,	("<XF86AudioMute>", spawn "pamixer --toggle-mute")
      , ("M-<Escape>", spawn "/home/syoeye/.local/bin/dmenu-power")
      , ("M-s", spawn "scrot")
      , ("M-S-s", spawn "scrot -s")
      , ("M-r", spawn "redshift -O 2400")
      , ("M-S-r", spawn "redshift -x")
      , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +10%")
      , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 10%-")
      ]

--------------------------------------------------------------------------------
-- | Customize layouts.
--
-- This layout configuration uses two primary layouts, 'ResizableTall'
-- and 'BinarySpacePartition'.  You can also use the 'M-<Esc>' key
-- binding defined above to toggle between the current layout and a
-- full screen layout.
myLayout = tiled 
       ||| Mirror tiled 
       ||| Full 
       ||| ThreeCol 1 (3/100) (1/2)
       ||| ThreeColMid 1 (3/100) (1/2)
       ||| Grid
    where
      tiled    = Tall nmaster delta ratio
      nmaster  = 1      -- Default number of windows in the master pane
      ratio    = 1/2    -- Default proportion of screen occupied by master pane
      delta    = 3/100  -- Percent of screen to increment by when resizing panes

--------------------------------------------------------------------------------
-- | Manipulate windows as they are created.  The list given to
-- @composeOne@ is processed from top to bottom.  The first matching
-- rule wins.
--
-- Use the `xprop' tool to get the info you need for these matches.
-- For className, use the second value that xprop gives you.
myManageHook = composeOne
  -- Handle floating windows:
  [ transience            -- move transient windows to their parent
  , isDialog              -?> doCenterFloat
  ] <+> composeAll
  [ className =? "Pidgin" --> doFloat
  , className =? "XCalc"  --> doFloat
  , className =? "mpv"    --> doFloat
  ]

myXmobarPP :: PP
myXmobarPP = def
      { ppSep             = green " | "
      , ppTitle           = wrap (white    "[") (white    "]") . green . ppWindow
      , ppTitleSanitize   = xmobarStrip
      , ppCurrent         = blue . wrap "[" "]"
      , ppHidden          = green . wrap "*" ""
      , ppHiddenNoWindows = lowWhite . wrap " " ""
      , ppUrgent          = red . wrap (orange "!") (orange "!")
      }
    where
      -- | Windows should have *some* title, which should not not exceed a
      -- sane length.
      ppWindow :: String -> String
      ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 20

      blue, lowWhite, green, red, white, yellow, orange :: String -> String
      green    = xmobarColor "#98971a" ""
      blue     = xmobarColor "#458588" ""
      white    = xmobarColor "#ebdbb2" ""
      yellow   = xmobarColor "#d79921" ""
      red      = xmobarColor "#cc241d" ""
      lowWhite = xmobarColor "#a89984" ""
      orange   = xmobarColor "#d65d0e" ""
