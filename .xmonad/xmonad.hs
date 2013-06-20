-- in ~/.xmonad
{-# LANGUAGE OverloadedStrings #-}

import XMonad
import XMonad.Config.Gnome
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Util.EZConfig
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import Data.List

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ gnomeConfig
         { logHook = dynamicLogWithPP (prettyPrinter dbus) }
         {  workspaces = ["1:ff","2:mail","3:vim","4:console","5:server","6:notes","7:dropbox","8:misc","9:minimized"]
         , manageHook = myManageHook <+> manageHook gnomeConfig
         , modMask = mod4Mask
         , layoutHook = myLayouts
         , terminal = "gnome-terminal"
         , focusFollowsMouse = False
         }
         `additionalKeysP` myKeys
         
         
myKeys = [ ("M-S-p", spawn "scrot window_%Y-%m-%d-%H-%M-%S.png -s") ]
          ++
         [ ("M-S-q", spawn "gnome-session-quit --power-off") ]

prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = pangoSanitize
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red" , ppLayout   = const "" , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs

myManageHook = composeAll
   [ className =? "Firefox-bin" --> doShift "1:ff" 
   , className =? "Firefox" --> doShift "1:ff"
   , className =? "Chromium-browser" --> doShift "1:chrome"
   , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
   , className =? "Thunderbird" --> doShift "2:mail"
   , className =? "Do" --> doFloat ]

myLayouts = smartBorders $ avoidStruts ( tiled ||| Mirror tiled ||| Full) ||| noBorders (fullscreenFull Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
