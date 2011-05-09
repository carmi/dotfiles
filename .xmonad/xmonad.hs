import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO

-- Go to window without having to find it.
import XMonad.Actions.WindowGo
 
myManageHook = composeAll
    [ className =? "Gimp"               --> doFloat
    , className =? "Vncviewer"          --> doFloat
    , resource  =? "chromium-browser"   --> doF (W.shift "<1:web>")
    , resource  =? "Mail"               --> doF (W.shift "<2:mail>")
    ]

myWorkspaces = ["<1:web>","<2:mail>","<3:chat>","<4:tmux>","<5:dev>","<6:media>","<7:dropbox>","<8:misc>"]
 
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/evan/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> myManageHook -- make sure to include myManageHook definition from above
                        <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , workspaces = myWorkspaces
        , logHook = dynamicLogWithPP $ xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        -- Configure colors and preferences
        , borderWidth           = 3
        , terminal              = "urxvt"
        , normalBorderColor     = "#cccccc"
        , focusedBorderColor    = "#cd8b00"
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        -- keymappings for WindowGo
        , ((mod4Mask, xK_b), raise (resource =? "chromium-browser"))
        , ((mod4Mask, xK_v), raise (resource =? "chromium-browser"))
        , ((mod4Mask, xK_v), raise (resource =? "tmux"))
        ]
