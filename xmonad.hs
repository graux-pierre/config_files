import XMonad
import XMonad.Config.Azerty
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.CycleWS
import System.IO

main = do
    xmproc <- spawnPipe "xmobar /home/patlepanda/.xmobarrc"

    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
	} 

	  `additionalKeys`
        [ ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")

	-- xterm shortcut
	, ((mod4Mask, xK_t), spawn "urxvt -e /usr/bin/zsh")
	, ((mod4Mask, xK_T), spawn "urxvt -e /usr/bin/zsh")
	
	-- firefox shortcut
	, ((mod4Mask, xK_f), spawn "firefox")
	, ((mod4Mask, xK_F), spawn "firefox")

	-- emacs shortcut
	, ((mod4Mask, xK_d), spawn "emacs")
	, ((mod4Mask, xK_D), spawn "emacs")

	-- se left and right (+shift) to switch beetween WS
        , ((mod4Mask, xK_Right), nextWS)
	, ((mod4Mask, xK_Left), prevWS)
	, ((mod4Mask .|. shiftMask, xK_Right), shiftToNext >> nextWS)
	, ((mod4Mask .|. shiftMask, xK_Left), shiftToPrev >> prevWS)
        

	-- sound keys
   	--
   	-- XF86AudioLowerVolume
   	, ((0            , 0x1008ff11), spawn "amixer -c 0 set Master 2dB-")
   	-- XF86AudioRaiseVolume
   	, ((0            , 0x1008ff13), spawn "amixer -c 0 set Master 2dB+")
   	-- XF86AudioMute
   	, ((0            , 0x1008ff12), spawn "amixer -D pulse set Master toggle")
	]

