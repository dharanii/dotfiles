{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (guard)
import qualified Data.Map as M
import qualified Data.Text as T
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, PP(..), wrap, pad, trim, shorten)
import XMonad.Hooks.ManageDocks (AvoidStruts, avoidStruts, docks)
import XMonad.Layout.LayoutModifier (ModifiedLayout(..))
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Util.Loggers (Logger,logCmd)
import XMonad.Util.Run (spawnPipe, hPutStrLn)


data ColorScheme = ColorScheme
    { background :: String
    , foreground :: String
    , cursor     :: String
    , color0     :: String
    , color1     :: String
    , color2     :: String
    , color3     :: String
    , color4     :: String
    , color5     :: String
    , color6     :: String
    , color7     :: String
    , color8     :: String
    , color9     :: String
    , color10    :: String
    , color11    :: String
    , color12    :: String
    , color13    :: String
    , color14    :: String
    , color15    :: String
    } deriving (Show, Eq)

getColorScheme :: FilePath -> IO (Maybe ColorScheme)
getColorScheme path = do
    exists <- doesFileExist path
    if not exists then 
        return $ Nothing
    else do
        !colors <- lines <$> readFile path
        return $ ColorScheme 
            <$> colors !? 0
            <*> colors !? 7
            <*> colors !? 7
            <*> colors !? 0
            <*> colors !? 1
            <*> colors !? 2
            <*> colors !? 3
            <*> colors !? 4
            <*> colors !? 5
            <*> colors !? 6
            <*> colors !? 7
            <*> colors !? 8
            <*> colors !? 9
            <*> colors !? 10
            <*> colors !? 11
            <*> colors !? 12
            <*> colors !? 13
            <*> colors !? 14
            <*> colors !? 15

(!?) :: [a] -> Int -> Maybe a
xs !? n = xs !! n <$ guard (n < length xs)


barFont = "-*-lemon-*"
barIcon = "-*-siji-*"
barHeight = 36

lemonbar :: LayoutClass l Window
         => Maybe ColorScheme -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
lemonbar colorscheme conf = do
    h <- spawnPipe $ "lemonbar " ++ unwords
            [ "-d"
            , "-g", "x" ++ show barHeight 
            , "-B", wrap "\"" "\"" (maybe "#000000" color0  colorscheme)
            , "-F", wrap "\"" "\"" (maybe "#F0F0F0" color15 colorscheme)
            , "-n", wrap "\"" "\"" "bar"
            , "-f", wrap "\"" "\"" barFont
            , "-f", wrap "\"" "\"" barIcon
            ]
    return $ docks $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
            logHook conf 
            dynamicLogWithPP (lemonbarPP colorscheme) { ppOutput = hPutStrLn h }
        }

lemonbarPP :: Maybe ColorScheme -> PP
lemonbarPP colorscheme = def
    { ppCurrent         = pad
    , ppHidden          = wrap ("%{F" ++ maybe "#F0F0F0" color3 colorscheme ++ "}") "%{F}" . pad
    , ppHiddenNoWindows = wrap ("%{F" ++ maybe "#F0F0F0" color8 colorscheme ++ "}") "%{F}" . pad
    , ppSep   = mempty 
    , ppWsSep = " "
    , ppOrder = \(workspaces:layout:tile:[date,wifi,volume,inputMethod]) -> 
        let
            icon = wrap ("%{F" ++ maybe "#F0F0F0" color3 colorscheme ++ "}") "%{F}"
            tileIcon   = icon "\xE0B9"
            layoutIcon = icon "\xE005"
            wifiIcon n
                | n < 10    = icon "\xE218"
                | n < 40    = icon "\xE218"
                | n < 70    = icon "\xE219"
                | otherwise = icon "\xE21A"
            volumeIcon n
                | n < 10    = icon "\xE051"
                | n < 40    = icon "\xE053"
                | n < 70    = icon "\xE053"
                | otherwise = icon "\xE152"
            inputMethodIcon = icon "\xE26F" 
            
            ws = pad $ workspaces
            tl = pad $ unwords [ tileIcon, tile ]
            dt = pad $ date
            ld = pad $ unwords [ layoutIcon, layout ]
            wf = if null wifi then mempty else 
                 pad $ unwords [ wifiIcon (read wifi), wifi ++ "%" ]
            vl = pad $ unwords [ volumeIcon (read volume), volume ++ "%" ]
            im = if null inputMethod then mempty else
                 pad $ unwords [ inputMethodIcon, inputMethod ]
        in
            [ mconcat [ "%{l}  ", ws, " | ", tl ]
            , mconcat [ "%{c}", dt ]
            , mconcat [ "%{r}", ld, " | ", wf, vl, im, "   " ]
            ]
    , ppExtras = [ date, wifi, volume, inputMethod ]
    }

date, wifi, volume, inputMethod :: Logger
date = logCmd "date +\"%a %d %b - %l:%M %p\""
wifi = go <$> logCmd "nmcli device wifi | grep '*' | awk 'NR==2{ print $7 }' | sed -e 's/[^0-9]//g'" where
    go = maybe (pure mempty) pure
volume = logCmd "amixer sget Master | grep -o -m 1 -E \"[[:digit:]]+%\" | sed -e 's/[^0-9]//g'"
inputMethod = go <$> logCmd "fcitx-remote" where
    go (Just "1") = Just "US"
    go (Just "2") = Just "JP"
    go _          = Just mempty


customKeys :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
customKeys conf@(XConfig {modMask = modm}) =
    M.fromList
        [ ((modm .|. shiftMask, xK_p), spawn "rof")
        , ((modm .|. shiftMask, xK_s), spawn "scr")
        
        --, ((noModMask, stringToKeysym "<XF86AudioLowerVolume>"), spawn "vol down")
        --, ((noModMask, stringToKeysym "<XF86AudioRaiseVolume>"), spawn "vol up")
        , ((controlMask, xK_space), spawn "fcitx-remote -t" >> windows id)
        --, ((noModMask, stringToKeysym "<XF86PowerOff>"), spawn "systemctl poweroff")
        --, ((noModMask, stringToKeysym "<XF86Sleep>"   ), spawn "loc && systemctl suspend")

        , ((modm .|. shiftMask, xK_slash), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
        ]


main :: IO ()
main = do
    home        <- getHomeDirectory
    colorscheme <- getColorScheme $ home </> ".cache/wal/colors"

    conf <- lemonbar colorscheme $ def
        { terminal           = "urxvtc"
        , normalBorderColor  = maybe def background colorscheme
        , focusedBorderColor = maybe def color1     colorscheme
        , keys               = \conf -> M.union (customKeys conf) (keys def conf)
        , layoutHook         = spacingWithEdge 9 $ layoutHook def
        , startupHook        = setDefaultCursor xC_left_ptr <+> startupHook def
        }

    xmonad conf


help :: String
help = unlines [
    "                                                       ",
    "                                                  '||  ",
    " ... ... .. .. ..     ...   .. ...    ....      .. ||  ",
    "  '|..'   || || ||  .|  '|.  ||  ||  '' .||   .'  '||  ",
    "   .|.    || || ||  ||   ||  ||  ||  .|' ||   |.   ||  ",
    " .|  ||. .|| || ||.  '|..|' .||. ||. '|..'|'  '|..'||. ",
    "                                                       ",
    "                                                       ",
    "",
    "The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch urxvtc",
    "mod-Shift-p      Launch rofi",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
