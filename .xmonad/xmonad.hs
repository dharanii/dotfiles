{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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
import XMonad.Util.Loggers (logCmd)
import XMonad.Util.Run (spawnPipe, hPutStrLn, runProcessWithInput)


data Wal = Wal
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

getWal :: FilePath -> IO (Maybe Wal)
getWal path = do
    exists <- doesFileExist path
    if not exists then 
       return $ Nothing
    else do
        colors <- traverse getWalColor [0..15]
        return $ Just $ 
            Wal (colors !! 0)
                (colors !! 7)
                (colors !! 7)
                (colors !! 0)
                (colors !! 1)
                (colors !! 2)
                (colors !! 3)
                (colors !! 4)
                (colors !! 5)
                (colors !! 6)
                (colors !! 7)
                (colors !! 8)
                (colors !! 9)
                (colors !! 10)
                (colors !! 11)
                (colors !! 12)
                (colors !! 13)
                (colors !! 14)
                (colors !! 15)
    where
        getWalColor n = 
            trim <$> runProcessWithInput "/bin/sed" [ "-n", show (n +1) ++ "p", path ] []


barFont = "-*-lemon-*"
barIcon = "-*-siji-*"
barHeight = 36

lemonbar :: LayoutClass l Window
         => Maybe Wal -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
lemonbar wal conf = do
    h <- spawnPipe $
            "lemonbar"
                ++ " -d"
                ++ " -g x" ++ show barHeight 
                ++ " -B \"" ++ maybe "#000000" color0  wal ++ "\"" 
                ++ " -F \"" ++ maybe "#F0F0F0" color15 wal ++ "\"" 
                ++ " -n \"bar\""
                ++ " -f \"" ++ barFont ++ "\""
                ++ " -f \"" ++ barIcon ++ "\""
                ++ " | /bin/bash"
    return $ docks $ conf
        { layoutHook = avoidStruts (layoutHook conf)
        , logHook = do
            logHook conf 
            dynamicLogWithPP (lemonbarPP wal) { ppOutput = hPutStrLn h }
        }

lemonbarPP :: Maybe Wal -> PP
lemonbarPP wal = def
    { ppCurrent         = pad
    , ppHidden          = wrap ("%{F" ++ maybe "#F0F0F0" color3 wal ++ "}") "%{F}" . pad
    , ppHiddenNoWindows = wrap ("%{F" ++ maybe "#F0F0F0" color8 wal ++ "}") "%{F}" . pad
    , ppSep   = mempty 
    , ppWsSep = " "
    , ppOrder = \(workspaces:layout:tile:[date,wifi,volume,inputMethod]) -> 
        let
            icon xs =
                "%{F" ++ maybe "#F0F0F0" color3 wal ++ "}" ++ xs ++ "%{F}"
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
            
            ws = wrap " " " " $ workspaces
            tl = wrap " " " " $ unwords [ tileIcon, tile ]
            dt = wrap " " " " $ date
            ld = wrap " " " " $ unwords [ layoutIcon, layout ]
            wf = if null wifi then mempty else 
                 wrap " " " " $ unwords [ wifiIcon (read wifi), wifi ++ "%" ]
            vl = wrap " " " " $ unwords [ volumeIcon (read volume), volume ++ "%" ]
            im = wrap " " " " $ unwords [ inputMethodIcon, inputMethod ]
        in
            [ mconcat [ "%{l}  ", ws, " | ", tl ]
            , mconcat [ "%{c}", dt ]
            , mconcat [ "%{r}", ld, " | ", wf, vl, im, "   " ]
            ]
    , ppExtras = 
        let
            date =
                logCmd "date +\"%a %d %b - %l:%M %p\""
            wifi = maybe (pure mempty) pure <$>
                logCmd "nmcli device wifi | sed -n '/^*/p' | sed -n 2p | awk '{ print $7 }' | sed -e 's/[^0-9]//g'"
            volume = 
                logCmd "amixer sget Master | grep -o -m 1 -E \"[[:digit:]]+%\" | sed -e 's/[^0-9]//g'"
            inputMethod =
                logCmd "echo \"EN\""
        in
            [ date, wifi, volume, inputMethod ]
    }


keys_ :: XConfig l -> M.Map (KeyMask, KeySym) (X ())
keys_ conf@(XConfig {modMask = modm}) =
    M.fromList
        [ ((modm .|. shiftMask, xK_p), spawn "rof")
        , ((modm .|. shiftMask, xK_s), spawn "scr")
        
        --, ((noModMask, stringToKeysym "<XF86AudioLowerVolume>"), spawn "vol down")
        --, ((noModMask, stringToKeysym "<XF86AudioRaiseVolume>"), spawn "vol up")
        ]


main :: IO ()
main = do
    home <- getHomeDirectory
    wal  <- getWal $ home </> ".cache/wal/colors"

    conf <- lemonbar wal $ def
        { terminal           = "urxvtc"
        , normalBorderColor  = maybe def background wal
        , focusedBorderColor = maybe def color1     wal
        , keys               = \conf -> M.union (keys_ conf) (keys def conf)
        , layoutHook         = spacingWithEdge 9 $ layoutHook def
        , startupHook        = setDefaultCursor xC_left_ptr <+> startupHook def
        }

    xmonad conf
