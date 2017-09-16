{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import System.Directory (getHomeDirectory, doesFileExist)
import System.FilePath ((</>))
import XMonad
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, PP(..), wrap, pad, trim, shorten)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Gaps (gaps, Direction2D(..))
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


cacheWalColors :: FilePath
cacheWalColors = ".cache/wal/colors"

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

lemonbar wal =
    "lemonbar"
        ++ " -d"
        ++ " -g x" ++ show barHeight 
        ++ " -B \"" ++ maybe "#000000" color0  wal ++ "\"" 
        ++ " -F \"" ++ maybe "#F0F0F0" color15 wal ++ "\"" 
        ++ " -n \"bar\""
        ++ " -f \"" ++ barFont ++ "\""
        ++ " -f \"" ++ barIcon ++ "\""
        ++ " | /bin/bash"

logHook_ wal h = dynamicLogWithPP $ def
    { ppCurrent         = pad
    , ppHidden          = wrap ("%{F" ++ maybe "#F0F0F0" color3 wal ++ "}") "%{F}" . pad
    , ppHiddenNoWindows = wrap ("%{F" ++ maybe "#F0F0F0" color8 wal ++ "}") "%{F}" . pad
    , ppSep = mempty 
    , ppWsSep = " "
    , ppLayout = \name ->
        maybe name T.unpack $ T.stripPrefix "SpacingWithEdge 9 " $ T.pack name 
    , ppOrder = \(workspaces:layout:tile:[date,volume,inputMethod]) -> 
        let
            icon xs =
                "%{F" ++ maybe "#F0F0F0" color3 wal ++ "}" ++ xs ++ "%{F}"
            tileIcon        = icon "\xE0B9"
            layoutIcon      = icon "\xE005"
            inputMethodIcon = icon "\xE26F" 
            volumeIcon n
              | n < 10    = icon "\xE051"
              | n < 40    = icon "\xE053"
              | n < 70    = icon "\xE053"
              | otherwise = icon "\xE152"
        in
            [ "%{l}   " ++ workspaces 
                ++ "  |  " 
                ++ tileIcon ++ " " ++ tile
            , "%{c}" ++ date
            , "%{r}" 
                ++ layoutIcon ++ " " ++ layout 
                ++ "  |  "
                ++ volumeIcon (read volume) ++ " " ++ volume ++ "%"
                ++ "  "
                ++ inputMethodIcon ++ " " ++ inputMethod
                ++ "    "
            ]
    , ppExtras = 
        let
            date =
                logCmd "date +\"%a %d %b - %l:%M %p\""
            volume = 
                logCmd "amixer sget Master | grep -o -m 1 -E \"[[:digit:]]+%\" | sed -e 's/[^0-9]//g'"
            inputMethod =
                logCmd "echo \"EN\""
        in
            [ date, volume, inputMethod ]
    , ppOutput = hPutStrLn h
    }

main :: IO ()
main = do
    home <- getHomeDirectory
    wal <- getWal $ home </> cacheWalColors

    lemonbarHandle <- spawnPipe $ lemonbar wal

    xmonad $ def
        { terminal           = "urxvtc"
        , normalBorderColor  = maybe def background wal
        , focusedBorderColor = maybe def color1     wal
        , layoutHook         = gaps [(U, barHeight)] $ spacingWithEdge 9 $ layoutHook def
        , logHook            = logHook_ wal lemonbarHandle
        , startupHook        = setDefaultCursor xC_left_ptr <+> startupHook def
        }
