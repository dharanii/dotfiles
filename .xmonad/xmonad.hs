{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Text as T
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Process (readProcess)
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, PP(..), wrap, pad, shorten)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Layout.Gaps (gaps, Direction2D(..))
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Util.Loggers (logCmd)
import XMonad.Util.Run (spawnPipe, hPutStrLn, runProcessWithInput)


walColors :: FilePath
walColors = ".cache/wal/colors"

getWalColor :: Int -> FilePath -> IO (Maybe String)
getWalColor n home =
    runProcessWithInput "/bin/sed" [ "-n", show (n +1) ++ "p", home </> walColors ] []
        >>= \output ->
            case T.unpack $ T.strip $ T.pack output of
                [] ->
                    return $ Nothing
                hex ->
                    return $ Just hex


barFont = "-*-lemon-*"
barIcon = "-*-siji-*"
barHeight = 36

logHook_ h color3 color8 = dynamicLogWithPP $ def
    { ppCurrent         = pad
    , ppHidden          = wrap ("%{F" ++ maybe "#F0F0F0" id color3 ++ "}") "%{F}" . pad
    , ppHiddenNoWindows = wrap ("%{F" ++ maybe "#F0F0F0" id color8 ++ "}") "%{F}" . pad
    , ppSep = mempty 
    , ppWsSep = " "
    , ppLayout = \name ->
        maybe name T.unpack $ T.stripPrefix "SpacingWithEdge 9 " $ T.pack name 
    , ppOrder = \(workspaces:layout:tile:[date,volume,inputMethod]) -> 
        let
            icon xs =
                "%{F" ++ maybe "#F0F0F0" id color3 ++ "}" ++ xs ++ "%{F}"
            tileIcon = icon "\xE0B9"
            layoutIcon = icon "\xE005"
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
                ++ inputMethodIcon ++ " " ++ inputMethod
                ++ "  " -- ++ "  |  "
                ++ volumeIcon (read volume) ++ " " ++ volume ++ "%"
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
    homeDir <- getHomeDirectory
    color0  <- getWalColor  0 homeDir
    color1  <- getWalColor  1 homeDir
    color3  <- getWalColor  3 homeDir
    color8  <- getWalColor  8 homeDir
    color15 <- getWalColor 15 homeDir

    lemonbar <- spawnPipe $
        "lemonbar"
            ++ " -d"
            ++ " -g x" ++ show barHeight 
            ++ " -B \"" ++ maybe "#000000" id color0  ++ "\"" 
            ++ " -F \"" ++ maybe "#F0F0F0" id color15 ++ "\"" 
            ++ " -n \"bar\""
            ++ " -f \"" ++ barFont ++ "\""
            ++ " -f \"" ++ barIcon ++ "\""
            ++ " | /bin/bash"

    xmonad $ def
        { layoutHook = gaps [(U, barHeight)] $ spacingWithEdge 9 $ layoutHook def
        , terminal = "urxvtc"
        , normalBorderColor = maybe def id color0
        , focusedBorderColor = maybe def id color1
        , startupHook = setDefaultCursor xC_left_ptr <+> startupHook def
        , logHook = logHook_ lemonbar color3 color8
        --, manageHook = manageDocks <+> manageHook def
        }
