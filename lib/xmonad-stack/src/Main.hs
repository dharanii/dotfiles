{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((^?))
import Control.Monad (void)
import Data.Aeson.Lens (key, _String)
import Data.Text as T
import System.Directory (getHomeDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, PP(..), wrap, pad, shorten)
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks)
import XMonad.Layout.Gaps (gaps, Direction2D(..))
import XMonad.Layout.Spacing (spacingWithEdge)
import XMonad.Util.Cursor (setDefaultCursor, xC_left_ptr)
import XMonad.Util.EntryHelper (withCustomHelper, Config(..), withLock, compileUsingShell)
import qualified XMonad.Util.EntryHelper as EH
import XMonad.Util.Loggers (date, logCmd)
import XMonad.Util.Run (spawnPipe, hPutStrLn)


barFont = "-*-lemon-*"
barIcon = "-*-siji-*"
barHeight = 36

logHook_ h color3 color8 = dynamicLogWithPP $ def
    { ppCurrent         = pad
    , ppHidden          = wrap ("%{F" ++ maybe "#F0F0F0" T.unpack color3 ++ "}") "%{F}" . pad
    , ppHiddenNoWindows = wrap ("%{F" ++ maybe "#F0F0F0" T.unpack color8 ++ "}") "%{F}" . pad
    , ppSep = mempty 
    , ppWsSep = " "
    , ppLayout = \name ->
        maybe name T.unpack $ T.stripPrefix "SpacingWithEdge 9 " $ T.pack name 
    , ppOrder = \(workspaces:layout:tile:[date,volume,inputMethod]) -> 
        let
            icon xs =
                "%{F" ++ maybe "#F0F0F0" T.unpack color3 ++ "}" ++ xs ++ "%{F}"
            tileIcon = icon "\xE0B9"
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
            , "%{r}" ++ layout 
                ++ "  |  "
                ++ inputMethod
                ++ "  |  "
                ++ volumeIcon (read volume) ++ " " ++ volume ++ "%"
                ++ "    "
            ]
    , ppExtras = 
        let
            volume = 
                logCmd "amixer sget Master | grep -o -m 1 -E \"[[:digit:]]+%\" | sed -e 's/[^0-9]//g'"
            inputMethod =
                logCmd "echo \"EN\""
        in
            [ date "%a %d %b - %l:%M %p", volume, inputMethod ]
    , ppOutput = hPutStrLn h
    }

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    colorsJson <- readFile $ homeDir </> ".cache/wal/colors.json"
    let color0  = colorsJson ^? key "colors" . key "color0" . _String
        color1  = colorsJson ^? key "colors" . key "color1" . _String
        color3  = colorsJson ^? key "colors" . key "color3" . _String
        color8  = colorsJson ^? key "colors" . key "color8" . _String
        color15 = colorsJson ^? key "colors" . key "color15" . _String

    lemonbar <- spawnPipe $
        "lemonbar"
            ++ " -d"
            ++ " -g x" ++ show barHeight 
            ++ " -B \"" ++ maybe "#000000" T.unpack color0  ++ "\"" 
            ++ " -F \"" ++ maybe "#F0F0F0" T.unpack color15 ++ "\"" 
            ++ " -n \"bar\""
            ++ " -f \"" ++ barFont ++ "\""
            ++ " -f \"" ++ barIcon ++ "\""
            ++ " | bash"

    withCustomHelper $ EH.defaultConfig
        { run = xmonad $ def
            { layoutHook = gaps [(U, barHeight)] $ spacingWithEdge 9 $ layoutHook def
            , terminal = "urxvtc"
            , normalBorderColor = maybe def T.unpack color0
            , focusedBorderColor = maybe def T.unpack color1
            , startupHook = setDefaultCursor xC_left_ptr <+> startupHook def
            , logHook = logHook_ lemonbar color3 color8
            --, manageHook = manageDocks <+> manageHook def
            }
        , compile = compile_ 
        , postCompile = postCompile_
        }

compile_ :: Bool -> IO ExitCode
postCompile_ :: ExitCode -> IO ()

compile_ force = 
    withLock ExitSuccess $ compileUsingShell $
        "cd \"${HOME}/dotfiles/lib/xmonad-stack\"; " ++
            if force then
                "stack clean; stack build"
            else
                "stack build"

postCompile_ ExitSuccess = void $ compileUsingShell "cd \"${HOME}/dotfiles/lib/xmonad-stack\"; stack install"
postCompile_ exitCode    = EH.defaultPostCompile exitCode
