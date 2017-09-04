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
import XMonad.Util.EntryHelper (withCustomHelper, Config(..), withLock, compileUsingShell)
import qualified XMonad.Util.EntryHelper as EH


main :: IO ()
main = do
    homeDir <- getHomeDirectory
    colorsJson <- readFile $ homeDir </> ".cache/wal/colors.json"
    let color0 = colorsJson ^? key "colors" . key "color0" . _String
        color1 = colorsJson ^? key "colors" . key "color1" . _String

    withCustomHelper $ EH.defaultConfig
        { run = xmonad $ def
                    { terminal = "urxvtc"
                    , normalBorderColor = maybe def T.unpack color0
                    , focusedBorderColor = maybe def T.unpack color1
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
