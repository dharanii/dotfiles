{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((^?))
import Control.Monad (void)
import Data.Aeson.Lens (key, _String)
import Data.Text as T
import System.Exit (ExitCode(..))
import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.EntryHelper (withCustomHelper, Config(..), compileUsingShell)
import qualified XMonad.Util.EntryHelper as EH



main :: IO ()
main = do
    colorsJson <- readFile "/home/vagrant/.cache/wal/colors.json"
    let color1 = colorsJson ^? key "colors" . key "color1" . _String
        color2 = colorsJson ^? key "colors" . key "color2" . _String

    withCustomHelper $ EH.Config
        { run = xmonad $ def
                    { terminal = "urxvtc"
                    , normalBorderColor = maybe def T.unpack color1
                    , focusedBorderColor = maybe def T.unpack color2
                    }
        , compile = compile_ 
        , postCompile = postCompile_
        }

compile_ :: Bool -> IO ExitCode
postCompile_ :: ExitCode -> IO ()

compile_ force = 
    compileUsingShell $
        if force then
            "stack clean; stack build"
        else
            "stack build"

postCompile_ ExitSuccess = void $ compileUsingShell "stack install"
postCompile_ exitCode    = EH.defaultPostCompile exitCode
