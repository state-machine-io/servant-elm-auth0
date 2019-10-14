{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Options.Generic as OptionsGeneric

import qualified Config
import qualified Lib

main :: IO ()
main = OptionsGeneric.getRecord "servant-auth0" >>= doIt

doIt :: FilePath -> IO ()
doIt configFilePath = do
  config <- Config.getConfig configFilePath
  Lib.startApp config
