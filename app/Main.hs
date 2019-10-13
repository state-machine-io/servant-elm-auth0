{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import qualified Options.Generic               as OptionsGeneric


import qualified Types.Config                  as Config
import           Config

main :: IO ()
main = OptionsGeneric.getRecord "servant-auth0" >>= doIt

doIt :: FilePath -> IO ()
doIt configFilePath = do
  config <- Config.getConfig configFilePath
  startApp config
