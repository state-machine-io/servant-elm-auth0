
{-# LANGUAGE TypeOperators   #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib
  ( startApp
  )
where

import           Data.Aeson.TH
import           Network.Wai
import           Control.Concurrent             ( forkIO )
import           Control.Monad                  ( forever )
import           Control.Monad.Trans            ( liftIO )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Network.Wai.Handler.Warp       ( run )
import           System.Environment             ( getArgs )
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan
                                                ( )
import           Data.ByteString.Char8          ( pack )
import qualified Control.Monad.Logger          as MonadLogger

import qualified Routes
import qualified Config
import qualified Types
import qualified Controller
import qualified Types.Config                  as Config


server :: Config.Config -> Server (Routes.API auths)
server config = Controller.protected config :<|> Controller.unprotected config


debug :: Middleware
debug app req resp = do
  putStrLn "Request headers:"
  print (requestHeaders req)
  app req resp

startApp :: Config.Config -> IO ()
startApp config@Config.Config {..} = do
  let context = _configCookieSettings :. _configJWTSettings :. EmptyContext
      api     = Routes.cookieApi
  run 7249 $ debug $ serveWithContext api context (server config)


