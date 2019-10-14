
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Lib
  ( startApp
  )
where

import           Network.Wai
import           Network.Wai.Handler.Warp            (run)
import           Servant
import           Servant.Auth.Server.SetCookieOrphan ()

import qualified Controller
import qualified Routes
import qualified Types.Config                        as Config


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


