{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

module Routes where

import qualified Data.Proxy          as Proxy
import           Servant
import           Servant.Auth.Server

import qualified Types

type API auths = (Servant.Auth.Server.Auth auths Types.User :> Protected) :<|> Unprotected

type Protected
    = "logout" :> Get '[JSON] NoContent
    :<|>"app" :> Raw

type Unprotected =
    "auth0" :> QueryParam "code" String :> Get  '[JSON] (Headers '[ Header "Set-Cookie" SetCookie
                                                    , Header "Set-Cookie" SetCookie]
                                                  NoContent)
    :<|> "login" :> Get '[JSON] NoContent
    :<|> "home" :> Raw


jwtApi :: Proxy.Proxy (API '[JWT])
jwtApi = Proxy.Proxy

cookieApi :: Proxy.Proxy (API '[Cookie])
cookieApi = Proxy.Proxy
