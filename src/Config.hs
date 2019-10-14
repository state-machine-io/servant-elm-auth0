{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Config where

import qualified Crypto.JOSE                as Jose
import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString            as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteStringLazyChar8
import qualified Data.Maybe                 as DataMaybe
import qualified Data.Text.Encoding         as TextEncoding
import           Servant.Auth.Server
import qualified System.Exit                as SystemExit

import qualified Types.Config               as Config

getConfig :: FilePath -> IO Config.Config
getConfig cfgFilePath = do
  configBs <- ByteStringLazyChar8.readFile cfgFilePath
  case Aeson.eitherDecode configBs of
    Left e -> do
      putStrLn $ "In file: " <> cfgFilePath <> "\nError: " <> e
      SystemExit.exitWith (SystemExit.ExitFailure 2)
    Right config -> pure (addDefaults config)

addDefaults :: Config.InputConfig -> Config.Config
addDefaults Config.InputConfig {..} = Config.Config
  _inputConfigClientSecret
  _inputConfigClientID
  _inputConfigTenantDomain
  _inputConfigLogoutRoute
  _inputConfigCallbackRoute
  _inputConfigApplicationDomain
  (jwtSettings $ TextEncoding.encodeUtf8 _inputConfigClientSecret)
  (cookieSettings $ TextEncoding.encodeUtf8 $ DataMaybe.fromMaybe
    "session-jwt"
    _inputConfigSessionCookieName
  )


jwtSettings :: ByteString.ByteString -> JWTSettings
jwtSettings clientSecret =
  let k = fromSecret clientSecret
  in  JWTSettings
        { signingKey      = k
        , jwtAlg          = Just Jose.HS256
        , validationKeys  = Jose.JWKSet [k]
        , audienceMatches = const Matches
        }

cookieSettings :: ByteString.ByteString -> CookieSettings
cookieSettings sessionCookieName = CookieSettings
  { cookieIsSecure    = NotSecure
  , cookieMaxAge      = Nothing
  , cookieExpires     = Nothing
  , cookiePath        = Nothing
  , cookieDomain      = Nothing
  , cookieSameSite    = AnySite
  , sessionCookieName = sessionCookieName
  , cookieXsrfSetting = Nothing
  }
