{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module Controller
  ( protected
  , unprotected
  )
where

import           Blaze.ByteString.Builder       ( toByteString )
import qualified Servant.Auth.Server           as AuthServer
import           Servant
import           Control.Monad.Trans            ( liftIO )
import qualified Servant.Client                as ServantClient
import           Web.Cookie
import           Data.Tagged
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as TextEncoding
import           Data.Monoid                    ( (<>) )
import           Data.ByteString.Char8
import qualified Data.ByteString.Lazy          as BSL
import           Data.Time.Calendar             ( Day(..) )
import           Data.Time.Clock                ( UTCTime(..)
                                                , secondsToDiffTime
                                                , getCurrentTime
                                                , nominalDay
                                                , addUTCTime
                                                )
import qualified Network.HTTP.Types.Header     as Header

-- Local
import qualified Routes
import qualified Types
import qualified Types.Config                  as Config

import           Auth0.Authentication.GetToken
import           Auth0.Authentication.Logout
import           Auth0
import           Auth0.Types


-- | 'Protected' will be protected by 'auths', which we still have to specify.
protected :: Config.Config -> AuthServer.AuthResult a -> Server Routes.Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected config (AuthServer.Authenticated _) =
  logoutUser config :<|> Servant.serveDirectoryFileServer "dist"
protected config _ = AuthServer.throwAll err401

logoutUser :: Config.Config -> Handler NoContent
logoutUser config@Config.Config {..} = do
  let logoutApi = logout
        (  Just
        $  Text.pack
        $  Text.unpack _configApplicationDomain
        ++ Text.unpack _configLogoutRoute
        )
        (Just _configClientID)
        Nothing
  env           <- liftIO $ mkAuth0Env $ Text.unpack _configTenantDomain
  _             <- liftIO $ ServantClient.runClientM logoutApi env
  cookieHeaders <- liftIO $ expireCookies _configCookieSettings
  throwError err302
    { errHeaders = cookieHeaders
                     ++ [(Header.hLocation, logoutHeaderLocation config)]
    }

logoutHeaderLocation :: Config.Config -> ByteString
logoutHeaderLocation Config.Config {..} = pack
  (  "https://"
  ++ Text.unpack _configTenantDomain
  ++ "/v2/logout?client_id="
  ++ Text.unpack _configClientID
  ++ "&returnTo="
  ++ Text.unpack _configApplicationDomain
  ++ "/"
  ++ Text.unpack _configLogoutRoute
  )

unprotected :: Config.Config -> Servant.Server Routes.Unprotected
unprotected config =
  checkCreds config :<|> redirect config :<|> Servant.serveDirectoryFileServer
    "assets/static"

redirect :: Config.Config -> Handler NoContent
redirect config = throwError err302
  { errHeaders = [("Location", redirectHeaderLocation config)]
  }

redirectHeaderLocation :: Config.Config -> ByteString
redirectHeaderLocation Config.Config {..} = pack
  (  "https://"
  ++ Text.unpack _configTenantDomain
  ++ "/authorize?response_type=code&scope=openid%20profile%20email&client_id="
  ++ Text.unpack _configClientID
  ++ "&redirect_uri="
  ++ Text.unpack _configApplicationDomain
  ++ "/"
  ++ Text.unpack _configCallbackRoute
  )

checkCreds
  :: Config.Config
  -> Maybe String
  -> Handler
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
           NoContent
       )
checkCreds Config.Config {..} (Just code) = do
  let bdy = GetToken
        AuthorizationCode
        (mkClientId _configClientID)
        (mkClientSecret _configClientSecret)
        (Text.pack code)
        (  Just
        $  Text.pack
        $  Text.unpack _configApplicationDomain
        ++ "/"
        ++ Text.unpack _configLogoutRoute
        )
  env <- liftIO $ mkAuth0Env $ Text.unpack _configTenantDomain
  res <- liftIO $ ServantClient.runClientM (getToken bdy) env
  case res of
    Left a -> throwError err302
      { errHeaders = [("Location", TextEncoding.encodeUtf8 _configLogoutRoute)]
      }
    Right tokenResponse -> case idToken tokenResponse of
      Nothing    -> throwError err401
      Just idJWT -> do
        cookieHeaders <- liftIO
          $ acceptAuth0LoginForRedirect _configCookieSettings idJWT
        throwError err302
          { errHeaders = cookieHeaders ++ [(Header.hLocation, pack "app")]
          }
checkCreds _ Nothing = throwError err401

acceptAuth0LoginForRedirect
  :: AuthServer.CookieSettings -> Text.Text -> IO [Header.Header]
acceptAuth0LoginForRedirect cookieSettings idJWT = do
  currentTime <- getCurrentTime
  let expiresTime = addUTCTime nominalDay currentTime
      cookieSettingsWithExpires =
        cookieSettings { AuthServer.cookieExpires = Just expiresTime }
      sessionCookie =
        applySessionCookieSettings cookieSettingsWithExpires
          $ applyCookieSettings cookieSettingsWithExpires
          $ def { setCookieValue = pack $ Text.unpack idJWT }
  xsrfCookie <- liftIO $ AuthServer.makeXsrfCookie cookieSettingsWithExpires
  return
    [ (Header.hSetCookie, toByteString . renderSetCookie $ sessionCookie)
    , (Header.hSetCookie, toByteString . renderSetCookie $ xsrfCookie)
    ]

expireCookies :: AuthServer.CookieSettings -> IO [Header.Header]
expireCookies cookieSettings = do
  let cookieSettingsExpires = cookieSettings
        { AuthServer.cookieExpires = Just expireTime
        , AuthServer.cookieMaxAge  = Just (secondsToDiffTime 0)
        }
      clearedSessionCookie =
        applySessionCookieSettings cookieSettingsExpires
          $ applyCookieSettings cookieSettingsExpires def
      clearedXsrfCookie = case AuthServer.cookieXsrfSetting cookieSettings of
        Just xsrfCookieSettings ->
          applyXsrfCookieSettings xsrfCookieSettings
            $ applyCookieSettings cookieSettingsExpires def
        Nothing -> noXsrfTokenCookie cookieSettingsExpires
  return
    [ (Header.hSetCookie, toByteString . renderSetCookie $ clearedSessionCookie)
    , (Header.hSetCookie, toByteString . renderSetCookie $ clearedXsrfCookie)
    ]


-- For if we just want to login. Currently not used.
acceptAuth0Login
  :: ( AddHeader "Set-Cookie" SetCookie response withOneCookie
     , AddHeader "Set-Cookie" SetCookie withOneCookie withTwoCookies
     )
  => AuthServer.CookieSettings
  -> Text.Text
  -> IO (response -> withTwoCookies)
acceptAuth0Login cookieSettings idJWT = do
  let sessionCookie =
        applySessionCookieSettings cookieSettings
          $ applyCookieSettings cookieSettings
          $ def { setCookieValue = pack $ Text.unpack idJWT }
  xsrfCookie <- AuthServer.makeXsrfCookie cookieSettings
  return $ addHeader sessionCookie . addHeader xsrfCookie


applyCookieSettings :: AuthServer.CookieSettings -> SetCookie -> SetCookie
applyCookieSettings cookieSettings setCookie = setCookie
  { setCookieMaxAge  = AuthServer.cookieMaxAge cookieSettings
  , setCookieExpires = AuthServer.cookieExpires cookieSettings
  , setCookiePath    = AuthServer.cookiePath cookieSettings
  , setCookieDomain  = AuthServer.cookieDomain cookieSettings
  , setCookieSecure  = case AuthServer.cookieIsSecure cookieSettings of
                         AuthServer.Secure    -> True
                         AuthServer.NotSecure -> False
  }

applySessionCookieSettings
  :: AuthServer.CookieSettings -> SetCookie -> SetCookie
applySessionCookieSettings cookieSettings setCookie = setCookie
  { setCookieName     = AuthServer.sessionCookieName cookieSettings
  , setCookieSameSite = case AuthServer.cookieSameSite cookieSettings of
                          AuthServer.AnySite        -> Nothing
                          AuthServer.SameSiteStrict -> Just sameSiteStrict
                          AuthServer.SameSiteLax    -> Just sameSiteLax
  , setCookieHttpOnly = True
  }

applyXsrfCookieSettings
  :: AuthServer.XsrfCookieSettings -> SetCookie -> SetCookie
applyXsrfCookieSettings xsrfCookieSettings setCookie = setCookie
  { setCookieName     = AuthServer.xsrfCookieName xsrfCookieSettings
  , setCookiePath     = AuthServer.xsrfCookiePath xsrfCookieSettings
  , setCookieHttpOnly = False
  }

noXsrfTokenCookie :: AuthServer.CookieSettings -> SetCookie
noXsrfTokenCookie cookieSettings = applyCookieSettings cookieSettings
  $ def { setCookieName = "NO-XSRF-TOKEN", setCookieValue = "" }

-- | Arbitrary cookie expiry time set back in history after unix time 0
expireTime :: UTCTime
expireTime = UTCTime (ModifiedJulianDay 50000) 0
