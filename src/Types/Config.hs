{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Types.Config where

import qualified Data.Aeson                    as Aeson
import qualified Data.Text                     as Text
import qualified Data.ByteString               as BS

import           Servant.Auth.Server
import           Options.Generic                ( Generic
                                                , ParseRecord
                                                )

data InputConfig = InputConfig
    {
        _inputConfigClientSecret :: Text.Text
        , _inputConfigClientID :: Text.Text
        , _inputConfigTenantDomain :: Text.Text
        , _inputConfigSessionCookieName :: Maybe Text.Text
        , _inputConfigLogoutRoute :: Text.Text
        , _inputConfigCallbackRoute :: Text.Text
        , _inputConfigApplicationDomain :: Text.Text
    } deriving (Show, Generic)

instance Aeson.FromJSON InputConfig where
    parseJSON = Aeson.withObject "Config" $ \o -> InputConfig
        <$> o Aeson..: "client_secret"
        <*> o Aeson..: "client_id"
        <*> o Aeson..: "tenant_domain"
        <*> o Aeson..:?  "session_cookie_name"
        <*> o Aeson..: "logout_route"
        <*> o Aeson..: "callback_route"
        <*> o Aeson..: "application_domain"

data Config = Config
    {
        _configClientSecret :: Text.Text
        , _configClientID :: Text.Text
        , _configTenantDomain :: Text.Text
        , _configLogoutRoute :: Text.Text
        , _configCallbackRoute :: Text.Text
        , _configApplicationDomain :: Text.Text
        , _configJWTSettings :: JWTSettings
        , _configCookieSettings :: CookieSettings
    }
