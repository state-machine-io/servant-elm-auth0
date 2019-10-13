{-# LANGUAGE DeriveGeneric   #-}

{-# LANGUAGE OverloadedStrings     #-}
module Types where

import qualified Data.Aeson                    as Aeson
import           GHC.Generics                   ( Generic )
import qualified Servant.Auth.Server           as AuthServer
import qualified Crypto.JOSE                   as Jose
import qualified Crypto.JWT                    as Jose
import qualified Data.HashMap.Strict           as HashMap
import           Control.Lens
import qualified Data.Text                     as Text

data Login = Login { username :: String, password :: String }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON Login
instance Aeson.FromJSON Login

data User = User {
        name :: String
    , email :: String
    }
    deriving (Eq, Show, Read, Generic)

instance Aeson.ToJSON User

instance AuthServer.ToJWT User where
    encodeJWT a = Jose.addClaim "name" (Aeson.toJSON $ name a) . Jose.addClaim "email" (Aeson.toJSON $ email a)$ Jose.emptyClaimsSet

instance Aeson.FromJSON User

instance AuthServer.FromJWT User where
    decodeJWT m = let
            val = (m ^. Jose.unregisteredClaims)
        in
        case (decodeName val, decodeEmail val) of
            (Right a, Right b) ->
                Right $ User a b

            _ ->
             Left "Failed to decode JWT"

decodeClaim
  :: Text.Text
  -> HashMap.HashMap Text.Text Aeson.Value
  -> Either Text.Text String
decodeClaim claim h = case HashMap.lookup claim h of
  Nothing -> Left $ Text.pack $ "Missing " ++ Text.unpack claim ++ " claim"
  Just v  -> case Aeson.fromJSON v of
    Aeson.Error   e -> Left $ Text.pack e
    Aeson.Success a -> Right a

decodeEmail :: HashMap.HashMap Text.Text Aeson.Value -> Either Text.Text String
decodeEmail = decodeClaim "email"

decodeName :: HashMap.HashMap Text.Text Aeson.Value -> Either Text.Text String
decodeName = decodeClaim "name"
