{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Plivo
       ( -- * Endpoints
         message
         -- * Types
       , AuthId (..)
       , AuthToken (..)
       , Message (..)
       , MessageResponse (..)
       ) where

import           Control.Monad.Trans.Either
import           Servant.API
import           Servant.Client
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Control.Arrow (first)
import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64

import           Plivo.Message

newtype AuthId
  = AuthId {unAuthId :: ByteString}
  deriving (Show)

instance ToText AuthId where
  toText = decodeUtf8 . unAuthId

newtype AuthToken
  = AuthToken {unAuthToken :: ByteString}
  deriving (Show)

instance ToText AuthToken where
  toText = ("Basic " <>) . decodeUtf8 . unAuthToken

type PlivoAPI =
     Header "Authorization" AuthToken
  :> "v1"
  :> "Account"
  :> Capture "auth_id" AuthId
  :> "Message"
  :> ReqBody '[JSON] Message
  :> Post '[JSON] MessageResponse

api :: Proxy PlivoAPI
api = Proxy

message' :: Maybe AuthToken
         -> AuthId
         -> Message
         -> EitherT ServantError IO MessageResponse

message' = client api (BaseUrl Https "api.plivo.com" 443)

message :: AuthToken
        -> AuthId
        -> Message
        -> IO (Either ServantError MessageResponse)
message token authId msg = runEitherT $ message' (Just $ creds authId token) authId msg

creds :: AuthId -> AuthToken -> AuthToken
creds (AuthId authId) (AuthToken token) = AuthToken . B64.encode $ authId <> ":" <> token

