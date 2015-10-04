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

import Control.Monad.Trans.Either
import Servant.API
import Servant.Client
import Data.Proxy
import Plivo.Message
import qualified Data.Text as T
import Data.Text (Text)
import Control.Arrow (first)
import Data.Monoid ((<>))

newtype AuthId
  = AuthId {unAuthId :: Text}
  deriving (ToText, FromText)

newtype AuthToken
  = AuthToken {unAuthToken :: Text}
  deriving (Show)

instance FromText AuthToken where
  fromText str = case first T.toLower $ T.break (== ' ') str of
    ("basic", token) -> Just $ AuthToken token
    _                -> Nothing

instance ToText AuthToken where
  toText = ("Basic " <>) . unAuthToken

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
message token authId msg = runEitherT $ message' (Just token) authId msg

