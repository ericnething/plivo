{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}

module Plivo.Message where

import Control.Applicative
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Servant.API
import Data.Text (Text)

type PhoneNumber = Integer

data Message = Message
  { src  :: PhoneNumber
  , dst  :: PhoneNumber
  , text :: Text
  } deriving (Show, Generic)

instance ToJSON Message

data MessageResponse = MessageResponse
  { _message      :: Text
  , _message_uuid :: [Text]
  , _api_id       :: Text
  } deriving (Show, Generic)

instance FromJSON MessageResponse where
  parseJSON (Object o) =
    MessageResponse
    <$> o .: "message"
    <*> o .: "message_uuid"
    <*> o .: "api_id"

