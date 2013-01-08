{-# LANGUAGE OverloadedStrings #-}

module Irk.Message
    (
      Message (..),
      toMessage,
      fromMessage,
      toMessageD,
      fromMessageD
    ) where

import Data.Aeson
import Data.Text
import Data.Attoparsec
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Proxy
import Control.Proxy.Attoparsec
import Control.Applicative

-- | Defines the basic message type used to communicate with peers
data Message = Message {
  stamp    :: Int,
  payload  :: Text
} deriving (Show, Eq)

instance FromJSON Message where
    parseJSON (Object v) = Message        <$>
                           v .: "stamp"   <*>
                           v .: "payload"

instance ToJSON Message where
    toJSON (Message stamp payload) = object ["stamp" .= stamp, "payload" .= payload]

-- | Convert values traveling downstream from @Value@s to @Message@s
fromJSOND :: (Monad m, Proxy p) => () -> Pipe p Value Message m ()
fromJSOND () = runIdentityP loop where
  loop = do
    a <- request ()
    case fromJSON a of
      Error _   -> loop
      Success r -> respond r >> loop

-- | Converts @ByteString@s traveling downstream to @Message@s
toMessageD :: (Monad m, Proxy p) => () -> Pipe p B.ByteString Message m ()
toMessageD = parserD json >-> fromJSOND

-- | Converts @Message@s taveling downstream to @ByteString@s
fromMessageD :: (Monad m, Proxy p) => () -> Pipe p Message BL.ByteString m ()
fromMessageD = mapD fromMessage

-- | Strict parsing of a @ByteString@ into a @Message@
toMessage :: B.ByteString -> Maybe Message
toMessage bs = case parse json' bs of
                 Done _ v -> case fromJSON v of
                               Success a -> Just a
                               _         -> Nothing
                 _        -> Nothing

-- | Strict conversion of @Message@ to @ByteString@
fromMessage :: Message -> BL.ByteString
fromMessage = encode
