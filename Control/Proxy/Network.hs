{-# LANGUAGE OverloadedStrings #-}

module Control.Proxy.Network (
                             sourceSocket,
                             sinkSocket
                             ) where

import Control.Proxy
import Data.ByteString (ByteString)
import qualified Data.ByteString  as B
import Network.Socket (Socket)
import Network.Socket.ByteString
import Control.Monad (forever)

sinkSocket :: Proxy p => Socket -> () -> Consumer p ByteString IO ()
sinkSocket s () = runIdentityP $ forever $ do
  d <- request ()
  lift $ sendAll s d

sourceSocket :: Proxy p => Socket -> () -> Producer p ByteString IO ()
sourceSocket s () = runIdentityP loop
  where
    loop = do
      d <- lift $ recv s 1024
      case d of
        "" -> return ()
        otherwise -> do
          respond d
          loop
