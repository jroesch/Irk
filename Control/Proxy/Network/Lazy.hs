module Control.Proxy.Network.Lazy (
                             sourceSocket,
                             sinkSocket
                             ) where

import Control.Proxy
import Data.ByteString.Lazy (ByteString)
import Network.Socket (Socket)
import Network.Socket.ByteString.Lazy
import Control.Monad (forever)

sinkSocket :: Proxy p => Socket -> () -> Consumer p ByteString IO ()
sinkSocket s () = runIdentityP $ forever $ do
  d <- request ()
  lift $ sendAll s d

sourceSocket :: Proxy p => Socket -> () -> Producer p ByteString IO ()
sourceSocket s () = runIdentityP $ forever $ do
  d <- lift $ recv s 1024
  respond d
