-- A simple test of sending a message to a group of servers
import System.IO
import Network.Socket
import Data.Conduit.Network
import Control.Concurrent.STM

-- add a connection
addConn :: HostAddress-> PortNumber-> TVar [Socket] -> IO (TVar [Socket])
addConn h p conns = do
  sock <- initConn h p
  case sock of
    Just s -> atomically $ modifyTVar conns (\x -> s:x)
    Nothing -> return ()
  return conns

-- Creates a socket, connects it, and handshakes
-- Needs to handle failure
initConn :: HostAddress-> PortNumber-> IO (Maybe Socket)
initConn h p = do
  s <- socket AF_INET Stream defaultProtocol
  connect s (SockAddrInet p h)
  suc <- handshake s
  case suc of
    True -> return $ Just s
    False -> return Nothing
  
-- should probably send user id
handshake :: Socket -> IO Bool
handshake s = do
  send s "hi"
  resp <- recv s 1024
  return $ resp == "hi"
  
-- need to make this work, or use a slightly different abstraction
sinkSockets :: MonadIO m => TVar [Socket] -> GInfSink ByteString m
sinkSockets conns = undefined