-- A simple test of sending a message to a group of servers
{-# LANGUAGE OverloadedStrings #-}
import System.IO
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM

-- add a connection
addConn :: HostAddress -> PortNumber -> TVar [Socket] -> IO (TVar [Socket])
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
  sendAll s "hi"
  resp <- recv s 1024
  return $ resp == "hi"

sendToAll :: TVar [Socket] -> B.ByteString -> IO ()
sendToAll conns msg = do
  conns' <- readTVarIO conns
  mapM_ ((flip sendAll) msg) conns'
  