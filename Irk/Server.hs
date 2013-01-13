
module Irk.Server
    (
      Server(..),
      newServer,
      run,
      connectToPeer,
      sendMessage
    ) where

{-# LANGUAGE OverloadedStrings #-}

import Control.Proxy hiding (Server)
import Control.Proxy.Attoparsec
import Control.Proxy.TChan

import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (unpack, pack)
import Network (listenOn, PortID(..))
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import Data.Text (Text, unpack, pack)
import Control.Monad (forever)
import Control.Exception

import Network (listenOn, PortID(..))
import Network.BSD
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)

import Control.Proxy hiding (Server)
import qualified Control.Proxy.Network as N
import qualified Control.Proxy.Network.Lazy as NL
import Control.Proxy.TChan

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Irk.Message


filterMessages :: Proxy p => Server -> () -> Pipe p Message Message IO ()
filterMessages server () = runIdentityP $ forever $ do
  msg <- request ()
  res <- lift $ atomically $ do
    i <- readTVar (currentStamp server)
    if stamp msg > i then
      do
        writeTVar (currentStamp server) (i + 1)
        return True
    else
      return False
  if res then respond msg else return ()


data Server = Server {
  recvChan     :: TChan Message,
  sendChan     :: TChan Message,
  currentStamp :: TVar Int,
  port         :: Int
}


newServer :: Int -> IO Server
newServer port = do
  c1 <- atomically newBroadcastTChan
  c2 <- atomically newTChan
  n  <- atomically $ newTVar 0
  return Server { sendChan = c1, recvChan = c2, port = port, currentStamp = n }

run :: Server -> IO ()
run server = do
    forkIO . runProxy $ sourceTChan (recvChan server) >-> filterMessages server >-> printD >-> sinkTChan (sendChan server)
    sock <- listenOn $ PortNumber $ fromIntegral $ port server 
    forever $ do
      (s, addr) <- accept sock
      runConnection server (connectionFromSocket s)
    return ()

connectToPeer :: Server -> ByteString -> Int -> IO ()
connectToPeer server host port = do
  connectTo host port >>= runConnection server
  return ()

sendMessage :: Server -> Text -> IO ()
sendMessage server msg = do
  i <- atomically $ do
    i <- readTVar (currentStamp server)
    writeTVar (currentStamp server) $ i + 1
    return $ i + 1
  atomically $ writeTChan (sendChan server) Message{payload=msg, stamp=i}



-- | Connections

-- | A @Connection@ between two peers
data Connection = Connection {
  rawSocket :: Socket
}

-- | Create a @Connection@ from a @Socket@. The @Socket@ should be ready to
-- read and write
connectionFromSocket :: Socket -> Connection
connectionFromSocket s = Connection { rawSocket = s }

-- Should have a better name
-- | Runs a @Connection@. New threads are created, so this does not block
runConnection :: Server -> Connection -> IO ()
runConnection server conn = do
    runProxy $ N.sourceSocket (rawSocket conn) >-> toMessageD >-> onConnect server >-> fromMessageD >-> NL.sinkSocket (rawSocket conn)
    st <- forkIO (sendConn conn server)
    rt <- forkFinally (recvConn conn server) (\_ -> killThread st)
    return ()

-- creates a socket for a given hostname and portnumber
getSocket :: ByteString -> Int -> IO Socket
getSocket host port = do
    sock <- socket AF_INET Stream defaultProtocol
    addr <- inet_addr $ BC.unpack host
    connect sock $ SockAddrInet (fromIntegral port) addr
    return sock

connectTo :: ByteString -> Int -> IO Connection
connectTo host port = do
    s <- getSocket host port
    return $ connectionFromSocket s

recvConn :: Connection -> Server -> IO ()
recvConn conn server = do
    runProxy $ N.sourceSocket (rawSocket conn) >-> toMessageD >-> sinkTChan (recvChan server)
    putStrLn "Connection closed"

sendConn :: Connection -> Server -> IO ()
sendConn conn server = do
    chan2 <- atomically $ dupTChan $ sendChan server
    runProxy $ sourceTChan chan2 >-> fromMessageD >-> NL.sinkSocket (rawSocket conn)

onConnect :: Proxy p => Server -> () -> Pipe p Message Message IO ()
onConnect server () = runIdentityP $ do
    s <- lift $ atomically $ readTVar $ currentStamp server
    respond $ Message { stamp = s, payload = pack "Hello" }
    m <- request ()
    lift $ atomically $ modifyTVar (currentStamp server) (max (stamp m))
