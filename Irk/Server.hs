
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

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent
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

import Irk.Message
import Irk.Connection


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
      runConnection (sendChan server) (recvChan server) (connectionFromSocket s)
    return ()

connectToPeer :: Server -> ByteString -> Int -> IO ()
connectToPeer server host port = do
  connectTo host port >>= runConnection (sendChan server) (recvChan server)
  return ()

sendMessage :: Server -> Text -> IO ()
sendMessage server msg = do
  i <- atomically $ do
    i <- readTVar (currentStamp server)
    writeTVar (currentStamp server) $ i + 1
    return $ i + 1
  atomically $ writeTChan (sendChan server) Message{payload=msg, stamp=i}
