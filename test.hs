{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Proxy hiding (Server)
import Control.Proxy.Attoparsec
import Control.Proxy.TChan
import Control.Proxy.Network (sourceSocket)
import Control.Proxy.Network.Lazy (sinkSocket)
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
      {-cur <- recv s 1024-}
      {-let num = read $ BC.unpack cur-}
      {-atomically $ modifyTVar (currentStamp server) (max num)-}
      -- kill the connection when the socket dies
      sendThread <- forkIO $ sendConn s server
      forkIO $ finally (recvConn s server) (killThread sendThread) 
    return ()

recvConn :: Socket -> Server -> IO ()
recvConn sock server = do
    runProxy $ sourceSocket sock >-> toMessageD >-> sinkTChan (recvChan server)
    putStrLn "Connection closed"

sendConn :: Socket -> Server -> IO ()
sendConn sock server = do
    chan <- atomically $ dupTChan $ sendChan server
    runProxy $ sourceTChan chan >-> fromMessageD >-> sinkSocket sock

connectToPeer :: Server -> ByteString -> Int -> IO ()
connectToPeer server host port = do
  sock <- getSocket host port
  -- handshake sock server
  forkIO $ sendConn sock server
  forkIO $ recvConn sock server
  return ()

sendMessage :: Server -> Text -> IO ()
sendMessage server msg = do
  i <- atomically $ do
    i <- readTVar (currentStamp server)
    writeTVar (currentStamp server) $ i + 1
    return $ i + 1
  atomically $ writeTChan (sendChan server) Message{payload=msg, stamp=i}

-- creates a socket for a given hostname and portnumber
getSocket :: ByteString -> Int -> IO Socket
getSocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr $ BC.unpack host
  connect sock $ SockAddrInet (fromIntegral port) addr
  return sock

main = do
  port:_ <- getArgs
  server <- newServer $ read port
  forkIO $ run server
  forever $ do
    line <- getLine
    case line of
      '/':'a':'d':'d':' ':xs -> do
        let (h, p) = span (/= ' ') xs
        connectToPeer server (BC.pack h) (read p)
      otherwise  -> sendMessage server $ pack line
