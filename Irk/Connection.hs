module Irk.Connection
    (
      Connection (..),
      runConnection,
      connectTo,
      connectionFromSocket
    ) where


import Network (listenOn, PortID(..))
import Network.BSD
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)

import Control.Proxy hiding (Server)
import Control.Proxy.Network (sourceSocket)
import Control.Proxy.Network.Lazy (sinkSocket)
import Control.Proxy.TChan

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Irk.Message

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
runConnection :: TChan Message -> TChan Message -> Connection -> IO ()
runConnection sendChan recvChan conn = do
    let sock = rawSocket conn
    st <- forkIO (sendConn sock sendChan)
    rt <- forkFinally (recvConn sock recvChan) (\_ -> killThread st)
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

recvConn :: Socket -> TChan Message -> IO ()
recvConn sock chan = do
    runProxy $ sourceSocket sock >-> toMessageD >-> sinkTChan chan
    putStrLn "Connection closed"

sendConn :: Socket -> TChan Message -> IO ()
sendConn sock chan = do
    chan2 <- atomically $ dupTChan chan
    runProxy $ sourceTChan chan2 >-> fromMessageD >-> sinkSocket sock
