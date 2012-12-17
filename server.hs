{-
  Sample server implimentation
  
  spawns a series of connections
  
  {"payload":"hi","stamp":1}
-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.TMChan
import Data.Conduit.Network hiding (getSocket)
import Network (listenOn, PortID(..))
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy (sendAll, recv)
import Data.Attoparsec.ByteString.Lazy
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS hiding (unpack)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Conduit.Attoparsec
import System.Environment

-- import qualified Data.ByteString as BS

data NMessage = NMessage {
  stamp    :: Int,
  payload  :: ByteString
}

instance Show NMessage where
  show a = show (stamp a) ++ ": " ++ (unpack $ payload a)

$(deriveJSON id ''NMessage)

type Message = ByteString

data Server = Server {
  sockets      :: TVar [Socket],
  recvChan     :: TMChan NMessage,
  sendChan     :: TMChan ByteString,
  currentStamp :: TVar Int,
  port         :: Int
}

newServer :: Int -> IO (Server)
newServer port = do
  c1 <- atomically $ newTMChan
  c2 <- atomically $ newTMChan
  n  <- atomically $ newTVar 0
  s  <- atomically $ newTVar []
  return $ Server { sockets = s, sendChan = c1, recvChan = c2, port = port, currentStamp = n }

-- Turns a TVar of [Socket] into a sink
sinkSockets :: MonadIO m => TVar [Socket] -> GInfSink ByteString m
sinkSockets sockets =
    loop
  where
    loop = awaitE >>= either return (\bs -> lift (liftIO $ readTVarIO sockets >>= mapM_ ((flip sendAll) bs)) >> loop)

-- creates a socket for a given hostname and portnumber
getSocket :: ByteString -> Int -> IO (Socket)
getSocket host port = do
  sock <- socket AF_INET Stream defaultProtocol
  addr <- inet_addr $ unpack host
  connect sock $ SockAddrInet (fromIntegral port) addr
  return sock

-- adds a creates and adds a connection to a server
addConn :: Server -> ByteString -> Int -> IO ()
addConn server host port = do
  sock <- getSocket host port
  handshake sock server
  forkIO $ sendConn sock server
  forkIO $ recvConn sock server
  atomically $ modifyTVar (sockets server) (sock:)
  return ()

-- does intitial handshake
handshake :: Socket -> Server -> IO ()
handshake s server = readTVarIO (currentStamp server) >>= sendAll s . pack . show
  
run :: Server -> IO ()
run server = do
    forkIO . runResourceT $ sourceTMChan (recvChan server) $= filterMessages $$ CL.map (encode) =$ sinkSockets (sockets server) -- =$ CL.mapM_ (liftIO . Data.ByteString.Lazy.putStrLn)
    sock <- listenOn $ PortNumber $ fromIntegral $ port server 
    forever $ do
      (s, addr) <- accept sock
      atomically $ do
        modifyTVar (sockets server) (s:)
      -- readTVarIO (sockets server) >>= putStrLn . show
        -- writeTVar (sockets server) (s:ss)
      cur <- recv s 1024
      let num = read $ unpack cur
      atomically $ modifyTVar (currentStamp server) (max num)
      forkIO $ recvConn s server
    return ()
  where
    filterMessages = CL.mapM (filterNothing (currentStamp server)) =$= CL.catMaybes
    filterNothing var x = liftIO $ do
      i <- readTVarIO var
      case stamp x > i of
        True -> do
          atomically $ writeTVar var $ stamp x
          Prelude.putStrLn $ show x
          return $ Just x
        False -> return Nothing 

sendConn :: Socket -> Server -> IO ()
sendConn sock server = forever $ do
  c <- atomically $ dupTMChan $ sendChan server
  forever $ do
    msg <- atomically $ readTMChan c
    case msg of
      Just m -> sendAll sock m
      Nothing -> return ()


recvConn :: Socket -> Server -> IO ()
recvConn sock server = runResourceT $ sourceSocket sock $= conduitParser json =$= CL.mapMaybe frjson $$ sinkTMChan (recvChan server)
  where
    frjson (_, v) = case fromJSON v of
      Error   _ -> Nothing
      Success a -> Just a
      
sendMessage :: Server -> ByteString -> IO ()
sendMessage server msg = do
  i <- atomically $ do
    i <- readTVar (currentStamp server)
    writeTVar (currentStamp server) $ i + 1
    return $ i + 1
  let encoded = encode $ NMessage { payload=msg, stamp=i }
  readTVarIO (sockets server) >>= mapM_ ((flip sendAll) encoded)
-- recvConn sock server = loop ""
--   where
--     loop s = do
--       msg <- recv sock 4096
--       parse json 
--       atomically $ writeTMChan (recvChan server) msg

main = do
  port:_ <- getArgs
  server <- newServer $ read port
  forkIO $ run server
  forever $ do
    line <- getLine
    case line of
      '/':'s':xs -> do
        readTVarIO (sockets server) >>= putStrLn . show
      '/':'a':'d':'d':' ':xs -> do
        let (h, p) = span (/= ' ') xs
        addConn server(pack h) (read p)
      otherwise  -> sendMessage server $ pack line
