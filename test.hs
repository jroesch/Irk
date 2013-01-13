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
import Irk.Server


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
