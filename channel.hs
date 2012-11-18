-- a simple channel abstraction
{-# LANGUAGE MultiParamTypeClasses #-}
import qualified Network.Socket as S

type Message = String

class (Monad m) => Channel a m where
  send :: a -> Message -> m ()
  recv :: a -> m Message
 
data TestChan = TestChan 
 
instance Channel TestChan IO where
  send t m = putStrLn m
  recv t = return "hi"
  
data SocketChan = SocketChan { socket :: S.Socket }

socketToChan :: S.Socket -> SocketChan
socketToChan s = SocketChan { socket = s }

instance Channel SocketChan IO where
  send t m = do
    S.send (socket t) m
    return ()
  recv t = S.recv (socket t) 1024
  
  
newSocket :: String -> Int -> IO (S.Socket)
newSocket h p = do
  addr <- S.inet_addr h
  s <- S.socket S.AF_INET S.Stream S.defaultProtocol
  S.connect s (S.SockAddrInet (fromIntegral p) addr)
  return s