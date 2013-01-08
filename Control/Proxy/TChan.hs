module Control.Proxy.TChan
     (
       sinkTChan,
       sourceTChan
     ) where

import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Proxy
import Control.Monad (forever)

sinkTChan :: (Proxy p) => TChan a -> () -> Consumer p a IO ()
sinkTChan chan () = runIdentityP $ forever $ do
    a <- request ()
    lift $ atomically $ writeTChan chan a

sourceTChan :: (Proxy p) => TChan a -> () -> Producer p a IO ()
sourceTChan chan () = runIdentityP $ forever $ do
    a <- lift $ atomically $readTChan chan
    respond a
