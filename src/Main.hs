{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Network.WebSockets

main :: IO ()
main = runServer "0.0.0.0" 9000 application

application :: Request -> WebSockets Hybi00 ()
application rq = do
  acceptRequest rq
  forever $ do 
    msg <- receive
    send msg

