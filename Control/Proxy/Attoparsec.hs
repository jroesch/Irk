module Control.Proxy.Attoparsec
     (
     parserD
     ) where

import Control.Proxy
import Data.Attoparsec
import qualified Data.ByteString as B

parserD :: (Proxy p, Monad m) => Parser b -> () -> Pipe p B.ByteString b m ()
parserD parser () = runIdentityP $ do
  loop (B.empty :: B.ByteString)
  where
    loopP partial = do
      a <- request ()
      case partial a of
        Fail remaining _ _ -> loop remaining
        Partial p          -> loopP p
        Done t result      -> respond result >> loop t
    loop d = do
      if B.null d then request () >>= loop
      else case parse parser d of
             Fail remaining _ _ -> loop remaining
             Partial partial    -> loopP partial
             Done t result      -> respond result >> loop t
