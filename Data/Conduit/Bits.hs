module Data.Conduit.Bits (
  decodeBits,
  encodeBits,
  ) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Internal
import qualified Data.Conduit.List as CL

decodeBits :: Monad m => Conduit S.ByteString m Bool
decodeBits = go where
  go = do
    mb <- sinkToPipe CB.head
    case mb of
      Nothing ->
        return ()
      Just b8 -> do
        forM_ [0..7] $ \i -> yield $ testBit b8 i
        go

encodeBits :: Monad m => Conduit Bool m S.ByteString
encodeBits = undefined
