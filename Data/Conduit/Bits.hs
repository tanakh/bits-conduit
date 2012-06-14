{-# LANGUAGE BangPatterns #-}

module Data.Conduit.Bits (
  -- * Conduits
  decodeBits,
  encodeBits,
  ) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.Internal

-- | Bitstream decoder
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

-- | Bitstream encoder
encodeBits :: Monad m => Conduit Bool m S.ByteString
encodeBits = go 0 0 where
  go !acc 8 = do
    yield $ S.singleton acc
    go 0 0
  go !acc !i = do
    mb <- await
    case mb of
      Nothing ->
        when (i > 0) $ yield $ S.singleton acc
      Just b -> do
        let n | b = acc `setBit` i
              | otherwise = acc
        go n (i+1)
