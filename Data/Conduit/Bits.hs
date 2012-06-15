{-# LANGUAGE BangPatterns #-}

module Data.Conduit.Bits (
  -- * Conduits
  decodeBits,
  encodeBits,

  -- * Functions
  yieldBits,
  awaitBits,
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
        yieldBits b8 8
        go

-- | Bitstream encoder
encodeBits :: Monad m => Conduit Bool m S.ByteString
encodeBits = go where
  go = do
    mb <- awaitBits 8
    case mb of
      Nothing -> return ()
      Just b8 -> do
        yield $ S.singleton b8
        go

-- | Yields specified amount of bits (LSB first)
yieldBits :: (Bits b, Monad m) => b -> Int -> Pipe i Bool m ()
yieldBits b n =
  forM_ [0..n-1] $ \i -> yield $ testBit b i
{-# INLINEABLE yieldBits #-}

-- | await specified amount of bits (LSB first)
awaitBits :: (Bits b, Monad m) => Int -> Pipe Bool o m (Maybe b)
awaitBits n = go 0 0 where
  go !acc !i
    | i == n = return $ Just acc
    | otherwise = do
      mb <- await
      case mb of
        Nothing
          | i == 0 -> return Nothing
          | otherwise -> return $ Just acc
        Just b ->
          go (if b then acc `setBit` i else acc) (i+1)
{-# INLINEABLE awaitBits #-}
