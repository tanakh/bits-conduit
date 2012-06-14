{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Identity
import Data.Bits
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Word
import Test.Hspec.HUnit ()
import Test.Hspec.QuickCheck
import Test.Hspec.Monadic
import Test.HUnit

import Data.Conduit.Bits

main :: IO ()
main = hspecX $ do
  describe "encoder" $ do
    it "encodes empty sequence" $ do
      a <- yield S.empty =$= decodeBits $$ CL.consume
      [] @=? a

    prop "encodes bits" $ \bs ->
      let a = runIdentity $ yield (S.pack bs) =$= decodeBits $$ CL.consume
      in decodeNaive bs == a

decodeNaive :: [Word8] -> [Bool]
decodeNaive = concatMap f where
  f c = map (\i -> testBit c i) [0..7 :: Int]
