{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

import Control.Monad.Identity
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Word
import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Conduit.Bits

main :: IO ()
main = hspec $ do
  describe "decoder" $ do
    it "decodes empty sequence" $ do
      a <- yield S.empty =$= decodeBits $$ CL.consume
      a `shouldBe` []

    prop "decodes bits" $ \bs ->
      let a = runIdentity $ yield (S.pack bs) =$= decodeBits $$ CL.consume
      in decodeNaive bs == a

  describe "encoder" $ do
    it "encodes empty sequence" $ do
      a <- CL.sourceNull =$= encodeBits $$ CB.take maxBound
      a `shouldBe` L.empty

    prop "encodes bits" $ \bs ->
      let a = runIdentity $ CL.sourceList bs =$= encodeBits $$ CB.take maxBound
      in L.pack (encodeNaive bs) == a

decodeNaive :: [Word8] -> [Bool]
decodeNaive = concatMap f where
  f c = map (\i -> testBit c i) [0..7 :: Int]

encodeNaive :: [Bool] -> [Word8]
encodeNaive [] = []
encodeNaive (splitAt 8 -> (cur, next)) =
  let c = foldl (.|.) 0 (zipWith (\i b -> if b then bit i else 0) [0..] cur)
  in c : encodeNaive next
