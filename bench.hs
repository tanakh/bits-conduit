import Control.Applicative
import Control.Monad
import Criterion.Main
import qualified Data.ByteString as S
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Word
import System.Random

import Data.Conduit.Bits

encode :: Bool -> Int -> IO ()
encode b n =
  replicateM_ n (yield b) =$= encodeBits $$ CL.sinkNull

encodes :: [Bool] -> IO ()
encodes bs =
  forM_ bs yield =$= encodeBits $$ CL.sinkNull

decode :: Word8 -> Int -> IO Int
decode b8 n =
  yield (S.replicate n b8) =$= decodeBits $$ CL.fold (\a _ -> a + 1) 0

decodes :: S.ByteString  -> IO Int
decodes bs =
  yield bs =$= decodeBits $$ CL.fold (\a _ -> a + 1) 0

main :: IO ()
main = do
  let size = 100000
      bsize = size `div` 8
  rb  <- replicateM size randomIO
  rbs <- S.pack <$> replicateM bsize randomIO

  defaultMain
    [ bgroup ("encode/" ++ show size)
      [ bench "0s" $ whnfIO $ encode False size
      , bench "1s" $ whnfIO $ encode True  size
      , bench "Rs" $ whnfIO $ encodes rb
      ]

    , bgroup ("decode/" ++ show size)
      [ bench "0s" $ whnfIO $ decode 0x00 bsize
      , bench "1s" $ whnfIO $ decode 0xFF bsize
      , bench "Rs" $ whnfIO $ decodes rbs
      ]
    ]
