module Main where

import qualified Data.ByteString.Char8 as B
import News (getNews)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, (!))
import Text.Printf (printf)
import GibbsOptBucket (prog)
import Language.Hakaru.Runtime.LogFloatPrelude (unMeasure)
import Control.Monad (forever, replicateM, forM_)
import Data.List (sort)
import Data.Number.LogFloat

onesFrom :: Vector Int -> Vector LogFloat
onesFrom v = V.replicate (V.maximum v + 1) 1

-- |Make it easier to draw a sample
sample g m = do
  maybeX <- unMeasure m g
  case maybeX of
    Nothing -> fail "Sample rejected"
    Just x  -> return x

accuracy
    :: V.Vector Int
    -> V.Vector Int
    -> Double
accuracy x y = V.sum z / (fromIntegral $ V.length x)
    where z = V.zipWith (\a b -> if a == b then 1 else 0) x y

main = do
  (words, docs, topics) <- getNews Nothing [0..]
  g <- MWC.createSystemRandom
  let 
    zPrior = onesFrom topics
    wPrior = onesFrom words
    predict = prog zPrior wPrior topics words docs
  printf "length zPrior == %d\n" (V.length zPrior)
  printf "length wPrior == %d\n" (V.length wPrior)
  printf "length words  == %d\n" (V.length words)
  printf "length docs   == %d\n" (V.length docs)
  printf "length topics == %d\n" (V.length topics)
  --forM_ [0..(V.length topics - 1)] $ \i -> do
  forM_ [0..10] $ \i -> do
    --print $ V.map logFromLogFloat $ predict i
    prediction <- sample g (predict i)
    printf "%d %d\n" (topics ! i) prediction
  -- print $ accuracy
  --           topics
  --           (V.map (V.maxIndex . predict) (V.generate (V.length topics) id))
  -- replicateM 5 . withGen g (print . sort) $ do
  --   pred <- predict
  --   let p = pred $ V.fromList $ [0,1,7]
  --   replicateM 100 p
