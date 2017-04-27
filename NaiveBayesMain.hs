module Main where

import           Control.Monad              (forever, replicateM, forM_)
import qualified Data.ByteString.Char8 as B
import           Data.List                  (sort)
import           Data.Number.LogFloat
import qualified Data.Vector.Unboxed   as V
import           Data.Vector.Unboxed        (Vector, (!))
import           GibbsOptBucket             (prog)
import           Language.Hakaru.Runtime.LogFloatPrelude
                                            (unMeasure, prob_, categorical)
import           News                       (getNews)
import           System.Environment         (getArgs)
import qualified System.Random.MWC as MWC
import           Text.Printf                (printf)

onesFrom :: Vector Int -> Vector LogFloat
onesFrom v = V.replicate (V.maximum v + 1) 1

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

initializeZs
    :: Int
    -> Int
    -> MWC.GenIO
    -> IO (Vector Int)
initializeZs testSize numTopics g =
    V.replicateM testSize $
      sample g (categorical $ V.replicate numTopics (prob_ 1))

main = do
  args  <- getArgs
  case length args == 2 of
    False -> putStrLn "./naive-bayes <docsPerTopic≻ <trial>"
    True  -> do
         let [docsPerTopic, trail] = map read args :: [Int]
         (words, docs, topics) <- getNews (Just docsPerTopic) [0..]
         g <- MWC.createSystemRandom
         let numTopics        = 20
             trainTestSplit   = 0.9
             testDocsPerTopic = floor (fromIntegral docsPerTopic *
                                       (1.0 - trainTestSplit))
             topicIndices     = getTopicIndices
                                  testDocsPerTopic
                                  docsPerTopic
                                  topics
                                  numTopics

             zTrues  = V.map (topics !) topicIndices
         zInits  <- initializeZs (testDocsPerTopic * numTopics) numTopics g
         let topics' = V.update_ topics topicIndices zInits
             zPrior  = onesFrom topics
             wPrior  = onesFrom words
             predict = prog zPrior wPrior topics' words docs
         zPreds <- V.forM topicIndices $ \i -> sample g (predict i)
         print zTrues
         print zPreds
         print $ accuracy zTrues zPreds

getTopicIndices testDocsPerTopic docsPerTopic topics numTopics =
    V.concatMap
         (\i -> V.generate (testDocsPerTopic + 1) (+ i))
         seq
    where seq = V.generate numTopics (* docsPerTopic)
