module Main where

import qualified Data.ByteString.Char8 as B
import News (getNews)
import qualified System.Random.MWC as MWC
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import Text.Printf (printf)
import GibbsOptBucket (prog)
import Control.Monad (forever, replicateM, forM_)
import Data.List (sort)
import Data.Number.LogFloat
import Data.Monoid
import System.IO
import System.Environment


writeVec :: String -> V.Vector Int -> IO ()
writeVec file v = withFile file WriteMode $ \h -> do
  V.forM_ v $ \x -> hPrint h x

main = do
  args  <- getArgs
  case length args == 1 of
    False -> putStrLn "./write-news <docsPerTopic≻"
    True  -> do
      let suffix = "." <> head args
      (words, docs, topics) <- getNews (Just 10) [0..]
      writeVec ("words"  <> suffix) words
      writeVec ("docs"   <> suffix) docs
      writeVec ("topics" <> suffix) topics
