{-# LANGUAGE OverloadedLists  #-}

module Main where

import           NaiveBayes
import qualified Data.Set                        as S
import qualified Data.Vector                     as V
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWCD
import           Text.Printf

labels :: S.Set String
labels = [ "alt.atheism"
         , "comp.graphics"
         , "sci.space"
         , "sci.med"
         , "rec.autos"
         , "rec.sport.hockey"
         , "talk.politics.guns"
         , "misc.forsale"
         , "soc.religion.christian"
         ]

example :: IO Double
example = do
  g  <- MWC.createSystemRandom
  d  <- loadDataset labels "/home/zv/datasets/20_newsgroups"
  d' <- MWCD.uniformShuffle d g
  let (train, test) = labelAwareTrainTestSplit 0.85 d'
  let ytrue = getLabels test
  ypred <- getLabels <$> sample 30 (S.size labels) train test g
  return (accuracy ytrue ypred)

main :: IO ()
main = do
  putStrLn "Running inference for 20 Newsgroups:"
  acc <- example
  putStrLn $ printf "Accuracy on 20 Newsgroups: %.2f%%" (acc * 100)
