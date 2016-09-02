{-# LANGUAGE OverloadedLists  #-}

module NaiveBayes where

-- Implements Gibbs sampling for a semi-supervised naive bayes
-- model. Includes functions to load text and classify.

import qualified Data.Map.Strict       as M
import qualified Data.ByteString       as BS
import qualified Data.Text.ICU.Convert as ICU
import qualified Data.Text.IO          as IO
import qualified Data.Text             as T
import qualified Data.Vector           as V
import qualified Data.Set              as S
import           Data.Char
import           Control.Monad
import           System.Directory   (listDirectory)
import           System.FilePath

type Label    = String
type Features = M.Map T.Text Int
type Dataset  = V.Vector (Features, Label)
    
bagOfWords :: T.Text -> Features
bagOfWords d = foldr (\x m -> M.insertWith (\_ old -> old + 1) x 1 m)
                M.empty
                (map (T.filter isAlphaNum) (T.words d))

dropRareWords :: Features -> Features
dropRareWords = M.filter (> 10)

trainTestSplit
    :: Double
    -> Dataset
    -> (Dataset, Dataset)
trainTestSplit r d = V.splitAt (trainSize $ V.length d) d
  where trainSize s = floor (r * fromIntegral s)

labels :: S.Set String
labels = [ "alt.atheism"
         , "comp.graphics"
         , "sci.med"
         , "soc.religion.christian"
         ]

when' :: Applicative f
      => a
      -> Bool
      -> f a
      -> f a
when' _ True  m = m
when' d False _ = pure d

convReadFile :: ICU.Converter -> FilePath -> IO T.Text
convReadFile conv file = do
  bs   <- BS.readFile file
  return (ICU.toUnicode conv bs)

loadDataset :: S.Set String -> String -> IO Dataset
loadDataset labels s = do
  conv <- ICU.open "utf-8" Nothing
  dirs <- listDirectory s
  rows <- forM dirs $ \l ->
            when' mempty (S.member l labels) $ do
                  files <- listDirectory (s </> l)
                  forM (V.fromList files) $ \f -> do
                    contents <- convReadFile conv (s </> l </> f)
                    return (bagOfWords contents, l)
  let (train, test) = trainTestSplit 0.8 (mconcat rows)
  return train
