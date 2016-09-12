module NaiveBayes where

-- Implements Gibbs sampling for a semi-supervised naive bayes
-- model. Includes functions to load text and classify.

import qualified Data.Map.Strict                 as M
import qualified Data.ByteString                 as BS
import qualified Data.Text.ICU.Convert           as ICU
import qualified Data.Text.IO                    as IO
import qualified Data.Text                       as T
import           Data.Tuple
import qualified Data.Vector                     as V
import qualified Data.Vector.Mutable             as MV
import qualified Data.Set                        as S
import           Data.Char
import           Control.Monad
import qualified System.Random.MWC               as MWC
import qualified System.Random.MWC.Distributions as MWCD
import           System.Directory   (listDirectory)
import           System.FilePath

type Label    = Int
type Vocab    = M.Map T.Text Int
type Features = M.Map T.Text Int -- TODO: replace with IntMap Int?
type Dataset  = V.Vector (Features, Label)

getFeatures :: Dataset -> V.Vector Features
getFeatures = fst . V.unzip

getLabels :: Dataset -> V.Vector Label
getLabels = snd . V.unzip

bagOfWords :: T.Text -> Features
bagOfWords d = foldr (\x m -> M.insertWith (\_ old -> old + 1) x 1 m)
                M.empty
                (map (T.filter isAlphaNum) (T.words d))

sumWordCounts :: V.Vector Features -> Features
sumWordCounts = M.unionsWith (+) . V.toList

addWordCount :: Features -> Features -> Features
addWordCount = M.unionWith (+)

subWordCount :: Features -> Features -> Features
subWordCount = M.unionWith (-)

dropRareWords :: Features -> Features
dropRareWords = M.filter (> 10)

trainTestSplit
    :: Double
    -> Dataset
    -> (Dataset, Dataset)
trainTestSplit r d = V.splitAt (trainSize $ V.length d) d
  where trainSize s = floor (r * fromIntegral s)

-- this is \gamma_{\pi} in the Resnick & Hardisty paper
labelHP :: Double
labelHP = 1.0

labelPrior :: Int -> MWC.GenIO -> IO (V.Vector Double)
labelPrior k g = MWCD.dirichlet (V.generate k (const labelHP)) g

-- this is \gamma_{\theta} in the Resnick & Hardisty paper
vocabHP :: Double
vocabHP = 1.0

vocabPrior :: Int -> MWC.GenIO -> IO (V.Vector Double)
vocabPrior v g = MWCD.dirichlet (V.generate v (const vocabHP)) g

initLabels
    :: Int
    -> Int
    -> MWC.GenIO
    -> IO (V.Vector Int)
initLabels k n g = do
  m <- labelPrior k g
  V.replicateM n (MWCD.categorical m g)

buildVocab
    :: Dataset
    -> Vocab
buildVocab = M.fromAscList
           . flip zip [0..]
           . M.keys
           . sumWordCounts
           . getFeatures

-- Calculates number of documents with a given category (C_x)
-- from list of current category labels
countDocumentCategories
    :: V.Vector Label
    -> V.Vector Int
countDocumentCategories
    = V.fromList
    . M.elems
    . M.fromListWith (+)
    . map (\x -> (x,1))
    . V.toList

-- Calculates frequency of a word in a given category (N_c)
countWordFreqCategories
    :: Dataset
    -> V.Vector Features
countWordFreqCategories
    = V.fromList
    . M.elems
    . M.fromListWith addWordCount
    . V.toList
    . V.map swap

sampleLabel
    :: Int                        -- ^ Total number of documents (train+test)
    -> Int                        -- ^ Total number of categories
    -> Vocab                      -- ^ vocab list
    -> V.Vector (V.Vector Double) -- ^ probability of category c for word i
    -> V.Vector Label             -- ^ current labels for documents
    -> Features                   -- ^ word counts for document j
    -> MWC.GenIO                  -- ^ random seed
    -> IO Label                   -- ^ new label to assign to document
sampleLabel n k vocab theta l wc g =
    MWCD.categorical labelPosterior g
    where
      categoryCounts x = fromIntegral (countDocumentCategories l V.! x)
      docLikelihood t  = M.foldrWithKey' (\word freq acc ->
                           (t V.! (vocab M.! word)) * fromIntegral freq)
                           0
                           wc
      labelPosterior   = V.generate k $ \x ->
        exp $ log (categoryCounts x + labelHP - 1)   -
              log (fromIntegral n + 2 * labelHP - 1) +
              docLikelihood (theta V.! x)

sampleTheta
    :: Vocab
    -> Dataset
    -> MWC.GenIO
    -> IO (V.Vector (V.Vector Double))
sampleTheta vocab d g =
    V.generateM k (flip MWCD.dirichlet g . t)
    where n             = countWordFreqCategories d
          k             = V.length n
          vocab'        = V.fromList (M.keys vocab)
          findCount v c = M.findWithDefault 0 v (n V.! c)
          t c           = flip V.map vocab' $ \v ->
                              fromIntegral (findCount v c) + vocabHP

sampleIter
    :: Int
    -> V.Vector (V.Vector Double)
    -> Vocab
    -> Dataset
    -> Dataset
    -> MWC.GenIO
    -> IO (Dataset, V.Vector (V.Vector Double))
sampleIter k theta vocab train test g = do
    test'  <- iterateM (V.length test) go test
    theta' <- sampleTheta vocab (train V.++ test) g
    return (test', theta')
    where
      n    = V.length train + V.length test
      go j test' =
          let (testPre, wcAndtestPost) = V.splitAt j test'
              (wc, _)  = V.head wcAndtestPost
              testPost = V.tail wcAndtestPost
              labels   = getLabels testPre V.++ getLabels testPost
          in do l <- sampleLabel n k vocab theta labels wc g
                return $ V.modify (\mv ->  MV.write mv j (wc, l)) test'

iterateM
    :: Monad m
    => Int
    -> (Int -> a -> m a)
    -> a
    -> m a
iterateM 0 _ a = return a
iterateM n f a = f n a >>= iterateM (n-1) f

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
                    return (bagOfWords contents, S.findIndex l labels)
  let (train, test) = trainTestSplit 0.8 (mconcat rows)
  return train
