{-# LANGUAGE DataKinds, NegativeLiterals #-}

module NBHakaru where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import qualified System.Random.MWC.Distributions  as MWCD
import           Control.Monad
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as MV
import qualified Data.Set                         as S
import qualified Data.Map.Strict                  as M
import qualified Data.Text                        as T

-- this is \gamma_{\pi} in the Resnik & Hardisty paper
labelHP :: Double
labelHP = 1.0

labelPrior :: Int -> MWC.GenIO -> IO (V.Vector Double)
labelPrior k g = MWCD.dirichlet (V.generate k (const labelHP)) g

-- this is \gamma_{\theta} in the Resnik & Hardisty paper
vocabHP :: Double
vocabHP = 1.0

vocabPrior :: Int -> MWC.GenIO -> IO (V.Vector Double)
vocabPrior v g = MWCD.dirichlet (V.generate v (const vocabHP)) g

prog = 
  let_ (lam $ \ as1 ->
        (plate (unsafeNat (nat2int (size as1) +
                           negate (nat2int (nat_ 1)))) $
               \ i3 ->
               beta (summate (i3 + nat_ 1) (size as1) (\ j4 -> as1 ! j4))
                    (as1 ! i3)) >>= \ xs2 ->
        dirac (array (size as1) $
                     \ i5 ->
                     let_ (product (nat_ 0) i5 (\ j7 -> xs2 ! j7)) $ \ x6 ->
                     x6 *
                     case_ (i5 + nat_ 1 == size as1)
                           [branch ptrue (nat2prob (nat_ 1)),
                            branch pfalse
                                   (unsafeProb (nat2real (nat_ 1) +
                                                negate (fromProb (xs2 ! i5))))])) $ \ dirichlet0 ->
  let_ (lam $ \ topic_prior9 ->
        lam $ \ word_prior10 ->
        lam $ \ z11 ->
        lam $ \ w12 ->
        lam $ \ doc13 ->
        lam $ \ docUpdate14 ->
        case_ (docUpdate14 < size z11)
              [branch ptrue
                      (dirichlet0 `app` topic_prior9 >>= \ theta15 ->
                       (plate (size topic_prior9) $
                              \ k17 -> dirichlet0 `app` word_prior10) >>= \ phi16 ->
                       categorical (array (size topic_prior9) $
                                          \ i19 -> nat2prob (nat_ 1)) >>= \ zNew18 ->
                       (plate (size z11) $
                              \ i21 ->
                              let_ (case_ (i21 == docUpdate14)
                                          [branch ptrue (zNew18),
                                           branch pfalse (z11 ! i21)]) $ \ zz22 ->
                              (pose (theta15 ! zz22 *
                                     recip (summate (nat_ 0)
                                                    (size theta15)
                                                    (\ x0 -> theta15 ! x0))) $
                                    (dirac (ann_ (SData (STyCon (SingSymbol :: Sing "Unit"))
                                                            (SPlus SDone SVoid))
                                                 (unit)))) >>= \ x23 ->
                              dirac zz22) >>= \ z20 ->
                       (plate (size w12) $
                              \ n24 ->
                              (pose (phi16 ! (z20 ! (doc13 ! n24)) ! (w12 ! n24) *
                                     recip (summate (nat_ 0)
                                                    (size (phi16 ! (z20 ! (doc13 ! n24))))
                                                    (\ x0 -> phi16 ! (z20 ! (doc13 ! n24)) ! x0))) $
                                    (dirac (ann_ (SData (STyCon (SingSymbol :: Sing "Unit"))
                                                            (SPlus SDone SVoid))
                                                 (unit)))) >>= \ x25 ->
                              dirac (w12 ! n24)) >>= \ w23 ->
                       dirac zNew18),
               branch pfalse (reject)]) $ \ naive_bayes8 ->
  naive_bayes8

type HDataset = (V.Vector Integer, V.Vector Integer)

-- Sample function to use with Hakaru-generated implementation
sampleH
    :: Int          -- ^ Total number of iterations
    -> Int          -- ^ Total number of categories
    -> HDataset     -- ^ Training Set
    -> HDataset     -- ^ Testing Set
    -> MWC.GenIO    -- ^ random seed
    -> IO HDataset  -- ^ New Testing Set
sampleH iters k train test g = undefined
  --prog vocabPrior labelPrior

loadDatasetH :: S.Set String -> FilePath -> HDataset
loadDatasetH = undefined
