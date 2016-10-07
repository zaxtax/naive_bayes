{-# LANGUAGE DataKinds, NegativeLiterals #-}

module Main where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import           System.CPUTime
import qualified System.Random.MWC                as MWC
import qualified System.Random.MWC.Distributions  as MWCD
import           Control.Monad
import qualified Data.Vector                      as V
import qualified Data.Vector.Mutable              as MV
import qualified Data.Set                         as S
import qualified Data.Map.Strict                  as M
import qualified Data.Text                        as T
import           Text.Printf

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

type Label    = Int
type Vocab    = M.Map T.Text Int
type Features = V.Vector Integer
type Dataset  = (Features, Label)

-- Sample function to use with Hakaru-generated implementation
sampleH
    :: Int         -- ^ Total number of iterations
    -> Int         -- ^ Total number of categories
    -> Dataset     -- ^ Training Set
    -> Dataset     -- ^ Testing Set
    -> MWC.GenIO   -- ^ Random seed
    -> IO Dataset  -- ^ New Testing Set
sampleH iters k train test g = undefined

featuresToHFeatures = undefined

runner :: IO ()
runner = do
    g      <- MWC.createSystemRandom
    -- Just (z, w) <- unMeasure (generateDataset 20 40000 20000 6000000 doc) g
    -- vocabP <- vocabPrior 40000 g
    -- labelP <- labelPrior 20 g
    start  <- getCPUTime
    Just (z, w) <- unMeasure (generateDataset 10 4000 1000 6000 doc) g
    mid    <- getCPUTime
    printf "Time to generate data: %0.3f sec\n" (diff start mid)
    vocabP <- vocabPrior 4000 g
    labelP <- labelPrior 10 g
    sample <- unMeasure (gibbs vocabP labelP z w doc 1) g
    stop   <- getCPUTime
    printf "Time to gibbs update: %0.3f sec\n" (diff mid stop)
    print sample
  where doc = V.concat $ map (V.replicate 1000) [0..5]
        diff :: Integer -> Integer -> Double
        diff start end = (fromIntegral (end - start)) / (10^12)
--  where doc = V.concat $ map (\i -> V.replicate 1000 i) [0..19]

main = runner

generateDataset = 
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
  let_ (lam $ \k9 ->
        lam $ \v10 ->
        lam $ \z11 ->
        lam $ \w12 ->
        lam $ \doc13 ->
        let_ (array k9 $ \k15 -> prob_ 1) $ \ topic_prior14 ->
        let_ (array v10 $ \v17 -> prob_ 1) $ \ word_prior16 ->
        dirichlet0 `app` topic_prior14 >>= \ theta18 ->
        (plate k9 $ \k20 -> dirichlet0 `app` word_prior16) >>= \ phi19 ->
        (plate z11 $ \i22 -> categorical theta18) >>= \ z21 ->
        (plate w12 $
               \ n24 -> categorical (phi19 ! (z21 ! (doc13 ! n24)))) >>= \ w23 ->
        dirac (ann_ (SData (STyApp (STyApp (STyCon (SingSymbol :: Sing "Pair")) (SArray SNat)) (SArray SNat)) (SPlus (SEt (SKonst (SArray SNat)) (SEt (SKonst (SArray SNat)) SDone)) SVoid))
                    ((pair z21 w23)))) $ \ naive_bayes8 ->
  naive_bayes8


gibbs = 
  lam $ \ topic_prior0 ->
  lam $ \ word_prior1 ->
  lam $ \ z2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ docUpdate5 ->
  case_ (docUpdate5 < size z2)
        [branch ptrue
                ((pose (product (nat_ 0)
                                (size topic_prior0)
                                (\ i6 ->
                                 product (nat_ 0)
                                         (size word_prior1)
                                         (\ i丣7 ->
                                          product (nat_ 0)
                                                  (summate (nat_ 0)
                                                           (size w3)
                                                           (\ i丙9 ->
                                                            case_ (docUpdate5 == doc4 ! i丙9)
                                                                  [branch ptrue (nat_ 0),
                                                                   branch pfalse
                                                                          (case_ (i6
                                                                                  == z2
                                                                                     ! (doc4
                                                                                        ! i丙9) &&
                                                                                  i丣7 == w3 ! i丙9)
                                                                                 [branch ptrue
                                                                                         (nat_ 1),
                                                                                  branch pfalse
                                                                                         (nat_ 0)])]))
                                                  (\ j8 ->
                                                   (nat2prob j8 + word_prior1 ! i丣7) *
                                                   product (nat_ 0)
                                                           (size topic_prior0)
                                                           (\ i10 ->
                                                            product (nat_ 0)
                                                                    (summate (nat_ 0)
                                                                             (size z2)
                                                                             (\ i丙12 ->
                                                                              case_ (i丙12
                                                                                     == docUpdate5)
                                                                                    [branch ptrue
                                                                                            (nat_ 0),
                                                                                     branch pfalse
                                                                                            (case_ (i10
                                                                                                    == z2
                                                                                                       ! i丙12)
                                                                                                   [branch ptrue
                                                                                                           (nat_ 1),
                                                                                                    branch pfalse
                                                                                                           (nat_ 0)])]))
                                                                    (\ j11 ->
                                                                     (nat2prob j11 +
                                                                      topic_prior0 ! i10) *
                                                                     recip (product (nat_ 0)
                                                                                    (summate (nat_ 0)
                                                                                             (size z2)
                                                                                             (\ i丙14 ->
                                                                                              case_ (i丙14
                                                                                                     == docUpdate5)
                                                                                                    [branch ptrue
                                                                                                            (nat_ 0),
                                                                                                     branch pfalse
                                                                                                            (case_ (z2
                                                                                                                    ! i丙14
                                                                                                                    < nat_ 0)
                                                                                                                   [branch ptrue
                                                                                                                           (nat_ 0),
                                                                                                                    branch pfalse
                                                                                                                           (nat_ 1)])]))
                                                                                    (\ i13 ->
                                                                                     (nat2prob i13 +
                                                                                      summate (nat_ 0)
                                                                                              (size topic_prior0)
                                                                                              (\ i丙15 ->
                                                                                               topic_prior0
                                                                                               ! i丙15)) *
                                                                                     recip (product (nat_ 0)
                                                                                                    (size topic_prior0)
                                                                                                    (\ i16 ->
                                                                                                     product (nat_ 0)
                                                                                                             (summate (nat_ 0)
                                                                                                                      (size w3)
                                                                                                                      (\ i丙18 ->
                                                                                                                       case_ (docUpdate5
                                                                                                                              == doc4
                                                                                                                                 ! i丙18)
                                                                                                                             [branch ptrue
                                                                                                                                     (nat_ 0),
                                                                                                                              branch pfalse
                                                                                                                                     (case_ (not (w3
                                                                                                                                                  ! i丙18
                                                                                                                                                  < nat_ 0) &&
                                                                                                                                             i16
                                                                                                                                             == z2
                                                                                                                                                ! (doc4
                                                                                                                                                   ! i丙18))
                                                                                                                                            [branch ptrue
                                                                                                                                                    (nat_ 1),
                                                                                                                                             branch pfalse
                                                                                                                                                    (nat_ 0)])]))
                                                                                                             (\ i丣17 ->
                                                                                                              nat2prob i丣17 +
                                                                                                              summate (nat_ 0)
                                                                                                                      (size word_prior1)
                                                                                                                      (\ i丙19 ->
                                                                                                                       word_prior1
                                                                                                                       ! i丙19)))))))))))) $
                       (categorical (array (size topic_prior0) $
                                           \ zNew丏20 ->
                                           product (nat_ 0)
                                                   (size topic_prior0)
                                                   (\ i21 ->
                                                    product (nat_ 0)
                                                            (size word_prior1)
                                                            (\ i丣22 ->
                                                             product (nat_ 0)
                                                                     (summate (nat_ 0)
                                                                              (size w3)
                                                                              (\ i丙24 ->
                                                                               case_ (docUpdate5
                                                                                      == doc4
                                                                                         ! i丙24)
                                                                                     [branch ptrue
                                                                                             (case_ (i21
                                                                                                     == zNew丏20 &&
                                                                                                     i丣22
                                                                                                     == w3
                                                                                                        ! i丙24)
                                                                                                    [branch ptrue
                                                                                                            (nat_ 1),
                                                                                                     branch pfalse
                                                                                                            (nat_ 0)]),
                                                                                      branch pfalse
                                                                                             (nat_ 0)]))
                                                                     (\ j23 ->
                                                                      (nat2prob (summate (nat_ 0)
                                                                                         (size w3)
                                                                                         (\ i丙25 ->
                                                                                          case_ (doc4
                                                                                                 ! i丙25
                                                                                                 == docUpdate5)
                                                                                                [branch ptrue
                                                                                                        (nat_ 0),
                                                                                                 branch pfalse
                                                                                                        (case_ (i21
                                                                                                                == z2
                                                                                                                   ! (doc4
                                                                                                                      ! i丙25) &&
                                                                                                                i丣22
                                                                                                                == w3
                                                                                                                   ! i丙25)
                                                                                                               [branch ptrue
                                                                                                                       (nat_ 1),
                                                                                                                branch pfalse
                                                                                                                       (nat_ 0)])])) +
                                                                       nat2prob j23 +
                                                                       word_prior1 ! i丣22) *
                                                                      (nat2prob (summate (nat_ 0)
                                                                                         (size z2)
                                                                                         (\ i丙26 ->
                                                                                          case_ (i丙26
                                                                                                 == docUpdate5)
                                                                                                [branch ptrue
                                                                                                        (nat_ 0),
                                                                                                 branch pfalse
                                                                                                        (case_ (zNew丏20
                                                                                                                == z2
                                                                                                                   ! i丙26)
                                                                                                               [branch ptrue
                                                                                                                       (nat_ 1),
                                                                                                                branch pfalse
                                                                                                                       (nat_ 0)])])) +
                                                                       topic_prior0 ! zNew丏20) *
                                                                      recip (nat2prob (summate (nat_ 0)
                                                                                               (size z2)
                                                                                               (\ i丙27 ->
                                                                                                case_ (i丙27
                                                                                                       == docUpdate5)
                                                                                                      [branch ptrue
                                                                                                              (nat_ 0),
                                                                                                       branch pfalse
                                                                                                              (case_ (z2
                                                                                                                      ! i丙27
                                                                                                                      < nat_ 0)
                                                                                                                     [branch ptrue
                                                                                                                             (nat_ 0),
                                                                                                                      branch pfalse
                                                                                                                             (nat_ 1)])])) +
                                                                             summate (nat_ 0)
                                                                                     (size topic_prior0)
                                                                                     (\ i丙28 ->
                                                                                      topic_prior0
                                                                                      ! i丙28)) *
                                                                      recip (product (nat_ 0)
                                                                                     (size topic_prior0)
                                                                                     (\ i29 ->
                                                                                      product (nat_ 0)
                                                                                              (summate (nat_ 0)
                                                                                                       (size w3)
                                                                                                       (\ i丙31 ->
                                                                                                        case_ (docUpdate5
                                                                                                               == doc4
                                                                                                                  ! i丙31)
                                                                                                              [branch ptrue
                                                                                                                      (case_ (not (w3
                                                                                                                                   ! i丙31
                                                                                                                                   < nat_ 0) &&
                                                                                                                              i29
                                                                                                                              == zNew丏20)
                                                                                                                             [branch ptrue
                                                                                                                                     (nat_ 1),
                                                                                                                              branch pfalse
                                                                                                                                     (nat_ 0)]),
                                                                                                               branch pfalse
                                                                                                                      (nat_ 0)]))
                                                                                              (\ i丣30 ->
                                                                                               nat2prob (summate (nat_ 0)
                                                                                                                 (size w3)
                                                                                                                 (\ i丙32 ->
                                                                                                                  case_ (doc4
                                                                                                                         ! i丙32
                                                                                                                         == docUpdate5)
                                                                                                                        [branch ptrue
                                                                                                                                (nat_ 0),
                                                                                                                         branch pfalse
                                                                                                                                (case_ (not (w3
                                                                                                                                             ! i丙32
                                                                                                                                             < nat_ 0) &&
                                                                                                                                        i29
                                                                                                                                        == z2
                                                                                                                                           ! (doc4
                                                                                                                                              ! i丙32))
                                                                                                                                       [branch ptrue
                                                                                                                                               (nat_ 1),
                                                                                                                                        branch pfalse
                                                                                                                                               (nat_ 0)])])) +
                                                                                               nat2prob i丣30 +
                                                                                               summate (nat_ 0)
                                                                                                       (size word_prior1)
                                                                                                       (\ i丙33 ->
                                                                                                        word_prior1
                                                                                                        ! i丙33))))))))))),
         branch pfalse (reject)]
