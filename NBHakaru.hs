{-# LANGUAGE DataKinds, NegativeLiterals #-}

module NBHakaru where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

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
                                    (dirac (ann_ (SData (STyCon (SingSymbol :: Sing "Unit")) (SPlus SDone SVoid))
                                                 (unit)))) >>= \ x23 ->
                              dirac zz22) >>= \ z20 ->
                       (plate (size w12) $
                              \ n24 ->
                              (pose (phi16 ! (z20 ! (doc13 ! n24)) ! (w12 ! n24) *
                                     recip (summate (nat_ 0)
                                                    (size (phi16 ! (z20 ! (doc13 ! n24))))
                                                    (\ x0 -> phi16 ! (z20 ! (doc13 ! n24)) ! x0))) $
                                    (dirac (ann_ (SData (STyCon (SingSymbol :: Sing "Unit")) (SPlus SDone SVoid))
                                                 (unit)))) >>= \ x25 ->
                              dirac (w12 ! n24)) >>= \ w23 ->
                       dirac zNew18),
               branch pfalse (reject)]) $ \ naive_bayes8 ->
  naive_bayes8
