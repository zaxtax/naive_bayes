{-# LANGUAGE DataKinds, NegativeLiterals #-}
module GibbsOpt where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

gibbsOpt =
  lam $ \ topic_prior0 ->
  let_ (summate (nat_ 0) (size topic_prior0) (\ i丙14 -> topic_prior0 ! i丙14)) $ \ summation_topic_prior ->
  lam $ \ word_prior1 ->
  let_ (summate (nat_ 0) (size word_prior1) (\ i丙19 -> word_prior1 ! i丙19)) $ \ summation_word_prior ->
  lam $ \ z2 ->
  lam $ \ w3 ->
  lam $ \ doc4 ->
  lam $ \ docUpdate5 ->
  (array (size topic_prior0) $
                     \ zNew丏6 ->
                     let_ (nat2prob (summate (nat_ 0) (size z2) (\ i丙12 -> case_ (i丙12 == docUpdate5) [branch ptrue (nat_ 0), branch pfalse (case_ (zNew丏6 == z2 ! i丙12) [branch ptrue (nat_ 1), branch pfalse (nat_ 0)])]))) $ \ count_z2 ->
                     product (nat_ 0)
                             (size topic_prior0)
                             (\ i7 ->
                              (case_ (i7 == zNew丏6)
                                 [branch pfalse (prob_ 1.0),
                                  branch ptrue (
                              product (nat_ 0)
                                      (size word_prior1)
                                      (\ i丣8 ->
                                       let_ (nat2prob (summate (nat_ 0) (size w3) (\ i丙11 -> case_ (doc4 ! i丙11 == docUpdate5) [branch ptrue (nat_ 0), branch pfalse (case_ (i7 == z2 ! (doc4 ! i丙11) && i丣8 == w3 ! i丙11) [branch ptrue (nat_ 1), branch pfalse (nat_ 0)])]))) $ \ summation_3 ->
                                       product (nat_ 0)
                                               -- If i7 != zNew丏6, then the summation is always zero and looping over w3 is a waste of time
                                               (summate (nat_ 0)
                                                        (size w3)
                                                        (\ i丙10 ->
                                                         case_ (docUpdate5 == doc4 ! i丙10)
                                                               [branch ptrue
                                                                       (case_ (i7 == zNew丏6 && i丣8 == w3 ! i丙10)
                                                                              [branch ptrue (nat_ 1),
                                                                               branch pfalse (nat_ 0)]),
                                                                branch pfalse (nat_ 0)]))
                                               (\ j9 -> (summation_3 + nat2prob j9 + word_prior1 ! i丣8) *
                                                        (count_z2 + topic_prior0 ! zNew丏6) *
                                                recip (nat2prob (summate (nat_ 0)
                                                                         (size z2)
                                                                         (\ i丙13 ->
                                                                          case_ (i丙13 == docUpdate5)
                                                                                [branch ptrue (nat_ 0),
                                                                                 branch pfalse (case_ (z2 ! i丙13 < nat_ 0)
                                                                                                      [branch ptrue (nat_ 0),
                                                                                                       branch pfalse (nat_ 1)])])) +
                                                               summation_topic_prior) *
                                                recip (product (nat_ 0)
                                                               (size topic_prior0)
                                                               (\ i15 ->
                                                                product (nat_ 0)
                                                                        (summate (nat_ 0)
                                                                                 (size w3)
                                                                                 (\ i丙17 ->
                                                                                  case_ (docUpdate5 == doc4 ! i丙17)
                                                                                        [branch ptrue (case_ (i15 == zNew丏6)
                                                                                                             [branch ptrue (nat_ 1),
                                                                                                              branch pfalse (nat_ 0)]),
                                                                                         branch pfalse (nat_ 0)]))
                                                                        (\ i丣16 ->
                                                                         nat2prob (summate (nat_ 0)
                                                                                           (size w3)
                                                                                           (\ i丙18 ->
                                                                                            case_ (doc4 ! i丙18 == docUpdate5)
                                                                                                  [branch ptrue (nat_ 0),
                                                                                                   branch pfalse (case_ (i15 == z2 ! (doc4 ! i丙18))
                                                                                                                        [branch ptrue (nat_ 1),
                                                                                                                         branch pfalse (nat_ 0)])])) +
                                                                         nat2prob i丣16 +
                                                                         summation_word_prior))))))])))
