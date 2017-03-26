{-# LANGUAGE DataKinds, NegativeLiterals #-}
module GibbsOptBucket where

import           Prelude                          hiding (product)
import           Language.Hakaru.Runtime.Prelude
import           Language.Hakaru.Types.Sing
import qualified System.Random.MWC                as MWC
import           Control.Monad

gibbsOptBucket =
   lam $ \ topic_prior0 ->
   lam $ \ word_prior1 ->
   lam $ \ z2 ->
   lam $ \ w3 ->
   lam $ \ doc4 ->
   lam $ \ docUpdate5 ->
   let_ (size topic_prior0) $ \ topic_prior_size6 ->
   let_ (size word_prior1) $ \ word_prior_size7 ->
   (array topic_prior_size6 $
          \ zNew88 ->
       product (nat_ 0)
               topic_prior_size6
               (\ i9 ->
                product (nat_ 0)
                        word_prior_size7
                        (\ i1810 ->
                         product (nat_ 0)
                                 (summate (nat_ 0)
                                          (size w3)
                                          (\ i1312 ->
                                           case_ (docUpdate5 == doc4 ! i1312)
                                                 [branch ptrue
                                                         (case_ (i9 == zNew88 &&
                                                                 i1810 == w3 ! i1312)
                                                                [branch ptrue (nat_ 1),
                                                                 branch pfalse (nat_ 0)]),
                                                  branch pfalse (nat_ 0)]))
                                 (\ j11 ->
                                  (nat2prob (summate (nat_ 0)
                                                     (size w3)
                                                     (\ i1313 ->
                                                      case_ (doc4 ! i1313 == docUpdate5)
                                                            [branch ptrue (nat_ 0),
                                                             branch pfalse
                                                                    (case_ (i9
                                                                            == z2
                                                                               ! (doc4 ! i1313) &&
                                                                            i1810 == w3 ! i1313)
                                                                           [branch ptrue (nat_ 1),
                                                                            branch pfalse
                                                                                   (nat_ 0)])])) +
                                   nat2prob j11 +
                                   word_prior1 ! i1810) *
                                  (nat2prob (summate (nat_ 0)
                                                     (size z2)
                                                     (\ i1314 ->
                                                      case_ (i1314 == docUpdate5)
                                                            [branch ptrue (nat_ 0),
                                                             branch pfalse
                                                                    (case_ (zNew88 == z2 ! i1314)
                                                                           [branch ptrue (nat_ 1),
                                                                            branch pfalse
                                                                                   (nat_ 0)])])) +
                                   topic_prior0 ! zNew88) *
                                  recip (nat2prob (summate (nat_ 0)
                                                           (size z2)
                                                           (\ i1315 ->
                                                            case_ (i1315 == docUpdate5)
                                                                  [branch ptrue (nat_ 0),
                                                                   branch pfalse
                                                                          (case_ (z2 ! i1315
                                                                                  < nat_ 0)
                                                                                 [branch ptrue
                                                                                         (nat_ 0),
                                                                                  branch pfalse
                                                                                         (nat_ 1)])])) +
                                         summate (nat_ 0)
                                                 topic_prior_size6
                                                 (\ i1316 -> topic_prior0 ! i1316)) *
                                  recip (product (nat_ 0)
                                                 topic_prior_size6
                                                 (\ i17 ->
                                                  product (nat_ 0)
                                                          (summate (nat_ 0)
                                                                   (size w3)
                                                                   (\ i1319 ->
                                                                    case_ (docUpdate5
                                                                           == doc4 ! i1319)
                                                                          [branch ptrue
                                                                                  (case_ (not (w3
                                                                                               ! i1319
                                                                                               < nat_ 0) &&
                                                                                          i17
                                                                                          == zNew88)
                                                                                         [branch ptrue
                                                                                                 (nat_ 1),
                                                                                          branch pfalse
                                                                                                 (nat_ 0)]),
                                                                           branch pfalse (nat_ 0)]))
                                                          (\ i1818 ->
                                                           nat2prob (summate (nat_ 0)
                                                                             (size w3)
                                                                             (\ i1320 ->
                                                                              case_ (doc4 ! i1320
                                                                                     == docUpdate5)
                                                                                    [branch ptrue
                                                                                            (nat_ 0),
                                                                                     branch pfalse
                                                                                            (case_ (not (w3
                                                                                                         ! i1320
                                                                                                         < nat_ 0) &&
                                                                                                    i17
                                                                                                    == z2
                                                                                                       ! (doc4
                                                                                                          ! i1320))
                                                                                                   [branch ptrue
                                                                                                           (nat_ 1),
                                                                                                    branch pfalse
                                                                                                           (nat_ 0)])])) +
                                                           nat2prob i1818 +
                                                           summate (nat_ 0)
                                                                   word_prior_size7
                                                                   (\ i1321 ->
                                                                    word_prior1
                                                                    ! i1321))))))))
