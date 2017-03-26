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
   (array (size topic_prior0) $
       \ zNew86 ->
       product (nat_ 0)
               (size topic_prior0)
               (\ i7 ->
                product (nat_ 0)
                        (size word_prior1)
                        (\ i188 ->
                         product (nat_ 0)
                                 (let_ (bucket (nat_ 0)
                                               (size w3)
                                               ((r_fanout (r_split (\ (i1311,()) ->
                                                                    docUpdate5
                                                                    == doc4 ! i1311)
                                                                   (r_index (\ () ->
                                                                             size word_prior1)
                                                                            (\ (i1311,()) ->
                                                                             w3
                                                                             ! i1311)
                                                                            (r_add (\ (i1311,(i1812,())) ->
                                                                                    nat_ 1)))
                                                                   r_nop)
                                                          r_nop))) $ \ summary10 ->
                                  case_ (i7 == zNew86)
                                        [branch ptrue
                                                (case_ (case_ summary10
                                                              [branch (ppair PVar PVar)
                                                                      (\ y13 z14 -> y13)])
                                                       [branch (ppair PVar PVar) (\ y15 z16 -> y15)]
                                                 ! i188),
                                         branch pfalse (nat_ 0)])
                                 (\ j9 ->
                                  (nat2prob (let_ (bucket (nat_ 0)
                                                          (size w3)
                                                          ((r_split (\ (i1318,()) ->
                                                                     doc4 ! i1318
                                                                     == docUpdate5)
                                                                    r_nop
                                                                    (r_index (\ () ->
                                                                              size topic_prior0)
                                                                             (\ (i1318,()) ->
                                                                              z2
                                                                              ! (doc4 ! i1318))
                                                                             (r_index (\ (i19,()) ->
                                                                                       size word_prior1)
                                                                                      (\ (i1318,(i19,())) ->
                                                                                       w3
                                                                                       ! i1318)
                                                                                      (r_add (\ (i1318,(i1820,(i19,()))) ->
                                                                                              nat_ 1))))))) $ \ summary17 ->
                                             case_ summary17
                                                   [branch (ppair PVar PVar) (\ y21 z22 -> z22)]
                                             ! i7
                                             ! i188) +
                                   nat2prob j9 +
                                   word_prior1 ! i188) *
                                  (nat2prob (let_ (bucket (nat_ 0)
                                                          (size z2)
                                                          ((r_split (\ (i1324,()) ->
                                                                     i1324
                                                                     == docUpdate5)
                                                                    r_nop
                                                                    (r_index (\ () ->
                                                                              size topic_prior0)
                                                                             (\ (i1324,()) ->
                                                                              z2
                                                                              ! i1324)
                                                                             (r_add (\ (i1324,(zNew825,())) ->
                                                                                     nat_ 1)))))) $ \ summary23 ->
                                             case_ summary23
                                                   [branch (ppair PVar PVar) (\ y26 z27 -> z27)]
                                             ! zNew86) +
                                   topic_prior0 ! zNew86) *
                                  recip (nat2prob (summate (nat_ 0)
                                                           (size z2)
                                                           (\ i1328 ->
                                                            case_ (i1328 == docUpdate5)
                                                                  [branch ptrue (nat_ 0),
                                                                   branch pfalse
                                                                          (case_ (z2 ! i1328
                                                                                  < nat_ 0)
                                                                                 [branch ptrue
                                                                                         (nat_ 0),
                                                                                  branch pfalse
                                                                                         (nat_ 1)])])) +
                                         summate (nat_ 0)
                                                 (size topic_prior0)
                                                 (\ i1329 -> topic_prior0 ! i1329)) *
                                  recip (product (nat_ 0)
                                                 (size topic_prior0)
                                                 (\ i不30 ->
                                                  product (nat_ 0)
                                                          (let_ (bucket (nat_ 0)
                                                                        (size w3)
                                                                        ((r_fanout (r_split (\ (i1333,()) ->
                                                                                             w3
                                                                                             ! i1333
                                                                                             < nat_ 0)
                                                                                            r_nop
                                                                                            (r_split (\ (i1333,()) ->
                                                                                                      docUpdate5
                                                                                                      == doc4
                                                                                                         ! i1333)
                                                                                                     (r_add (\ (i1333,()) ->
                                                                                                             nat_ 1))
                                                                                                     r_nop))
                                                                                   r_nop))) $ \ summary32 ->
                                                           case_ (i不30 == zNew86)
                                                                 [branch ptrue
                                                                         (case_ (case_ (case_ summary32
                                                                                              [branch (ppair PVar
                                                                                                             PVar)
                                                                                                      (\ y34
                                                                                                         z35 ->
                                                                                                       y34)])
                                                                                       [branch (ppair PVar
                                                                                                      PVar)
                                                                                               (\ y36
                                                                                                  z37 ->
                                                                                                z37)])
                                                                                [branch (ppair PVar
                                                                                               PVar)
                                                                                        (\ y38
                                                                                           z39 ->
                                                                                         y38)]),
                                                                  branch pfalse (nat_ 0)])
                                                          (\ i18丏31 ->
                                                           nat2prob (let_ (bucket (nat_ 0)
                                                                                  (size w3)
                                                                                  ((r_split (\ (i1341,()) ->
                                                                                             w3
                                                                                             ! i1341
                                                                                             < nat_ 0)
                                                                                            r_nop
                                                                                            (r_split (\ (i1341,()) ->
                                                                                                      doc4
                                                                                                      ! i1341
                                                                                                      == docUpdate5)
                                                                                                     r_nop
                                                                                                     (r_index (\ () ->
                                                                                                               size topic_prior0)
                                                                                                              (\ (i1341,()) ->
                                                                                                               z2
                                                                                                               ! (doc4
                                                                                                                  ! i1341))
                                                                                                              (r_add (\ (i1341,(i不42,())) ->
                                                                                                                      nat_ 1))))))) $ \ summary40 ->
                                                                     case_ (case_ summary40
                                                                                  [branch (ppair PVar
                                                                                                 PVar)
                                                                                          (\ y43
                                                                                             z44 ->
                                                                                           z44)])
                                                                           [branch (ppair PVar PVar)
                                                                                   (\ y45 z46 ->
                                                                                    z46)]
                                                                     ! i不30) +
                                                           nat2prob i18丏31 +
                                                           summate (nat_ 0)
                                                                   (size word_prior1)
                                                                   (\ i1347 ->
                                                                    word_prior1
                                                                    ! i1347))))))))
