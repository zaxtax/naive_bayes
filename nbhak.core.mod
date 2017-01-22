[1 of 1] Compiling Main             ( NBHakaru.hs, NBHakaru.o )

==================== Tidy Core ====================
Result size of Tidy Core
  = {terms: 11,468, types: 8,378, coercions: 1,619}

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl_rIqm :: [Char]

lvl_rIqm = unpackCString# "error"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
$fShowArrayType1 :: Int
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 20}]
$fShowArrayType1 = I# 0#

-- RHS size: {terms: 6, types: 7, coercions: 3}
$slength5_rIqu :: SV.Vector Int -> Data.Vector.Fusion.Util.Id Int

$slength5_rIqu =
  \ (x_ahgW :: SV.Vector Int) ->
    case x_ahgW
    of _ [Occ=Dead]
    { Data.Vector.Storable.Vector ipv_spM2 ipv1_spM3 ipv2_spM4 ->
    (I# ipv_spM2) `cast` ...
    }

-- RHS size: {terms: 1, types: 0, coercions: 5}
$slength [InlPrag=[NEVER]] :: SV.Vector Int -> Int

$slength = $slength5_rIqu `cast` ...

-- RHS size: {terms: 6, types: 7, coercions: 3}
$slength6_rIqv
  :: SV.Vector Double -> Data.Vector.Fusion.Util.Id Int

$slength6_rIqv =
  \ (x_Xhya :: SV.Vector Double) ->
    case x_Xhya
    of _ [Occ=Dead]
    { Data.Vector.Storable.Vector ipv_spLg ipv1_spLh ipv2_spLi ->
    (I# ipv_spLg) `cast` ...
    }

-- RHS size: {terms: 1, types: 0, coercions: 5}
$slength2 [InlPrag=[NEVER]] :: SV.Vector Double -> Int

$slength2 = $slength6_rIqv `cast` ...

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl8_rIqx :: [Char]

lvl8_rIqx =
  unpackCString#
    "vector-0.11.0.0-6uB77qGCxR6GPLxI2sqsX3"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl9_rIqy :: [Char]

lvl9_rIqy =
  unpackCString# "Data.Vector.Primitive.Mutable"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl10_rIqz :: [Char]

lvl10_rIqz =
  unpackCString# "./Data/Vector/Primitive/Mutable.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl11_rIqA :: Int

lvl11_rIqA = I# 97#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl12_rIqB :: Int

lvl12_rIqB = I# 16#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl13_rIqC :: Int

lvl13_rIqC = I# 79#

-- RHS size: {terms: 8, types: 0, coercions: 0}
lvl14_rIqD :: Types.SrcLoc

lvl14_rIqD =
  Types.SrcLoc
    lvl8_rIqx
    lvl9_rIqy
    lvl10_rIqz
    lvl11_rIqA
    lvl12_rIqB
    lvl11_rIqA
    lvl13_rIqC

-- RHS size: {terms: 4, types: 0, coercions: 0}
lvl15_rIqE :: Types.CallStack

lvl15_rIqE =
  Types.PushCallStack
    lvl_rIqm lvl14_rIqD Types.EmptyCallStack

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl16_rIqF
  :: Int#
     -> ST
          RealWorld
          (Data.Vector.Primitive.Mutable.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Int)

lvl16_rIqF =
  \ (ww_sygq :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (Data.Vector.Primitive.Mutable.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Int))
      (lvl15_rIqE `cast` ...)
      (unpackAppendCString#
         "Primitive.basicUnsafeNew: length to large: "#
         (case $wshowSignedInt 0# ww_sygq ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl17_rIqG :: Int

lvl17_rIqG = I# 96#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl18_rIqH :: Int

lvl18_rIqH = I# 15#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl19_rIqI :: Int

lvl19_rIqI = I# 78#

-- RHS size: {terms: 8, types: 0, coercions: 0}
lvl20_rIqJ :: Types.SrcLoc

lvl20_rIqJ =
  Types.SrcLoc
    lvl8_rIqx
    lvl9_rIqy
    lvl10_rIqz
    lvl17_rIqG
    lvl18_rIqH
    lvl17_rIqG
    lvl19_rIqI

-- RHS size: {terms: 4, types: 0, coercions: 0}
lvl21_rIqK :: Types.CallStack

lvl21_rIqK =
  Types.PushCallStack
    lvl_rIqm lvl20_rIqJ Types.EmptyCallStack

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl22_rIqL
  :: Int#
     -> ST
          RealWorld
          (Data.Vector.Primitive.Mutable.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Int)

lvl22_rIqL =
  \ (ww_sygq :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (Data.Vector.Primitive.Mutable.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Int))
      (lvl21_rIqK `cast` ...)
      (unpackAppendCString#
         "Primitive.basicUnsafeNew: negative length: "#
         (case $wshowSignedInt 0# ww_sygq ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl25_rIqP
  :: Int#
     -> ST
          RealWorld
          (Data.Vector.Primitive.Mutable.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Double)

lvl25_rIqP =
  \ (ipv_spHW :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (Data.Vector.Primitive.Mutable.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Double))
      (lvl15_rIqE `cast` ...)
      (unpackAppendCString#
         "Primitive.basicUnsafeNew: length to large: "#
         (case $wshowSignedInt 0# ipv_spHW ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl26_rIqQ
  :: Int#
     -> ST
          RealWorld
          (Data.Vector.Primitive.Mutable.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Double)

lvl26_rIqQ =
  \ (ipv_spHW :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (Data.Vector.Primitive.Mutable.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Double))
      (lvl21_rIqK `cast` ...)
      (unpackAppendCString#
         "Primitive.basicUnsafeNew: negative length: "#
         (case $wshowSignedInt 0# ipv_spHW ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 9, types: 11, coercions: 0}
lvl29_rIqT :: [Char]

lvl29_rIqT =
  case $wshowSignedInt 0# 0# ([] @ Char)
  of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
  : @ Char ww5_agJC ww6_agJD
  }

-- RHS size: {terms: 3, types: 0, coercions: 0}
lvl30_rIqU :: [Char]

lvl30_rIqU =
  unpackAppendCString#
    "Primitive.basicUnsafeNew: length to large: "# lvl29_rIqT

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl32_rIqW :: [Char]

lvl32_rIqW =
  unpackCString# "Data.Vector.Storable.Mutable"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl33_rIqX :: [Char]

lvl33_rIqX =
  unpackCString# "./Data/Vector/Storable/Mutable.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl34_rIqY :: Int

lvl34_rIqY = I# 121#

-- RHS size: {terms: 8, types: 0, coercions: 0}
lvl35_rIqZ :: Types.SrcLoc

lvl35_rIqZ =
  Types.SrcLoc
    lvl8_rIqx
    lvl32_rIqW
    lvl33_rIqX
    lvl34_rIqY
    lvl12_rIqB
    lvl34_rIqY
    lvl13_rIqC

-- RHS size: {terms: 4, types: 0, coercions: 0}
lvl36_rIr0 :: Types.CallStack

lvl36_rIr0 =
  Types.PushCallStack
    lvl_rIqm lvl35_rIqZ Types.EmptyCallStack

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl38_rIr2 :: Int

lvl38_rIr2 = I# 120#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl39_rIr3 :: Int

lvl39_rIr3 = I# 77#

-- RHS size: {terms: 8, types: 0, coercions: 0}
lvl40_rIr4 :: Types.SrcLoc

lvl40_rIr4 =
  Types.SrcLoc
    lvl8_rIqx
    lvl32_rIqW
    lvl33_rIqX
    lvl38_rIr2
    lvl18_rIqH
    lvl38_rIr2
    lvl39_rIr3

-- RHS size: {terms: 4, types: 0, coercions: 0}
lvl41_rIr5 :: Types.CallStack

lvl41_rIr5 =
  Types.PushCallStack
    lvl_rIqm lvl40_rIr4 Types.EmptyCallStack

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl46_rIrc :: [Char]

lvl46_rIrc = unpackCString# "(!)"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl47_rIrd :: Int

lvl47_rIrd = I# 235#

-- RHS size: {terms: 2, types: 0, coercions: 0}
file_rIre :: String

file_rIre = unpackCString# "./Data/Vector/Generic.hs"#

-- RHS size: {terms: 10, types: 4, coercions: 0}
lvl48_rIrf
  :: Int#
     -> Int# -> Data.Vector.Unboxed.Base.Vector Double

lvl48_rIrf =
  \ (n#_ahl6 :: Int#) (x_ahkY :: Int#) ->
    Data.Vector.Internal.Check.checkError
      @ (Data.Vector.Unboxed.Base.Vector Double)
      file_rIre
      lvl47_rIrd
      Data.Vector.Internal.Check.Bounds
      lvl46_rIrc
      (Data.Vector.Internal.Check.checkIndex_msg# x_ahkY n#_ahl6)

-- RHS size: {terms: 10, types: 3, coercions: 0}
lvl49_rIrg :: Int# -> Int# -> Int

lvl49_rIrg =
  \ (n#_ahl6 :: Int#) (x_ahkY :: Int#) ->
    Data.Vector.Internal.Check.checkError
      @ Int
      file_rIre
      lvl47_rIrd
      Data.Vector.Internal.Check.Bounds
      lvl46_rIrc
      (Data.Vector.Internal.Check.checkIndex_msg# x_ahkY n#_ahl6)

-- RHS size: {terms: 10, types: 3, coercions: 0}
lvl50_rIrh :: Int# -> Int# -> Double

lvl50_rIrh =
  \ (n#_ahl6 :: Int#) (x_ahkY :: Int#) ->
    Data.Vector.Internal.Check.checkError
      @ Double
      file_rIre
      lvl47_rIrd
      Data.Vector.Internal.Check.Bounds
      lvl46_rIrc
      (Data.Vector.Internal.Check.checkIndex_msg# x_ahkY n#_ahl6)

-- RHS size: {terms: 6, types: 13, coercions: 8}
$slength7_rIri
  :: MayBoxVec
       (Data.Vector.Unboxed.Base.Vector Double)
       (Data.Vector.Unboxed.Base.Vector Double)
     -> Data.Vector.Fusion.Util.Id Int

$slength7_rIri =
  \ (x_XhEh
       :: MayBoxVec
            (Data.Vector.Unboxed.Base.Vector Double)
            (Data.Vector.Unboxed.Base.Vector Double)) ->
    case x_XhEh `cast` ...
    of _ [Occ=Dead]
    { Data.Vector.Vector ipv_srbB ipv1_srbC ipv2_srbD ->
    (I# ipv1_srbC) `cast` ...
    }

-- RHS size: {terms: 1, types: 0, coercions: 8}
$s!_$slength [InlPrag=[NEVER]]
  :: MayBoxVec
       (Data.Vector.Unboxed.Base.Vector Double)
       (Data.Vector.Unboxed.Base.Vector Double)
     -> Int

$s!_$slength = $slength7_rIri `cast` ...

-- RHS size: {terms: 6, types: 8, coercions: 10}
$slength8_rIrj
  :: MayBoxVec Int Int -> Data.Vector.Fusion.Util.Id Int

$slength8_rIrj =
  \ (x_XhEq :: MayBoxVec Int Int) ->
    case x_XhEq `cast` ...
    of _ [Occ=Dead]
    { Data.Vector.Primitive.Vector ipv_srcN ipv1_srcO ipv2_srcP ->
    (I# ipv1_srcO) `cast` ...
    }

-- RHS size: {terms: 1, types: 0, coercions: 6}
$s!_$slength1 [InlPrag=[NEVER]] :: MayBoxVec Int Int -> Int

$s!_$slength1 = $slength8_rIrj `cast` ...

-- RHS size: {terms: 6, types: 8, coercions: 10}
$slength9_rIrk
  :: MayBoxVec Double Double -> Data.Vector.Fusion.Util.Id Int

$slength9_rIrk =
  \ (x_XhEt :: MayBoxVec Double Double) ->
    case x_XhEt `cast` ...
    of _ [Occ=Dead]
    { Data.Vector.Primitive.Vector ipv_srde ipv1_srdf ipv2_srdg ->
    (I# ipv1_srdf) `cast` ...
    }

-- RHS size: {terms: 1, types: 0, coercions: 6}
$s!_$slength2 [InlPrag=[NEVER]]
  :: MayBoxVec Double Double -> Int

$s!_$slength2 = $slength9_rIrk `cast` ...

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl52_rIrm
  :: Int#
     -> ST
          RealWorld
          (SV.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Double)

lvl52_rIrm =
  \ (ipv_spdP :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (SV.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Double))
      (lvl36_rIr0 `cast` ...)
      (unpackAppendCString#
         "Storable.basicUnsafeNew: length too large: "#
         (case $wshowSignedInt 0# ipv_spdP ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl53_rIrn
  :: Int#
     -> ST
          RealWorld
          (SV.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Double)

lvl53_rIrn =
  \ (ipv_spdP :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (SV.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Double))
      (lvl41_rIr5 `cast` ...)
      (unpackAppendCString#
         "Storable.basicUnsafeNew: negative length: "#
         (case $wshowSignedInt 0# ipv_spdP ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 3, types: 2, coercions: 4}
lvl56_rIrq :: Int

lvl56_rIrq =
  error
    @ 'PtrRepLifted
    @ Int
    (errorWithoutStackTrace_wild1 `cast` ...)
    Text.Read.readEither4

-- RHS size: {terms: 3, types: 2, coercions: 4}
lvl57_rIrr :: Int

lvl57_rIrr =
  error
    @ 'PtrRepLifted
    @ Int
    (errorWithoutStackTrace_wild1 `cast` ...)
    Text.Read.readEither2

-- RHS size: {terms: 4, types: 2, coercions: 2}
lvl58_rIrs :: Text.ParserCombinators.ReadP.P Int

lvl58_rIrs =
  (($fReadInt3
      $fReadInt_$sconvertInt
      Text.ParserCombinators.ReadPrec.minPrec)
   `cast` ...)
    @ Int (Text.Read.readEither5 @ Int)

-- RHS size: {terms: 15, types: 13, coercions: 0}
$sread :: String -> Int
[GblId,
 Arity=1,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=False)
         Tmpl= \ (s_agId [Occ=Once] :: String) ->
                 case Text.Read.readEither @ Int $fReadInt s_agId
                 of _ [Occ=Dead] {
                   Left x_agIl [Occ=Once] ->
                     errorWithoutStackTrace @ 'PtrRepLifted @ Int x_agIl;
                   Right y_agIo [Occ=Once] -> y_agIo
                 }}]
$sread =
  \ (s_agId :: String) ->
    case Text.Read.readEither6
           @ Int (Text.ParserCombinators.ReadP.run @ Int lvl58_rIrs s_agId)
    of _ [Occ=Dead] {
      [] -> lvl56_rIrq;
      : x_ahbu ds_ahbv ->
        case ds_ahbv of _ [Occ=Dead] {
          [] -> x_ahbu;
          : ipv_ajo9 ipv1_ajoa -> lvl57_rIrr
        }
    }

-- RHS size: {terms: 3, types: 8, coercions: 4}
lvl59_rIrt
  :: ST
       RealWorld
       (Data.Vector.Primitive.Mutable.MVector
          (Control.Monad.Primitive.PrimState (ST RealWorld))
          Int)

lvl59_rIrt =
  error
    @ 'PtrRepLifted
    @ (ST
         RealWorld
         (Data.Vector.Primitive.Mutable.MVector
            (Control.Monad.Primitive.PrimState (ST RealWorld))
            Int))
    (lvl15_rIqE `cast` ...)
    lvl30_rIqU

-- RHS size: {terms: 374, types: 691, coercions: 260}
$sreplicate_$sunstream [InlPrag=INLINE (sat-args=1)]
  :: Data.Vector.Fusion.Bundle.Bundle
       Data.Vector.Unboxed.Base.Vector Int
     -> Data.Vector.Unboxed.Base.Vector Int
[GblId,
 Arity=1,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=False,boring_ok=False)
         Tmpl= \ (s_ah2I [Occ=Once]
                    :: Data.Vector.Fusion.Bundle.Bundle
                         Data.Vector.Unboxed.Base.Vector Int) ->
                 G.new
                   @ Data.Vector.Unboxed.Base.Vector
                   @ Int
                   Data.Vector.Unboxed.Base.$fVectorVectorInt
                   (Data.Vector.Generic.New.unstream
                      @ Data.Vector.Unboxed.Base.Vector
                      @ Int
                      Data.Vector.Unboxed.Base.$fVectorVectorInt
                      s_ah2I)}]
$sreplicate_$sunstream =
  \ (s_ah2I
       :: Data.Vector.Fusion.Bundle.Bundle
            Data.Vector.Unboxed.Base.Vector Int) ->
    case s_ah2I
    of _ [Occ=Dead]
    { Data.Vector.Fusion.Bundle.Monadic.Bundle ipv_ah38 ipv1_ah39
                                               ipv2_ah3a ipv3_ah3b ->
    case runRW#
           @ 'PtrRepLifted
           @ (Data.Vector.Unboxed.Base.Vector Int)
           (\ (s2_ah7U [OS=OneShot] :: State# RealWorld) ->
              case ipv_ah38
              of _ [Occ=Dead]
              { Data.Vector.Fusion.Stream.Monadic.Stream @ s30_ah3f step_ah3g
                                                         s4_ah3h ->
              case ipv1_ah39
              of _ [Occ=Dead]
              { Data.Vector.Fusion.Stream.Monadic.Stream @ s50_ah3m vstep_ah3p
                                                         t_ah3q ->
              let {
                $w$j_sykd [InlPrag=[0]]
                  :: Int#
                     -> State# RealWorld
                     -> (# State# RealWorld,
                           G.Mutable
                             Data.Vector.Unboxed.Base.Vector
                             (Control.Monad.Primitive.PrimState (ST RealWorld))
                             Int #)

                $w$j_sykd =
                  \ (ww_sykb [OS=OneShot] :: Int#)
                    (w_syk8 [OS=OneShot] :: State# RealWorld) ->
                    case tagToEnum# @ Bool (<# ww_sykb 0#)
                    of _ [Occ=Dead] {
                      False ->
                        case divInt# 9223372036854775807# 8#
                        of ww4_agN3 { __DEFAULT ->
                        case tagToEnum# @ Bool (># ww_sykb ww4_agN3)
                        of _ [Occ=Dead] {
                          False ->
                            case newByteArray#
                                   @ (Control.Monad.Primitive.PrimState
                                        (ST RealWorld))
                                   (*# ww_sykb 8#)
                                   (w_syk8 `cast` ...)
                            of _ [Occ=Dead] { (# ipv4_ajsv, ipv5_ajsw #) ->
                            letrec {
                              $s$wfoldlM'_loop_sEtm [Occ=LoopBreaker]
                                :: State# RealWorld
                                   -> s50_ah3m
                                   -> Int#
                                   -> (# State# RealWorld, Int #)

                              $s$wfoldlM'_loop_sEtm =
                                \ (sc_sEtl [OS=OneShot] :: State# RealWorld)
                                  (sc1_sEtj :: s50_ah3m)
                                  (sc2_sEti :: Int#) ->
                                  case (vstep_ah3p sc1_sEtj) `cast` ... of _ [Occ=Dead] {
                                    Data.Vector.Fusion.Stream.Monadic.Yield x_ah4g s'_ah4h ->
                                      case x_ah4g
                                      of _ [Occ=Dead]
                                      { Data.Vector.Fusion.Bundle.Monadic.Chunk m1_ah9m f1_ah9n ->
                                      case ((f1_ah9n
                                               @ (ST RealWorld)
                                               (Control.Monad.Primitive.$fPrimMonadST
                                                  @ RealWorld)
                                               Data.Vector.Unboxed.Base.$fVectorVectorInt
                                               (case m1_ah9m
                                                of _ [Occ=Dead] { I# dt4_ajtM ->
                                                (Data.Vector.Primitive.Mutable.MVector
                                                   @ (Control.Monad.Primitive.PrimState
                                                        (ST RealWorld))
                                                   @ Int
                                                   sc2_sEti
                                                   dt4_ajtM
                                                   ipv5_ajsw)
                                                `cast` ...
                                                }))
                                            `cast` ...)
                                             sc_sEtl
                                      of _ [Occ=Dead] { (# ipv6_ah9r, ipv7_ah9s #) ->
                                      case m1_ah9m of _ [Occ=Dead] { I# y_ah9A ->
                                      $s$wfoldlM'_loop_sEtm
                                        ipv6_ah9r s'_ah4h (+# sc2_sEti y_ah9A)
                                      }
                                      }
                                      };
                                    Data.Vector.Fusion.Stream.Monadic.Skip s'_ah4p ->
                                      $s$wfoldlM'_loop_sEtm sc_sEtl s'_ah4p sc2_sEti;
                                    Data.Vector.Fusion.Stream.Monadic.Done ->
                                      (# sc_sEtl, I# sc2_sEti #)
                                  }; } in
                            case $s$wfoldlM'_loop_sEtm (ipv4_ajsv `cast` ...) t_ah3q 0#
                            of _ [Occ=Dead] { (# ipv6_ah4v, ipv7_ah4w #) ->
                            (# ipv6_ah4v,
                               case ipv7_ah4w of _ [Occ=Dead] { I# dt4_ajtM ->
                               (Data.Vector.Primitive.Mutable.MVector
                                  @ (Control.Monad.Primitive.PrimState
                                       (ST RealWorld))
                                  @ Int
                                  0#
                                  dt4_ajtM
                                  ipv5_ajsw)
                               `cast` ...
                               } #)
                            }
                            };
                          True -> case lvl16_rIqF ww_sykb of wild5_00 { }
                        }
                        };
                      True -> case lvl22_rIqL ww_sykb of wild4_00 { }
                    } } in
              case ipv3_ah3b of _ [Occ=Dead] {
                Data.Vector.Fusion.Bundle.Size.Exact n_ah4A ->
                  case n_ah4A of _ [Occ=Dead] { I# ww1_sykb ->
                  case ($w$j_sykd ww1_sykb s2_ah7U) `cast` ...
                  of _ [Occ=Dead] { (# ipv4_ah7Z, ipv5_ah80 #) ->
                  case ipv5_ah80 `cast` ...
                  of _ [Occ=Dead]
                  { Data.Vector.Primitive.Mutable.MVector dt_ajqR dt1_ajqS
                                                          dt2_ajqT ->
                  case unsafeFreezeByteArray#
                         @ (Control.Monad.Primitive.PrimState
                              (ST RealWorld))
                         dt2_ajqT
                         (ipv4_ah7Z `cast` ...)
                  of _ [Occ=Dead] { (# ipv6_ajqY, ipv7_ajqZ #) ->
                  (# ipv6_ajqY `cast` ...,
                     (Data.Vector.Primitive.Vector @ Int dt_ajqR dt1_ajqS ipv7_ajqZ)
                     `cast` ... #)
                  }
                  }
                  }
                  };
                Data.Vector.Fusion.Bundle.Size.Max n_ah4D ->
                  case n_ah4D of _ [Occ=Dead] { I# ww1_sykb ->
                  case ($w$j_sykd ww1_sykb s2_ah7U) `cast` ...
                  of _ [Occ=Dead] { (# ipv4_ah7Z, ipv5_ah80 #) ->
                  case ipv5_ah80 `cast` ...
                  of _ [Occ=Dead]
                  { Data.Vector.Primitive.Mutable.MVector dt_ajqR dt1_ajqS
                                                          dt2_ajqT ->
                  case unsafeFreezeByteArray#
                         @ (Control.Monad.Primitive.PrimState
                              (ST RealWorld))
                         dt2_ajqT
                         (ipv4_ah7Z `cast` ...)
                  of _ [Occ=Dead] { (# ipv6_ajqY, ipv7_ajqZ #) ->
                  (# ipv6_ajqY `cast` ...,
                     (Data.Vector.Primitive.Vector @ Int dt_ajqR dt1_ajqS ipv7_ajqZ)
                     `cast` ... #)
                  }
                  }
                  }
                  };
                Data.Vector.Fusion.Bundle.Size.Unknown ->
                  case divInt# 9223372036854775807# 8#
                  of ww4_agN3 { __DEFAULT ->
                  case tagToEnum# @ Bool (># 0# ww4_agN3)
                  of _ [Occ=Dead] {
                    False ->
                      case newByteArray#
                             @ (Control.Monad.Primitive.PrimState
                                  (ST RealWorld))
                             0#
                             (s2_ah7U `cast` ...)
                      of _ [Occ=Dead] { (# ipv4_ajsv, ipv5_ajsw #) ->
                      letrec {
                        $sfoldlM_loop_sEtR [Occ=LoopBreaker]
                          :: State# RealWorld
                             -> s50_ah3m
                             -> Int#
                             -> Int#
                             -> MutableByteArray#
                                  (Control.Monad.Primitive.PrimState (ST RealWorld))
                             -> (Data.Vector.Primitive.Mutable.MVector
                                   (Control.Monad.Primitive.PrimState
                                      (ST RealWorld))
                                   Int :: *)
                                ~R#
                                (G.Mutable
                                   Data.Vector.Unboxed.Base.Vector
                                   (Control.Monad.Primitive.PrimState
                                      (ST RealWorld))
                                   Int :: *) =>
                                Int#
                                -> (# State# RealWorld,
                                      (G.Mutable
                                         Data.Vector.Unboxed.Base.Vector
                                         (Control.Monad.Primitive.PrimState
                                            (ST RealWorld))
                                         Int,
                                       Int) #)

                        $sfoldlM_loop_sEtR =
                          \ (sc_sEtQ [OS=OneShot] :: State# RealWorld)
                            (sc1_sEtO :: s50_ah3m)
                            (sc2_sEtJ :: Int#)
                            (sc3_sEtK :: Int#)
                            (sc4_sEtL
                               :: MutableByteArray#
                                    (Control.Monad.Primitive.PrimState
                                       (ST RealWorld)))
                            (sg0_sEtM
                               :: (Data.Vector.Primitive.Mutable.MVector
                                     (Control.Monad.Primitive.PrimState
                                        (ST RealWorld))
                                     Int :: *)
                                  ~R#
                                  (G.Mutable
                                     Data.Vector.Unboxed.Base.Vector
                                     (Control.Monad.Primitive.PrimState
                                        (ST RealWorld))
                                     Int :: *))
                            (sc5_sEtN :: Int#) ->
                            case (vstep_ah3p sc1_sEtO) `cast` ... of _ [Occ=Dead] {
                              Data.Vector.Fusion.Stream.Monadic.Yield x_ah7q s'_ah7r ->
                                case x_ah7q
                                of _ [Occ=Dead]
                                { Data.Vector.Fusion.Bundle.Monadic.Chunk n_ah8p f1_ah8q ->
                                case n_ah8p of _ [Occ=Dead] { I# y_ah8y ->
                                let {
                                  y1_ah8G [Dmd=<S,U>] :: Int#

                                  y1_ah8G = +# sc5_sEtN y_ah8y } in
                                let {
                                  $s$j_sEu7
                                    :: Int#
                                       -> Int#
                                       -> MutableByteArray#
                                            (Control.Monad.Primitive.PrimState
                                               (ST RealWorld))
                                       -> (Data.Vector.Primitive.Mutable.MVector
                                             (Control.Monad.Primitive.PrimState
                                                (ST RealWorld))
                                             Int :: *)
                                          ~R#
                                          (G.Mutable
                                             Data.Vector.Unboxed.Base.Vector
                                             (Control.Monad.Primitive.PrimState
                                                (ST RealWorld))
                                             Int :: *) =>
                                          State# RealWorld
                                          -> (# State# RealWorld,
                                                (G.Mutable
                                                   Data.Vector.Unboxed.Base.Vector
                                                   (Control.Monad.Primitive.PrimState
                                                      (ST RealWorld))
                                                   Int,
                                                 Int) #)

                                  $s$j_sEu7 =
                                    \ (sc6_sEtX :: Int#)
                                      (sc7_sEtY :: Int#)
                                      (sc8_sEtZ
                                         :: MutableByteArray#
                                              (Control.Monad.Primitive.PrimState
                                                 (ST RealWorld)))
                                      (sg1_sEu0
                                         :: (Data.Vector.Primitive.Mutable.MVector
                                               (Control.Monad.Primitive.PrimState
                                                  (ST RealWorld))
                                               Int :: *)
                                            ~R#
                                            (G.Mutable
                                               Data.Vector.Unboxed.Base.Vector
                                               (Control.Monad.Primitive.PrimState
                                                  (ST RealWorld))
                                               Int :: *))
                                      (sc9_sEtW [OS=OneShot]
                                         :: State# RealWorld) ->
                                      case ((f1_ah8q
                                               @ (ST RealWorld)
                                               (Control.Monad.Primitive.$fPrimMonadST
                                                  @ RealWorld)
                                               Data.Vector.Unboxed.Base.$fVectorVectorInt
                                               ((Data.Vector.Primitive.Mutable.MVector
                                                   @ (Control.Monad.Primitive.PrimState
                                                        (ST RealWorld))
                                                   @ Int
                                                   (+# sc6_sEtX sc5_sEtN)
                                                   y_ah8y
                                                   sc8_sEtZ)
                                                `cast` ...))
                                            `cast` ...)
                                             sc9_sEtW
                                      of _ [Occ=Dead] { (# ipv6_asFE, ipv7_asFF #) ->
                                      $sfoldlM_loop_sEtR
                                        ipv6_asFE s'_ah7r sc6_sEtX sc7_sEtY sc8_sEtZ @~ ... y1_ah8G
                                      } } in
                                case tagToEnum# @ Bool (<# sc3_sEtK y1_ah8G)
                                of _ [Occ=Dead] {
                                  False -> $s$j_sEu7 sc2_sEtJ sc3_sEtK sc4_sEtL @~ ... sc_sEtQ;
                                  True ->
                                    let {
                                      $j_su5B
                                        :: Int#
                                           -> (# State# RealWorld,
                                                 (G.Mutable
                                                    Data.Vector.Unboxed.Base.Vector
                                                    (Control.Monad.Primitive.PrimState
                                                       (ST RealWorld))
                                                    Int,
                                                  Int) #)

                                      $j_su5B =
                                        \ (x1_agLr [OS=OneShot] :: Int#) ->
                                          case tagToEnum# @ Bool (<# x1_agLr 0#)
                                          of _ [Occ=Dead] {
                                            False ->
                                              case tagToEnum#
                                                     @ Bool (># x1_agLr ww4_agN3)
                                              of _ [Occ=Dead] {
                                                False ->
                                                  case newByteArray#
                                                         @ (Control.Monad.Primitive.PrimState
                                                              (ST RealWorld))
                                                         (*# x1_agLr 8#)
                                                         (sc_sEtQ `cast` ...)
                                                  of _ [Occ=Dead] { (# ipv6_at8q, ipv7_at8r #) ->
                                                  case copyMutableByteArray#
                                                         @ (Control.Monad.Primitive.PrimState
                                                              (ST RealWorld))
                                                         sc4_sEtL
                                                         (*# sc2_sEtJ 8#)
                                                         ipv7_at8r
                                                         0#
                                                         (*# sc3_sEtK 8#)
                                                         ipv6_at8q
                                                  of s'#_at97 [OS=OneShot] { __DEFAULT ->
                                                  $s$j_sEu7
                                                    0#
                                                    x1_agLr
                                                    ipv7_at8r
                                                    @~ ...
                                                    (s'#_at97 `cast` ...)
                                                  }
                                                  };
                                                True -> case lvl16_rIqF x1_agLr of wild12_00 { }
                                              };
                                            True -> case lvl22_rIqL x1_agLr of wild11_00 { }
                                          } } in
                                    case tagToEnum# @ Bool (<=# sc3_sEtK 1#)
                                    of _ [Occ=Dead] {
                                      False ->
                                        let {
                                          y2_ah8W [Dmd=<S,U>] :: Int#

                                          y2_ah8W = -# y1_ah8G sc3_sEtK } in
                                        case tagToEnum#
                                               @ Bool (<=# sc3_sEtK y2_ah8W)
                                        of _ [Occ=Dead] {
                                          False -> $j_su5B (+# sc3_sEtK sc3_sEtK);
                                          True -> $j_su5B (+# sc3_sEtK y2_ah8W)
                                        };
                                      True ->
                                        let {
                                          y2_ah98 [Dmd=<S,U>] :: Int#

                                          y2_ah98 = -# y1_ah8G sc3_sEtK } in
                                        case tagToEnum# @ Bool (<=# 1# y2_ah98)
                                        of _ [Occ=Dead] {
                                          False -> $j_su5B (+# sc3_sEtK 1#);
                                          True -> $j_su5B (+# sc3_sEtK y2_ah98)
                                        }
                                    }
                                }
                                }
                                };
                              Data.Vector.Fusion.Stream.Monadic.Skip s'_ah7z ->
                                $sfoldlM_loop_sEtR
                                  sc_sEtQ s'_ah7z sc2_sEtJ sc3_sEtK sc4_sEtL @~ ... sc5_sEtN;
                              Data.Vector.Fusion.Stream.Monadic.Done ->
                                (# sc_sEtQ,
                                   ((Data.Vector.Primitive.Mutable.MVector
                                       @ (Control.Monad.Primitive.PrimState
                                            (ST RealWorld))
                                       @ Int
                                       sc2_sEtJ
                                       sc3_sEtK
                                       sc4_sEtL)
                                    `cast` ...,
                                    I# sc5_sEtN) #)
                            }; } in
                      case $sfoldlM_loop_sEtR
                             (ipv4_ajsv `cast` ...) t_ah3q 0# 0# ipv5_ajsw @~ ... 0#
                      of _ [Occ=Dead] { (# ipv6_ah7F, ipv7_ah7G #) ->
                      case ipv7_ah7G of _ [Occ=Dead] { (v'_ah7K, n_ah7L) ->
                      case v'_ah7K `cast` ...
                      of _ [Occ=Dead]
                      { Data.Vector.Primitive.Mutable.MVector dt_ajtC dt1_ajtD
                                                              dt2_ajtE ->
                      case n_ah7L of _ [Occ=Dead] { I# dt4_ajtM ->
                      case unsafeFreezeByteArray#
                             @ (Control.Monad.Primitive.PrimState
                                  (ST RealWorld))
                             dt2_ajtE
                             (ipv6_ah7F `cast` ...)
                      of _ [Occ=Dead] { (# ipv9_ajqY, ipv10_ajqZ #) ->
                      (# ipv9_ajqY `cast` ...,
                         (Data.Vector.Primitive.Vector @ Int dt_ajtC dt4_ajtM ipv10_ajqZ)
                         `cast` ... #)
                      }
                      }
                      }
                      }
                      }
                      };
                    True -> case lvl59_rIrt of wild_00 { }
                  }
                  }
              }
              }
              })
    of _ [Occ=Dead] { (# ipv4_ah84, ipv5_ah85 #) ->
    ipv5_ah85
    }
    }

-- RHS size: {terms: 78, types: 106, coercions: 19}
$sreplicate [InlPrag=INLINE (sat-args=2)]
  :: Int -> Int -> Data.Vector.Unboxed.Base.Vector Int
[GblId,
 Arity=2,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=2,unsat_ok=False,boring_ok=False)
         Tmpl= \ (n_agHI [Occ=Once] :: Int) (x_agHJ :: Int) ->
                 $ @ 'PtrRepLifted
                   @ (Data.Vector.Unboxed.Base.Vector Int)
                   @ (Data.Vector.Unboxed.Base.Vector Int)
                   (seq @ Int @ (Data.Vector.Unboxed.Base.Vector Int) x_agHJ)
                   ($ @ 'PtrRepLifted
                      @ (Data.Vector.Fusion.Bundle.Bundle
                           Data.Vector.Unboxed.Base.Vector Int)
                      @ (Data.Vector.Unboxed.Base.Vector Int)
                      $sreplicate_$sunstream
                      (Data.Vector.Fusion.Bundle.Monadic.replicate
                         @ Data.Vector.Fusion.Util.Id
                         @ Int
                         @ Data.Vector.Unboxed.Base.Vector
                         Data.Vector.Fusion.Util.$fMonadId
                         n_agHI
                         x_agHJ))}]
$sreplicate =
  \ (n_agHI :: Int) (x_agHJ :: Int) ->
    case x_agHJ of _ [Occ=Dead] { I# ipv_stat ->
    case runRW#
           @ 'PtrRepLifted
           @ (Data.Vector.Unboxed.Base.Vector Int)
           (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
              case n_agHI of _ [Occ=Dead] { I# x1_ai6o ->
              let {
                $j_suS9
                  :: Int#
                     -> (# State# RealWorld,
                           Data.Vector.Unboxed.Base.Vector Int #)

                $j_suS9 =
                  \ (x2_agLr [OS=OneShot] :: Int#) ->
                    case tagToEnum# @ Bool (<# x2_agLr 0#)
                    of _ [Occ=Dead] {
                      False ->
                        case divInt# 9223372036854775807# 8#
                        of ww4_agN3 { __DEFAULT ->
                        case tagToEnum# @ Bool (># x2_agLr ww4_agN3)
                        of _ [Occ=Dead] {
                          False ->
                            case newByteArray#
                                   @ (Control.Monad.Primitive.PrimState
                                        (ST RealWorld))
                                   (*# x2_agLr 8#)
                                   (s1_ah7U `cast` ...)
                            of _ [Occ=Dead] { (# ipv1_ajsv, ipv2_ajsw #) ->
                            case {__pkg_ccall primitive-0.6.1.0 hsprimitive_memset_Word forall s.
                                                       MutableByteArray# s
                                                       -> Int#
                                                       -> Word#
                                                       -> Int#
                                                       -> State# RealWorld
                                                       -> (# State# RealWorld #)}_au78
                                   @ (Control.Monad.Primitive.PrimState
                                        (ST RealWorld))
                                   ipv2_ajsw
                                   0#
                                   (int2Word# x2_agLr)
                                   ipv_stat
                                   (ipv1_ajsv `cast` ...)
                            of _ [Occ=Dead] { (# ds8_au7d #) ->
                            case unsafeFreezeByteArray#
                                   @ (Control.Monad.Primitive.PrimState
                                        (ST RealWorld))
                                   ipv2_ajsw
                                   (ds8_au7d `cast` ...)
                            of _ [Occ=Dead] { (# ipv3_ajqY, ipv4_ajqZ #) ->
                            (# ipv3_ajqY `cast` ...,
                               (Data.Vector.Primitive.Vector @ Int 0# x2_agLr ipv4_ajqZ)
                               `cast` ... #)
                            }
                            }
                            };
                          True -> case lvl16_rIqF x2_agLr of wild3_00 { }
                        }
                        };
                      True -> case lvl22_rIqL x2_agLr of wild2_00 { }
                    } } in
              case tagToEnum# @ Bool (<=# x1_ai6o 0#)
              of _ [Occ=Dead] {
                False -> $j_suS9 x1_ai6o;
                True -> $j_suS9 0#
              }
              })
    of _ [Occ=Dead] { (# ipv1_ah84, ipv2_ah85 #) ->
    ipv2_ah85
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 0}
sizeCInt :: Int
[GblId,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= Foreign.Storable.$fStorableBool7}]
sizeCInt = Foreign.Storable.$fStorableBool7

-- RHS size: {terms: 1, types: 0, coercions: 0}
sizePtr :: Int
[GblId,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= Foreign.Storable.$fStorableDouble5}]
sizePtr = Foreign.Storable.$fStorableDouble5

-- RHS size: {terms: 4, types: 6, coercions: 0}
$fStorableArrayType_$calignment
  :: forall a_afiL. Storable a_afiL => ArrayType a_afiL -> Int
[GblId,
 Arity=2,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=2,unsat_ok=True,boring_ok=True)
         Tmpl= \ (@ a_afiL) _ [Occ=Dead] _ [Occ=Dead] ->
                 Foreign.Storable.$fStorableDouble5}]
$fStorableArrayType_$calignment =
  \ (@ a_afiL) _ [Occ=Dead] _ [Occ=Dead] ->
    Foreign.Storable.$fStorableDouble5

-- RHS size: {terms: 1, types: 0, coercions: 0}
alignPtr_ :: Int
[GblId,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= Foreign.Storable.$fStorableDouble5}]
alignPtr_ = Foreign.Storable.$fStorableDouble5

-- RHS size: {terms: 11, types: 1, coercions: 0}
$fStorableArrayType7 :: Int
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 52 20}]
$fStorableArrayType7 =
  case divInt# 4# -8# of ww4_agN3 { __DEFAULT ->
  I# (+# (*# ww4_agN3 -8#) 8#)
  }

-- RHS size: {terms: 4, types: 6, coercions: 0}
$fStorableArrayType_$csizeOf
  :: forall a_afiL. Storable a_afiL => ArrayType a_afiL -> Int
[GblId,
 Arity=2,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=2,unsat_ok=True,boring_ok=True)
         Tmpl= \ (@ a_afiL) _ [Occ=Dead] _ [Occ=Dead] ->
                 $fStorableArrayType7}]
$fStorableArrayType_$csizeOf =
  \ (@ a_afiL) _ [Occ=Dead] _ [Occ=Dead] -> $fStorableArrayType7

-- RHS size: {terms: 9, types: 1, coercions: 0}
$fStorableArrayType2 :: Int
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 51 20}]
$fStorableArrayType2 =
  case divInt# 4# -8# of ww4_agN3 { __DEFAULT ->
  I# (*# ww4_agN3 -8#)
  }

-- RHS size: {terms: 39, types: 58, coercions: 0}
$fStorableArrayType3
  :: forall a_afiL.
     Storable a_afiL =>
     Ptr (ArrayType a_afiL)
     -> State# RealWorld
     -> (# State# RealWorld, ArrayType a_afiL #)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_afiL)
                 _ [Occ=Dead]
                 (ptr_ac6E [Occ=Once!] :: Ptr (ArrayType a_afiL))
                 (eta_B1 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case ptr_ac6E of _ [Occ=Dead] { Ptr addr_ajvU ->
                 case readIntOffAddr#
                        @ RealWorld addr_ajvU 0# eta_B1
                 of _ [Occ=Dead] { (# ipv_ajw2 [Occ=Once], ipv1_ajw3 #) ->
                 case $fStorableArrayType2
                 of _ [Occ=Dead] { I# d_ajwi [Occ=Once] ->
                 case readAddrOffAddr#
                        @ RealWorld
                        (plusAddr# addr_ajvU d_ajwi)
                        0#
                        ipv_ajw2
                 of _ [Occ=Dead]
                 { (# ipv2_ajwm [Occ=Once], ipv3_ajwn [Occ=Once] #) ->
                 case newMutVar#
                        @ Finalizers
                        @ RealWorld
                        NoFinalizers
                        ipv2_ajwm
                 of _ [Occ=Dead]
                 { (# ipv4_ajwH [Occ=Once], ipv5_ajwI [Occ=Once] #) ->
                 (# ipv4_ajwH,
                    ArrayType
                      @ a_afiL
                      (I# ipv1_ajw3)
                      (Data.Vector.Storable.Vector
                         @ a_afiL
                         ipv1_ajw3
                         ipv3_ajwn
                         (PlainForeignPtr ipv5_ajwI)) #)
                 }
                 }
                 }
                 }
                 }}]
$fStorableArrayType3 =
  \ (@ a_afiL)
    _ [Occ=Dead]
    (ptr_ac6E :: Ptr (ArrayType a_afiL))
    (eta_B1 [OS=OneShot] :: State# RealWorld) ->
    case ptr_ac6E of _ [Occ=Dead] { Ptr addr_ajvU ->
    case readIntOffAddr#
           @ RealWorld addr_ajvU 0# eta_B1
    of _ [Occ=Dead] { (# ipv_ajw2, ipv1_ajw3 #) ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d_ajwi ->
    case readAddrOffAddr#
           @ RealWorld
           (plusAddr# addr_ajvU d_ajwi)
           0#
           ipv_ajw2
    of _ [Occ=Dead] { (# ipv2_ajwm, ipv3_ajwn #) ->
    case newMutVar#
           @ Finalizers
           @ RealWorld
           NoFinalizers
           ipv2_ajwm
    of _ [Occ=Dead] { (# ipv4_ajwH, ipv5_ajwI #) ->
    (# ipv4_ajwH,
       ArrayType
         @ a_afiL
         (I# ipv1_ajw3)
         (Data.Vector.Storable.Vector
            @ a_afiL
            ipv1_ajw3
            ipv3_ajwn
            (PlainForeignPtr ipv5_ajwI)) #)
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 14}
$fStorableArrayType_$cpeek
  :: forall a_afiL.
     Storable a_afiL =>
     Ptr (ArrayType a_afiL) -> IO (ArrayType a_afiL)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= $fStorableArrayType3 `cast` ...}]
$fStorableArrayType_$cpeek =
  $fStorableArrayType3 `cast` ...

-- RHS size: {terms: 48, types: 62, coercions: 0}
$fStorableArrayType16
  :: forall a_XfkK.
     Storable a_XfkK =>
     forall b_Xh5L.
     Ptr b_Xh5L
     -> Int
     -> State# RealWorld
     -> (# State# RealWorld, ArrayType a_XfkK #)
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_XfkK)
                 _ [Occ=Dead]
                 (@ b1_Xh5L)
                 (ptr_Xh5N [Occ=Once!] :: Ptr b1_Xh5L)
                 (off_Xh5P [Occ=Once!] :: Int)
                 (eta_B1 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case ptr_Xh5N of _ [Occ=Dead] { Ptr addr_ahx6 [Occ=Once] ->
                 case off_Xh5P of _ [Occ=Dead] { I# d_ahxa [Occ=Once] ->
                 let {
                   addr1_ajvU :: Addr#

                   addr1_ajvU = plusAddr# addr_ahx6 d_ahxa } in
                 case readIntOffAddr#
                        @ RealWorld addr1_ajvU 0# eta_B1
                 of _ [Occ=Dead] { (# ipv_ajw2 [Occ=Once], ipv1_ajw3 #) ->
                 case $fStorableArrayType2
                 of _ [Occ=Dead] { I# d1_ajwi [Occ=Once] ->
                 case readAddrOffAddr#
                        @ RealWorld
                        (plusAddr# addr1_ajvU d1_ajwi)
                        0#
                        ipv_ajw2
                 of _ [Occ=Dead]
                 { (# ipv2_ajwm [Occ=Once], ipv3_ajwn [Occ=Once] #) ->
                 case newMutVar#
                        @ Finalizers
                        @ RealWorld
                        NoFinalizers
                        ipv2_ajwm
                 of _ [Occ=Dead]
                 { (# ipv4_ajwH [Occ=Once], ipv5_ajwI [Occ=Once] #) ->
                 (# ipv4_ajwH,
                    ArrayType
                      @ a_XfkK
                      (I# ipv1_ajw3)
                      (Data.Vector.Storable.Vector
                         @ a_XfkK
                         ipv1_ajw3
                         ipv3_ajwn
                         (PlainForeignPtr ipv5_ajwI)) #)
                 }
                 }
                 }
                 }
                 }
                 }}]
$fStorableArrayType16 =
  \ (@ a_XfkK)
    _ [Occ=Dead]
    (@ b1_Xh5L)
    (ptr_Xh5N :: Ptr b1_Xh5L)
    (off_Xh5P :: Int)
    (eta_B1 [OS=OneShot] :: State# RealWorld) ->
    case ptr_Xh5N of _ [Occ=Dead] { Ptr addr_ahx6 ->
    case off_Xh5P of _ [Occ=Dead] { I# d_ahxa ->
    let {
      addr1_ajvU [Dmd=<S,U>] :: Addr#

      addr1_ajvU = plusAddr# addr_ahx6 d_ahxa } in
    case readIntOffAddr#
           @ RealWorld addr1_ajvU 0# eta_B1
    of _ [Occ=Dead] { (# ipv_ajw2, ipv1_ajw3 #) ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d1_ajwi ->
    case readAddrOffAddr#
           @ RealWorld
           (plusAddr# addr1_ajvU d1_ajwi)
           0#
           ipv_ajw2
    of _ [Occ=Dead] { (# ipv2_ajwm, ipv3_ajwn #) ->
    case newMutVar#
           @ Finalizers
           @ RealWorld
           NoFinalizers
           ipv2_ajwm
    of _ [Occ=Dead] { (# ipv4_ajwH, ipv5_ajwI #) ->
    (# ipv4_ajwH,
       ArrayType
         @ a_XfkK
         (I# ipv1_ajw3)
         (Data.Vector.Storable.Vector
            @ a_XfkK
            ipv1_ajw3
            ipv3_ajwn
            (PlainForeignPtr ipv5_ajwI)) #)
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 18}
$fStorableArrayType_$cpeekByteOff
  :: forall a_afiL.
     Storable a_afiL =>
     forall b_achZ. Ptr b_achZ -> Int -> IO (ArrayType a_afiL)
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= $fStorableArrayType16 `cast` ...}]
$fStorableArrayType_$cpeekByteOff =
  $fStorableArrayType16 `cast` ...

-- RHS size: {terms: 52, types: 64, coercions: 0}
$fStorableArrayType18
  :: forall a_XfkI.
     Storable a_XfkI =>
     Ptr (ArrayType a_XfkI)
     -> Int
     -> State# RealWorld
     -> (# State# RealWorld, ArrayType a_XfkI #)
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_XfkI)
                 _ [Occ=Dead]
                 (ptr_XhfC [Occ=Once!] :: Ptr (ArrayType a_XfkI))
                 (off_XhfF [Occ=Once!] :: Int)
                 (eta_B1 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case ptr_XhfC of _ [Occ=Dead] { Ptr addr_ahx6 [Occ=Once] ->
                 case off_XhfF of _ [Occ=Dead] { I# x_agN8 [Occ=Once] ->
                 case $fStorableArrayType7
                 of _ [Occ=Dead] { I# y_agNc [Occ=Once] ->
                 let {
                   addr1_ajvU :: Addr#

                   addr1_ajvU =
                     plusAddr# addr_ahx6 (*# x_agN8 y_agNc) } in
                 case readIntOffAddr#
                        @ RealWorld addr1_ajvU 0# eta_B1
                 of _ [Occ=Dead] { (# ipv_ajw2 [Occ=Once], ipv1_ajw3 #) ->
                 case $fStorableArrayType2
                 of _ [Occ=Dead] { I# d_ajwi [Occ=Once] ->
                 case readAddrOffAddr#
                        @ RealWorld
                        (plusAddr# addr1_ajvU d_ajwi)
                        0#
                        ipv_ajw2
                 of _ [Occ=Dead]
                 { (# ipv2_ajwm [Occ=Once], ipv3_ajwn [Occ=Once] #) ->
                 case newMutVar#
                        @ Finalizers
                        @ RealWorld
                        NoFinalizers
                        ipv2_ajwm
                 of _ [Occ=Dead]
                 { (# ipv4_ajwH [Occ=Once], ipv5_ajwI [Occ=Once] #) ->
                 (# ipv4_ajwH,
                    ArrayType
                      @ a_XfkI
                      (I# ipv1_ajw3)
                      (Data.Vector.Storable.Vector
                         @ a_XfkI
                         ipv1_ajw3
                         ipv3_ajwn
                         (PlainForeignPtr ipv5_ajwI)) #)
                 }
                 }
                 }
                 }
                 }
                 }
                 }}]
$fStorableArrayType18 =
  \ (@ a_XfkI)
    _ [Occ=Dead]
    (ptr_XhfC :: Ptr (ArrayType a_XfkI))
    (off_XhfF :: Int)
    (eta_B1 [OS=OneShot] :: State# RealWorld) ->
    case ptr_XhfC of _ [Occ=Dead] { Ptr addr_ahx6 ->
    case off_XhfF of _ [Occ=Dead] { I# x_agN8 ->
    case $fStorableArrayType7
    of _ [Occ=Dead] { I# y_agNc ->
    let {
      addr1_ajvU [Dmd=<S,U>] :: Addr#

      addr1_ajvU =
        plusAddr# addr_ahx6 (*# x_agN8 y_agNc) } in
    case readIntOffAddr#
           @ RealWorld addr1_ajvU 0# eta_B1
    of _ [Occ=Dead] { (# ipv_ajw2, ipv1_ajw3 #) ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d_ajwi ->
    case readAddrOffAddr#
           @ RealWorld
           (plusAddr# addr1_ajvU d_ajwi)
           0#
           ipv_ajw2
    of _ [Occ=Dead] { (# ipv2_ajwm, ipv3_ajwn #) ->
    case newMutVar#
           @ Finalizers
           @ RealWorld
           NoFinalizers
           ipv2_ajwm
    of _ [Occ=Dead] { (# ipv4_ajwH, ipv5_ajwI #) ->
    (# ipv4_ajwH,
       ArrayType
         @ a_XfkI
         (I# ipv1_ajw3)
         (Data.Vector.Storable.Vector
            @ a_XfkI
            ipv1_ajw3
            ipv3_ajwn
            (PlainForeignPtr ipv5_ajwI)) #)
    }
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 16}
$fStorableArrayType_$cpeekElemOff
  :: forall a_afiL.
     Storable a_afiL =>
     Ptr (ArrayType a_afiL) -> Int -> IO (ArrayType a_afiL)
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= $fStorableArrayType18 `cast` ...}]
$fStorableArrayType_$cpeekElemOff =
  $fStorableArrayType18 `cast` ...

-- RHS size: {terms: 44, types: 44, coercions: 0}
$fStorableArrayType1
  :: forall a_XfkB.
     Storable a_XfkB =>
     Ptr (ArrayType a_XfkB)
     -> ArrayType a_XfkB
     -> State# RealWorld
     -> (# State# RealWorld, () #)
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_XfkB)
                 _ [Occ=Dead]
                 (ptr_aca0 [Occ=Once!] :: Ptr (ArrayType a_XfkB))
                 (ds_dfET [Occ=Once!] :: ArrayType a_XfkB)
                 (eta_B1 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case ds_dfET
                 of _ [Occ=Dead]
                 { ArrayType n_aca1 [Occ=Once!] x_aca2 [Occ=Once!] ->
                 case ptr_aca0 of _ [Occ=Dead] { Ptr addr_ajxS ->
                 case n_aca1 of _ [Occ=Dead] { I# x1_ajy0 [Occ=Once] ->
                 case writeIntOffAddr#
                        @ RealWorld addr_ajxS 0# x1_ajy0 eta_B1
                 of s2_ajy2 [OS=OneShot] { __DEFAULT ->
                 case x_aca2
                 of _ [Occ=Dead]
                 { Data.Vector.Storable.Vector _ [Occ=Dead] dt1_agFy [Occ=Once]
                                               dt2_agFz [Occ=Once] ->
                 case $fStorableArrayType2
                 of _ [Occ=Dead] { I# d_ajyi [Occ=Once] ->
                 case writeAddrOffAddr#
                        @ RealWorld
                        (plusAddr# addr_ajxS d_ajyi)
                        0#
                        dt1_agFy
                        s2_ajy2
                 of s1_ajyo [OS=OneShot] { __DEFAULT ->
                 case touch#
                        @ 'PtrRepLifted
                        @ ForeignPtrContents
                        dt2_agFz
                        s1_ajyo
                 of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                 (# s'_ahW8, () #)
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 }}]
$fStorableArrayType1 =
  \ (@ a_XfkB)
    _ [Occ=Dead]
    (ptr_aca0 :: Ptr (ArrayType a_XfkB))
    (ds_dfET :: ArrayType a_XfkB)
    (eta_B1 [OS=OneShot] :: State# RealWorld) ->
    case ds_dfET of _ [Occ=Dead] { ArrayType n_aca1 x_aca2 ->
    case ptr_aca0 of _ [Occ=Dead] { Ptr addr_ajxS ->
    case n_aca1 of _ [Occ=Dead] { I# x1_ajy0 ->
    case writeIntOffAddr#
           @ RealWorld addr_ajxS 0# x1_ajy0 eta_B1
    of s2_ajy2 [OS=OneShot] { __DEFAULT ->
    case x_aca2
    of _ [Occ=Dead]
    { Data.Vector.Storable.Vector dt_agFx dt1_agFy dt2_agFz ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d_ajyi ->
    case writeAddrOffAddr#
           @ RealWorld
           (plusAddr# addr_ajxS d_ajyi)
           0#
           dt1_agFy
           s2_ajy2
    of s1_ajyo [OS=OneShot] { __DEFAULT ->
    case touch#
           @ 'PtrRepLifted
           @ ForeignPtrContents
           dt2_agFz
           s1_ajyo
    of s'_ahW8 [OS=OneShot] { __DEFAULT ->
    (# s'_ahW8, () #)
    }
    }
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 16}
$fStorableArrayType_$cpoke
  :: forall a_afiL.
     Storable a_afiL =>
     Ptr (ArrayType a_afiL) -> ArrayType a_afiL -> IO ()
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= $fStorableArrayType1 `cast` ...}]
$fStorableArrayType_$cpoke =
  $fStorableArrayType1 `cast` ...

-- RHS size: {terms: 53, types: 48, coercions: 0}
$fStorableArrayType15
  :: forall a_XfkL.
     Storable a_XfkL =>
     forall b_XgMs.
     Ptr b_XgMs
     -> Int
     -> ArrayType a_XfkL
     -> State# RealWorld
     -> (# State# RealWorld, () #)
[GblId,
 Arity=5,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=5,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_XfkL)
                 _ [Occ=Dead]
                 (@ b1_XgMs)
                 (ptr_XgMu [Occ=Once!] :: Ptr b1_XgMs)
                 (off_XgMw [Occ=Once!] :: Int)
                 (eta_X6E [Occ=Once!] :: ArrayType a_XfkL)
                 (eta1_B1 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case eta_X6E
                 of _ [Occ=Dead]
                 { ArrayType n_aca1 [Occ=Once!] x_aca2 [Occ=Once!] ->
                 case ptr_XgMu of _ [Occ=Dead] { Ptr addr_ahx6 [Occ=Once] ->
                 case off_XgMw of _ [Occ=Dead] { I# d_ahxa [Occ=Once] ->
                 case n_aca1 of _ [Occ=Dead] { I# x1_ajy0 [Occ=Once] ->
                 let {
                   addr1_ajxS :: Addr#

                   addr1_ajxS = plusAddr# addr_ahx6 d_ahxa } in
                 case writeIntOffAddr#
                        @ RealWorld addr1_ajxS 0# x1_ajy0 eta1_B1
                 of s2_ajy2 [OS=OneShot] { __DEFAULT ->
                 case x_aca2
                 of _ [Occ=Dead]
                 { Data.Vector.Storable.Vector _ [Occ=Dead] dt1_agFy [Occ=Once]
                                               dt2_agFz [Occ=Once] ->
                 case $fStorableArrayType2
                 of _ [Occ=Dead] { I# d1_ajyi [Occ=Once] ->
                 case writeAddrOffAddr#
                        @ RealWorld
                        (plusAddr# addr1_ajxS d1_ajyi)
                        0#
                        dt1_agFy
                        s2_ajy2
                 of s1_ajyo [OS=OneShot] { __DEFAULT ->
                 case touch#
                        @ 'PtrRepLifted
                        @ ForeignPtrContents
                        dt2_agFz
                        s1_ajyo
                 of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                 (# s'_ahW8, () #)
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 }}]
$fStorableArrayType15 =
  \ (@ a_XfkL)
    _ [Occ=Dead]
    (@ b1_XgMs)
    (ptr_XgMu :: Ptr b1_XgMs)
    (off_XgMw :: Int)
    (eta_X6E :: ArrayType a_XfkL)
    (eta1_B1 [OS=OneShot] :: State# RealWorld) ->
    case eta_X6E of _ [Occ=Dead] { ArrayType n_aca1 x_aca2 ->
    case ptr_XgMu of _ [Occ=Dead] { Ptr addr_ahx6 ->
    case off_XgMw of _ [Occ=Dead] { I# d_ahxa ->
    case n_aca1 of _ [Occ=Dead] { I# x1_ajy0 ->
    let {
      addr1_ajxS [Dmd=<S,U>] :: Addr#

      addr1_ajxS = plusAddr# addr_ahx6 d_ahxa } in
    case writeIntOffAddr#
           @ RealWorld addr1_ajxS 0# x1_ajy0 eta1_B1
    of s2_ajy2 [OS=OneShot] { __DEFAULT ->
    case x_aca2
    of _ [Occ=Dead]
    { Data.Vector.Storable.Vector dt_agFx dt1_agFy dt2_agFz ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d1_ajyi ->
    case writeAddrOffAddr#
           @ RealWorld
           (plusAddr# addr1_ajxS d1_ajyi)
           0#
           dt1_agFy
           s2_ajy2
    of s1_ajyo [OS=OneShot] { __DEFAULT ->
    case touch#
           @ 'PtrRepLifted
           @ ForeignPtrContents
           dt2_agFz
           s1_ajyo
    of s'_ahW8 [OS=OneShot] { __DEFAULT ->
    (# s'_ahW8, () #)
    }
    }
    }
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 20}
$fStorableArrayType_$cpokeByteOff
  :: forall a_afiL.
     Storable a_afiL =>
     forall b_aci0. Ptr b_aci0 -> Int -> ArrayType a_afiL -> IO ()
[GblId,
 Arity=5,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= $fStorableArrayType15 `cast` ...}]
$fStorableArrayType_$cpokeByteOff =
  $fStorableArrayType15 `cast` ...

-- RHS size: {terms: 57, types: 50, coercions: 0}
$fStorableArrayType17
  :: forall a_XfkJ.
     Storable a_XfkJ =>
     Ptr (ArrayType a_XfkJ)
     -> Int
     -> ArrayType a_XfkJ
     -> State# RealWorld
     -> (# State# RealWorld, () #)
[GblId,
 Arity=5,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=5,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_XfkJ)
                 _ [Occ=Dead]
                 (ptr_XgNV [Occ=Once!] :: Ptr (ArrayType a_XfkJ))
                 (off_XgNX [Occ=Once!] :: Int)
                 (val_XgNZ [Occ=Once!] :: ArrayType a_XfkJ)
                 (eta_B1 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case val_XgNZ
                 of _ [Occ=Dead]
                 { ArrayType n_aca1 [Occ=Once!] x_aca2 [Occ=Once!] ->
                 case ptr_XgNV of _ [Occ=Dead] { Ptr addr_ahx6 [Occ=Once] ->
                 case off_XgNX of _ [Occ=Dead] { I# x1_agN8 [Occ=Once] ->
                 case $fStorableArrayType7
                 of _ [Occ=Dead] { I# y_agNc [Occ=Once] ->
                 case n_aca1 of _ [Occ=Dead] { I# x2_ajy0 [Occ=Once] ->
                 let {
                   addr1_ajxS :: Addr#

                   addr1_ajxS =
                     plusAddr# addr_ahx6 (*# x1_agN8 y_agNc) } in
                 case writeIntOffAddr#
                        @ RealWorld addr1_ajxS 0# x2_ajy0 eta_B1
                 of s2_ajy2 [OS=OneShot] { __DEFAULT ->
                 case x_aca2
                 of _ [Occ=Dead]
                 { Data.Vector.Storable.Vector _ [Occ=Dead] dt1_agFy [Occ=Once]
                                               dt2_agFz [Occ=Once] ->
                 case $fStorableArrayType2
                 of _ [Occ=Dead] { I# d_ajyi [Occ=Once] ->
                 case writeAddrOffAddr#
                        @ RealWorld
                        (plusAddr# addr1_ajxS d_ajyi)
                        0#
                        dt1_agFy
                        s2_ajy2
                 of s1_ajyo [OS=OneShot] { __DEFAULT ->
                 case touch#
                        @ 'PtrRepLifted
                        @ ForeignPtrContents
                        dt2_agFz
                        s1_ajyo
                 of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                 (# s'_ahW8, () #)
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 }
                 }}]
$fStorableArrayType17 =
  \ (@ a_XfkJ)
    _ [Occ=Dead]
    (ptr_XgNV :: Ptr (ArrayType a_XfkJ))
    (off_XgNX :: Int)
    (val_XgNZ :: ArrayType a_XfkJ)
    (eta_B1 [OS=OneShot] :: State# RealWorld) ->
    case val_XgNZ of _ [Occ=Dead] { ArrayType n_aca1 x_aca2 ->
    case ptr_XgNV of _ [Occ=Dead] { Ptr addr_ahx6 ->
    case off_XgNX of _ [Occ=Dead] { I# x1_agN8 ->
    case $fStorableArrayType7
    of _ [Occ=Dead] { I# y_agNc ->
    case n_aca1 of _ [Occ=Dead] { I# x2_ajy0 ->
    let {
      addr1_ajxS [Dmd=<S,U>] :: Addr#

      addr1_ajxS =
        plusAddr# addr_ahx6 (*# x1_agN8 y_agNc) } in
    case writeIntOffAddr#
           @ RealWorld addr1_ajxS 0# x2_ajy0 eta_B1
    of s2_ajy2 [OS=OneShot] { __DEFAULT ->
    case x_aca2
    of _ [Occ=Dead]
    { Data.Vector.Storable.Vector dt_agFx dt1_agFy dt2_agFz ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d_ajyi ->
    case writeAddrOffAddr#
           @ RealWorld
           (plusAddr# addr1_ajxS d_ajyi)
           0#
           dt1_agFy
           s2_ajy2
    of s1_ajyo [OS=OneShot] { __DEFAULT ->
    case touch#
           @ 'PtrRepLifted
           @ ForeignPtrContents
           dt2_agFz
           s1_ajyo
    of s'_ahW8 [OS=OneShot] { __DEFAULT ->
    (# s'_ahW8, () #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 18}
$fStorableArrayType_$cpokeElemOff
  :: forall a_afiL.
     Storable a_afiL =>
     Ptr (ArrayType a_afiL) -> Int -> ArrayType a_afiL -> IO ()
[GblId,
 Arity=5,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= $fStorableArrayType17 `cast` ...}]
$fStorableArrayType_$cpokeElemOff =
  $fStorableArrayType17 `cast` ...

-- RHS size: {terms: 72, types: 78, coercions: 2}
$wwithVector [InlPrag=[0]]
  :: forall a_adpZ b_adq0.
     Storable a_adpZ =>
     Int#
     -> Addr#
     -> ForeignPtrContents
     -> (ArrayStruct a_adpZ -> IO b_adq0)
     -> State# RealWorld
     -> (# State# RealWorld, b_adq0 #)
[GblId,
 Arity=6,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=IF_ARGS [0 0 0 0 60 0] 149 30}]
$wwithVector =
  \ (@ a_adpZ)
    (@ b1_adq0)
    (w_sylc :: Storable a_adpZ)
    (ww_syli :: Int#)
    (ww1_sylj :: Addr#)
    (ww2_sylk :: ForeignPtrContents)
    (w1_syle :: ArrayStruct a_adpZ -> IO b1_adq0)
    (w2_sylf [OS=OneShot] :: State# RealWorld) ->
    case $fStorableArrayType7
    of _ [Occ=Dead] { I# size_ak3d ->
    case newAlignedPinnedByteArray#
           @ RealWorld size_ak3d 8# w2_sylf
    of _ [Occ=Dead] { (# ipv_ak3l, ipv1_ak3m #) ->
    case unsafeFreezeByteArray#
           @ RealWorld ipv1_ak3m ipv_ak3l
    of _ [Occ=Dead] { (# ipv2_ak3r, ipv3_ak3s #) ->
    case G.length
           @ SV.Vector
           @ a_adpZ
           (Data.Vector.Storable.$fVectorVectora @ a_adpZ w_sylc)
           (Data.Vector.Storable.Vector @ a_adpZ ww_syli ww1_sylj ww2_sylk)
    of _ [Occ=Dead] { I# x_ajy0 ->
    let {
      ptr_ak3q [Dmd=<S,U>] :: Addr#

      ptr_ak3q = byteArrayContents# ipv3_ak3s } in
    case writeIntOffAddr#
           @ RealWorld ptr_ak3q 0# x_ajy0 ipv2_ak3r
    of s2_ajy2 [OS=OneShot] { __DEFAULT ->
    case $fStorableArrayType2
    of _ [Occ=Dead] { I# d_ajyi ->
    case writeAddrOffAddr#
           @ RealWorld
           (plusAddr# ptr_ak3q d_ajyi)
           0#
           ww1_sylj
           s2_ajy2
    of s1_ajyo [OS=OneShot] { __DEFAULT ->
    case touch#
           @ 'PtrRepLifted
           @ ForeignPtrContents
           ww2_sylk
           s1_ajyo
    of s'_ahW8 [OS=OneShot] { __DEFAULT ->
    case ((w1_syle (Ptr @ (ArrayType a_adpZ) ptr_ak3q))
          `cast` ...)
           s'_ahW8
    of _ [Occ=Dead] { (# ipv6_ak3C, ipv7_ak3D #) ->
    case touch#
           @ 'PtrRepUnlifted
           @ ByteArray#
           ipv3_ak3s
           ipv6_ak3C
    of s4_ak3F [OS=OneShot] { __DEFAULT ->
    (# s4_ak3F, ipv7_ak3D #)
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 16, types: 21, coercions: 0}
withVector3 [InlPrag=INLINE[0]]
  :: forall a_adpZ b_adq0.
     Storable a_adpZ =>
     SV.Vector a_adpZ
     -> (ArrayStruct a_adpZ -> IO b_adq0)
     -> State# RealWorld
     -> (# State# RealWorld, b_adq0 #)
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_adpZ)
                 (@ b1_adq0)
                 (w_sylc [Occ=Once] :: Storable a_adpZ)
                 (w1_syld [Occ=Once!] :: SV.Vector a_adpZ)
                 (w2_syle [Occ=Once] :: ArrayStruct a_adpZ -> IO b1_adq0)
                 (w3_sylf [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case w1_syld
                 of _ [Occ=Dead]
                 { Data.Vector.Storable.Vector ww1_syli [Occ=Once]
                                               ww2_sylj [Occ=Once] ww3_sylk [Occ=Once] ->
                 $wwithVector
                   @ a_adpZ
                   @ b1_adq0
                   w_sylc
                   ww1_syli
                   ww2_sylj
                   ww3_sylk
                   w2_syle
                   w3_sylf
                 }}]
withVector3 =
  \ (@ a_adpZ)
    (@ b1_adq0)
    (w_sylc :: Storable a_adpZ)
    (w1_syld :: SV.Vector a_adpZ)
    (w2_syle :: ArrayStruct a_adpZ -> IO b1_adq0)
    (w3_sylf [OS=OneShot] :: State# RealWorld) ->
    case w1_syld
    of _ [Occ=Dead]
    { Data.Vector.Storable.Vector ww1_syli ww2_sylj ww3_sylk ->
    $wwithVector
      @ a_adpZ
      @ b1_adq0
      w_sylc
      ww1_syli
      ww2_sylj
      ww3_sylk
      w2_syle
      w3_sylf
    }

-- RHS size: {terms: 1, types: 0, coercions: 20}
withVector
  :: forall a_aayk b_aayl.
     Storable a_aayk =>
     SV.Vector a_aayk -> (ArrayStruct a_aayk -> IO b_aayl) -> IO b_aayl
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= withVector3 `cast` ...}]
withVector = withVector3 `cast` ...

-- RHS size: {terms: 3, types: 4, coercions: 0}
lvl67_rIrE :: forall a_XfkH. ArrayType a_XfkH -> Int

lvl67_rIrE = \ (@ a_XfkH) _ [Occ=Dead] -> $fStorableArrayType7

-- RHS size: {terms: 3, types: 4, coercions: 0}
lvl68_rIrF :: forall a_XfkH. ArrayType a_XfkH -> Int

lvl68_rIrF =
  \ (@ a_XfkH) _ [Occ=Dead] -> Foreign.Storable.$fStorableDouble5

-- RHS size: {terms: 17, types: 14, coercions: 66}
$fStorableArrayType [InlPrag=[ALWAYS] CONLIKE]
  :: forall a_aci3. Storable a_aci3 => Storable (ArrayType a_aci3)
[GblId[DFunId],
 Arity=1,

 Unf=DFun: \ (@ a_XfkH[ssk])
             ($dStorable_Xft5 :: Storable a_XfkH[ssk]) ->
       Foreign.Storable.C:Storable TYPE: ArrayType a_XfkH[ssk]
                                   $fStorableArrayType_$csizeOf @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$calignment
                                     @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$cpeekElemOff
                                     @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$cpokeElemOff
                                     @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$cpeekByteOff
                                     @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$cpokeByteOff
                                     @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$cpeek @ a_XfkH[ssk] $dStorable_Xft5
                                   $fStorableArrayType_$cpoke @ a_XfkH[ssk] $dStorable_Xft5]
$fStorableArrayType =
  \ (@ a_XfkH) ($dStorable_Xft5 :: Storable a_XfkH) ->
    Foreign.Storable.C:Storable
      @ (ArrayType a_XfkH)
      (lvl67_rIrE @ a_XfkH)
      (lvl68_rIrF @ a_XfkH)
      (($fStorableArrayType18 @ a_XfkH $dStorable_Xft5) `cast` ...)
      (($fStorableArrayType17 @ a_XfkH $dStorable_Xft5) `cast` ...)
      (($fStorableArrayType16 @ a_XfkH $dStorable_Xft5) `cast` ...)
      (($fStorableArrayType15 @ a_XfkH $dStorable_Xft5) `cast` ...)
      (($fStorableArrayType3 @ a_XfkH $dStorable_Xft5) `cast` ...)
      (($fStorableArrayType1 @ a_XfkH $dStorable_Xft5) `cast` ...)

-- RHS size: {terms: 1, types: 6, coercions: 1}
$sprintf3 :: (() :: *) ~~ (() :: *)
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 0 10}]
$sprintf3 = Eq# @ * @ * @ () @ () @~ ...

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl69_rIrH
  :: Int#
     -> ST
          RealWorld
          (SV.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Int)

lvl69_rIrH =
  \ (ipv_sp33 :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (SV.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Int))
      (lvl36_rIr0 `cast` ...)
      (unpackAppendCString#
         "Storable.basicUnsafeNew: length too large: "#
         (case $wshowSignedInt 0# ipv_sp33 ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 14, types: 20, coercions: 4}
lvl70_rIrI
  :: Int#
     -> ST
          RealWorld
          (SV.MVector
             (Control.Monad.Primitive.PrimState (ST RealWorld))
             Int)

lvl70_rIrI =
  \ (ipv_sp33 :: Int#) ->
    error
      @ 'PtrRepLifted
      @ (ST
           RealWorld
           (SV.MVector
              (Control.Monad.Primitive.PrimState (ST RealWorld))
              Int))
      (lvl41_rIr5 `cast` ...)
      (unpackAppendCString#
         "Storable.basicUnsafeNew: negative length: "#
         (case $wshowSignedInt 0# ipv_sp33 ([] @ Char)
          of _ [Occ=Dead] { (# ww5_agJC, ww6_agJD #) ->
          : @ Char ww5_agJC ww6_agJD
          }))

-- RHS size: {terms: 20, types: 15, coercions: 0}
$w$c== [InlPrag=[0]]
  :: forall a_afir.
     (Eq a_afir, Storable a_afir) =>
     Int#
     -> SV.Vector a_afir -> Int# -> SV.Vector a_afir -> Bool
[GblId,
 Arity=6,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=IF_ARGS [0 0 0 0 0 0] 72 10}]
$w$c== =
  \ (@ a_afir)
    (w_sylH :: Eq a_afir)
    (w1_sylI :: Storable a_afir)
    (ww_sylQ :: Int#)
    (ww1_sylS :: SV.Vector a_afir)
    (ww2_sylZ :: Int#)
    (ww3_sym1 :: SV.Vector a_afir) ->
    case tagToEnum# @ Bool (==# ww_sylQ ww2_sylZ)
    of _ [Occ=Dead] {
      False -> False;
      True ->
        Data.Vector.Storable.$fEqVector_$c==
          @ a_afir w1_sylI w_sylH ww1_sylS ww3_sym1
    }

-- RHS size: {terms: 24, types: 25, coercions: 0}
$fEqArrayType_$c== [InlPrag=INLINE[0]]
  :: forall a_afir.
     (Eq a_afir, Storable a_afir) =>
     ArrayType a_afir -> ArrayType a_afir -> Bool
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_afir)
                 (w_sylH [Occ=Once] :: Eq a_afir)
                 (w1_sylI [Occ=Once] :: Storable a_afir)
                 (w2_sylJ [Occ=Once!] :: ArrayType a_afir)
                 (w3_sylK [Occ=Once!] :: ArrayType a_afir) ->
                 case w2_sylJ
                 of _ [Occ=Dead]
                 { ArrayType ww1_sylN [Occ=Once!] ww2_sylS [Occ=Once] ->
                 case ww1_sylN of _ [Occ=Dead] { I# ww4_sylQ [Occ=Once] ->
                 case w3_sylK
                 of _ [Occ=Dead]
                 { ArrayType ww6_sylW [Occ=Once!] ww7_sym1 [Occ=Once] ->
                 case ww6_sylW of _ [Occ=Dead] { I# ww9_sylZ [Occ=Once] ->
                 $w$c==
                   @ a_afir w_sylH w1_sylI ww4_sylQ ww2_sylS ww9_sylZ ww7_sym1
                 }
                 }
                 }
                 }}]
$fEqArrayType_$c== =
  \ (@ a_afir)
    (w_sylH :: Eq a_afir)
    (w1_sylI :: Storable a_afir)
    (w2_sylJ :: ArrayType a_afir)
    (w3_sylK :: ArrayType a_afir) ->
    case w2_sylJ of _ [Occ=Dead] { ArrayType ww1_sylN ww2_sylS ->
    case ww1_sylN of _ [Occ=Dead] { I# ww4_sylQ ->
    case w3_sylK of _ [Occ=Dead] { ArrayType ww6_sylW ww7_sym1 ->
    case ww6_sylW of _ [Occ=Dead] { I# ww9_sylZ ->
    $w$c==
      @ a_afir w_sylH w1_sylI ww4_sylQ ww2_sylS ww9_sylZ ww7_sym1
    }
    }
    }
    }

-- RHS size: {terms: 98, types: 72, coercions: 15}
$w$c/= [InlPrag=[0]]
  :: forall a_afir.
     (Eq a_afir, Storable a_afir) =>
     Int#
     -> SV.Vector a_afir -> Int# -> SV.Vector a_afir -> Bool
[GblId,
 Arity=6,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=IF_ARGS [30 60 0 20 0 20] 420 30}]
$w$c/= =
  \ (@ a_afir)
    (w_symg :: Eq a_afir)
    (w1_symh :: Storable a_afir)
    (ww_symp :: Int#)
    (ww1_symr :: SV.Vector a_afir)
    (ww2_symy :: Int#)
    (ww3_symA :: SV.Vector a_afir) ->
    case tagToEnum# @ Bool (==# ww_symp ww2_symy)
    of _ [Occ=Dead] {
      False -> True;
      True ->
        case ww1_symr
        of _ [Occ=Dead]
        { Data.Vector.Storable.Vector ipv_sp13 ipv1_sp14 ipv2_sp15 ->
        case ww3_symA
        of _ [Occ=Dead]
        { Data.Vector.Storable.Vector ipv3_sp1H ipv4_sp1I ipv5_sp1J ->
        let {
          lvl102_sB60 :: Ptr a_afir

          lvl102_sB60 = Ptr @ a_afir ipv4_sp1I } in
        let {
          lvl103_sB5Z :: Ptr a_afir

          lvl103_sB5Z = Ptr @ a_afir ipv1_sp14 } in
        letrec {
          $s$weq_loop0_sErT [Occ=LoopBreaker]
            :: Int#
               -> Int# -> Data.Vector.Fusion.Util.Id Bool

          $s$weq_loop0_sErT =
            \ (sc_sErS :: Int#) (sc1_sErR :: Int#) ->
              case tagToEnum# @ Bool (>=# sc1_sErR ipv_sp13)
              of _ [Occ=Dead] {
                False ->
                  case tagToEnum# @ Bool (>=# sc_sErS ipv3_sp1H)
                  of _ [Occ=Dead] {
                    False ->
                      case ==
                             @ a_afir
                             w_symg
                             (case ((peekElemOff
                                       @ a_afir w1_symh lvl103_sB5Z (I# sc1_sErR))
                                    `cast` ...)
                                     realWorld#
                              of _ [Occ=Dead] { (# ipv6_ahW0, ipv7_ahW1 #) ->
                              case touch#
                                     @ 'PtrRepLifted
                                     @ ForeignPtrContents
                                     ipv2_sp15
                                     ipv6_ahW0
                              of _ [Occ=Dead, OS=OneShot] { __DEFAULT ->
                              ipv7_ahW1
                              }
                              })
                             (case ((peekElemOff
                                       @ a_afir w1_symh lvl102_sB60 (I# sc_sErS))
                                    `cast` ...)
                                     realWorld#
                              of _ [Occ=Dead] { (# ipv6_ahW0, ipv7_ahW1 #) ->
                              case touch#
                                     @ 'PtrRepLifted
                                     @ ForeignPtrContents
                                     ipv5_sp1J
                                     ipv6_ahW0
                              of _ [Occ=Dead, OS=OneShot] { __DEFAULT ->
                              ipv7_ahW1
                              }
                              })
                      of _ [Occ=Dead] {
                        False -> False `cast` ...;
                        True ->
                          $s$weq_loop0_sErT
                            (+# sc_sErS 1#) (+# sc1_sErR 1#)
                      };
                    True -> False `cast` ...
                  };
                True ->
                  (tagToEnum# @ Bool (>=# sc_sErS ipv3_sp1H))
                  `cast` ...
              }; } in
        case ($s$weq_loop0_sErT 0# 0#) `cast` ... of _ [Occ=Dead] {
          False -> True;
          True -> False
        }
        }
        }
    }

-- RHS size: {terms: 24, types: 25, coercions: 0}
$fEqArrayType_$c/= [InlPrag=INLINE[0]]
  :: forall a_afir.
     (Eq a_afir, Storable a_afir) =>
     ArrayType a_afir -> ArrayType a_afir -> Bool
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_afir)
                 (w_symg [Occ=Once] :: Eq a_afir)
                 (w1_symh [Occ=Once] :: Storable a_afir)
                 (w2_symi [Occ=Once!] :: ArrayType a_afir)
                 (w3_symj [Occ=Once!] :: ArrayType a_afir) ->
                 case w2_symi
                 of _ [Occ=Dead]
                 { ArrayType ww1_symm [Occ=Once!] ww2_symr [Occ=Once] ->
                 case ww1_symm of _ [Occ=Dead] { I# ww4_symp [Occ=Once] ->
                 case w3_symj
                 of _ [Occ=Dead]
                 { ArrayType ww6_symv [Occ=Once!] ww7_symA [Occ=Once] ->
                 case ww6_symv of _ [Occ=Dead] { I# ww9_symy [Occ=Once] ->
                 $w$c/=
                   @ a_afir w_symg w1_symh ww4_symp ww2_symr ww9_symy ww7_symA
                 }
                 }
                 }
                 }}]
$fEqArrayType_$c/= =
  \ (@ a_afir)
    (w_symg :: Eq a_afir)
    (w1_symh :: Storable a_afir)
    (w2_symi :: ArrayType a_afir)
    (w3_symj :: ArrayType a_afir) ->
    case w2_symi of _ [Occ=Dead] { ArrayType ww1_symm ww2_symr ->
    case ww1_symm of _ [Occ=Dead] { I# ww4_symp ->
    case w3_symj of _ [Occ=Dead] { ArrayType ww6_symv ww7_symA ->
    case ww6_symv of _ [Occ=Dead] { I# ww9_symy ->
    $w$c/=
      @ a_afir w_symg w1_symh ww4_symp ww2_symr ww9_symy ww7_symA
    }
    }
    }
    }

-- RHS size: {terms: 10, types: 10, coercions: 0}
$fEqArrayType [InlPrag=[ALWAYS] CONLIKE]
  :: forall a_adog.
     (Eq a_adog, Storable a_adog) =>
     Eq (ArrayType a_adog)
[GblId[DFunId],
 Arity=2,

 Unf=DFun: \ (@ a_afir[ssk])
             ($dEq_afis :: Eq a_afir[ssk])
             ($dStorable_afit :: Storable a_afir[ssk]) ->
       C:Eq TYPE: ArrayType a_afir[ssk]
                        $fEqArrayType_$c== @ a_afir[ssk] $dEq_afis $dStorable_afit
                        $fEqArrayType_$c/= @ a_afir[ssk] $dEq_afis $dStorable_afit]
$fEqArrayType =
  \ (@ a_afir)
    ($dEq_afis :: Eq a_afir)
    ($dStorable_afit :: Storable a_afir) ->
    C:Eq
      @ (ArrayType a_afir)
      ($fEqArrayType_$c== @ a_afir $dEq_afis $dStorable_afit)
      ($fEqArrayType_$c/= @ a_afir $dEq_afis $dStorable_afit)

-- RHS size: {terms: 2, types: 0, coercions: 0}
$fShowArrayType2 :: [Char]
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=False, ConLike=False,
         WorkFree=False, Expandable=False, Guidance=IF_ARGS [] 60 0}]
$fShowArrayType2 = unpackCString# "ArrayType "#

-- RHS size: {terms: 80, types: 64, coercions: 2}
$w$cshowsPrec [InlPrag=[0]]
  :: forall a_afhH.
     (Show a_afhH, Storable a_afhH) =>
     Int# -> Int -> SV.Vector a_afhH -> ShowS
[GblId,
 Arity=5,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=IF_ARGS [30 30 0 20 20] 445 120}]
$w$cshowsPrec =
  \ (@ a_afhH)
    (w_symK :: Show a_afhH)
    (w1_symL :: Storable a_afhH)
    (ww_symQ :: Int#)
    (ww1_symU :: Int)
    (ww2_symV :: SV.Vector a_afhH) ->
    let {
      g_Xh5F [Dmd=<L,C(U)>] :: String -> String

      g_Xh5F =
        showList
          @ a_afhH
          w_symK
          (case ww2_symV
           of _ [Occ=Dead]
           { Data.Vector.Storable.Vector ipv_sp0j ipv1_sp0k ipv2_sp0l ->
           let {
             lvl102_sB61 :: Ptr a_afhH

             lvl102_sB61 = Ptr @ a_afhH ipv1_sp0k } in
           letrec {
             $wgo3_symI [InlPrag=[0], Occ=LoopBreaker]
               :: Int# -> [a_afhH]

             $wgo3_symI =
               \ (ww3_symG :: Int#) ->
                 case tagToEnum# @ Bool (>=# ww3_symG ipv_sp0j)
                 of _ [Occ=Dead] {
                   False ->
                     :
                       @ a_afhH
                       (case ((peekElemOff
                                 @ a_afhH w1_symL lvl102_sB61 (I# ww3_symG))
                              `cast` ...)
                               realWorld#
                        of _ [Occ=Dead] { (# ipv3_ahW0, ipv4_ahW1 #) ->
                        case touch#
                               @ 'PtrRepLifted
                               @ ForeignPtrContents
                               ipv2_sp0l
                               ipv3_ahW0
                        of _ [Occ=Dead, OS=OneShot] { __DEFAULT ->
                        ipv4_ahW1
                        }
                        })
                       ($wgo3_symI (+# ww3_symG 1#));
                   True -> [] @ a_afhH
                 }; } in
           $wgo3_symI 0#
           }) } in
    let {
      p_agCj :: ShowS

      p_agCj =
        \ (x_Xh5L :: String) ->
          ++
            @ Char
            $fShowArrayType2
            (case ww1_symU of _ [Occ=Dead] { I# ww4_agJy ->
             case $wshowSignedInt
                    11#
                    ww4_agJy
                    (: @ Char showSpace1 (g_Xh5F x_Xh5L))
             of _ [Occ=Dead] { (# ww6_agJC, ww7_agJD #) ->
             : @ Char ww6_agJC ww7_agJD
             }
             }) } in
    case tagToEnum# @ Bool (>=# ww_symQ 11#)
    of _ [Occ=Dead] {
      False -> p_agCj;
      True ->
        \ (x_agCo :: String) ->
          :
            @ Char
            shows9
            (p_agCj (: @ Char shows6 x_agCo))
    }

-- RHS size: {terms: 17, types: 17, coercions: 0}
$fShowArrayType_$cshowsPrec [InlPrag=INLINE[0]]
  :: forall a_afhH.
     (Show a_afhH, Storable a_afhH) =>
     Int -> ArrayType a_afhH -> ShowS
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_afhH)
                 (w_symK [Occ=Once] :: Show a_afhH)
                 (w1_symL [Occ=Once] :: Storable a_afhH)
                 (w2_symM [Occ=Once!] :: Int)
                 (w3_symN [Occ=Once!] :: ArrayType a_afhH) ->
                 case w2_symM of _ [Occ=Dead] { I# ww1_symQ [Occ=Once] ->
                 case w3_symN
                 of _ [Occ=Dead]
                 { ArrayType ww3_symU [Occ=Once] ww4_symV [Occ=Once] ->
                 $w$cshowsPrec
                   @ a_afhH w_symK w1_symL ww1_symQ ww3_symU ww4_symV
                 }
                 }}]
$fShowArrayType_$cshowsPrec =
  \ (@ a_afhH)
    (w_symK :: Show a_afhH)
    (w1_symL :: Storable a_afhH)
    (w2_symM :: Int)
    (w3_symN :: ArrayType a_afhH) ->
    case w2_symM of _ [Occ=Dead] { I# ww1_symQ ->
    case w3_symN of _ [Occ=Dead] { ArrayType ww3_symU ww4_symV ->
    $w$cshowsPrec
      @ a_afhH w_symK w1_symL ww1_symQ ww3_symU ww4_symV
    }
    }

-- RHS size: {terms: 14, types: 15, coercions: 0}
$fShowArrayType_$cshow
  :: forall a_afhH.
     (Show a_afhH, Storable a_afhH) =>
     ArrayType a_afhH -> String
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_afhH)
                 ($dShow_afhI [Occ=Once] :: Show a_afhH)
                 ($dStorable_afhJ [Occ=Once] :: Storable a_afhH)
                 (x_agCZ [Occ=Once] :: ArrayType a_afhH) ->
                 $fShowArrayType_$cshowsPrec
                   @ a_afhH
                   $dShow_afhI
                   $dStorable_afhJ
                   shows22
                   x_agCZ
                   ([] @ Char)}]
$fShowArrayType_$cshow =
  \ (@ a_afhH)
    ($dShow_afhI :: Show a_afhH)
    ($dStorable_afhJ :: Storable a_afhH)
    (x_agCZ :: ArrayType a_afhH) ->
    case x_agCZ of _ [Occ=Dead] { ArrayType ww1_symU ww2_symV ->
    $w$cshowsPrec
      @ a_afhH
      $dShow_afhI
      $dStorable_afhJ
      0#
      ww1_symU
      ww2_symV
      ([] @ Char)
    }

-- RHS size: {terms: 18, types: 20, coercions: 0}
$fShowArrayType_$cshowList
  :: forall a_afhH.
     (Show a_afhH, Storable a_afhH) =>
     [ArrayType a_afhH] -> ShowS
[GblId,
 Arity=4,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=4,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ a_afhH)
                 ($dShow_afhI [Occ=Once] :: Show a_afhH)
                 ($dStorable_afhJ [Occ=Once] :: Storable a_afhH)
                 (eta_B2 [Occ=Once] :: [ArrayType a_afhH])
                 (eta1_Xt0 [Occ=Once] :: String) ->
                 showList__
                   @ (ArrayType a_afhH)
                   ($fShowArrayType_$cshowsPrec
                      @ a_afhH $dShow_afhI $dStorable_afhJ $fShowArrayType1)
                   eta_B2
                   eta1_Xt0}]
$fShowArrayType_$cshowList =
  \ (@ a_afhH)
    ($dShow_afhI :: Show a_afhH)
    ($dStorable_afhJ :: Storable a_afhH)
    (eta_B2 :: [ArrayType a_afhH])
    (eta1_Xt0 :: String) ->
    showList__
      @ (ArrayType a_afhH)
      (\ (w_symN :: ArrayType a_afhH) ->
         case w_symN of _ [Occ=Dead] { ArrayType ww1_symU ww2_symV ->
         $w$cshowsPrec
           @ a_afhH $dShow_afhI $dStorable_afhJ 0# ww1_symU ww2_symV
         })
      eta_B2
      eta1_Xt0

-- RHS size: {terms: 13, types: 11, coercions: 0}
$fShowArrayType [InlPrag=[ALWAYS] CONLIKE]
  :: forall a_adof.
     (Show a_adof, Storable a_adof) =>
     Show (ArrayType a_adof)
[GblId[DFunId],
 Arity=2,

 Unf=DFun: \ (@ a_afhH[ssk])
             ($dShow_afhI :: Show a_afhH[ssk])
             ($dStorable_afhJ :: Storable a_afhH[ssk]) ->
       C:Show TYPE: ArrayType a_afhH[ssk]
                       $fShowArrayType_$cshowsPrec
                         @ a_afhH[ssk] $dShow_afhI $dStorable_afhJ
                       $fShowArrayType_$cshow
                         @ a_afhH[ssk] $dShow_afhI $dStorable_afhJ
                       $fShowArrayType_$cshowList
                         @ a_afhH[ssk] $dShow_afhI $dStorable_afhJ]
$fShowArrayType =
  \ (@ a_afhH)
    ($dShow_afhI :: Show a_afhH)
    ($dStorable_afhJ :: Storable a_afhH) ->
    C:Show
      @ (ArrayType a_afhH)
      ($fShowArrayType_$cshowsPrec
         @ a_afhH $dShow_afhI $dStorable_afhJ)
      ($fShowArrayType_$cshow @ a_afhH $dShow_afhI $dStorable_afhJ)
      ($fShowArrayType_$cshowList
         @ a_afhH $dShow_afhI $dStorable_afhJ)

-- RHS size: {terms: 6, types: 9, coercions: 0}
getV :: forall t_afhA. ArrayType t_afhA -> SV.Vector t_afhA
[GblId,
 Arity=1,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=False)
         Tmpl= \ (@ t_afhA) (ds_dfEy [Occ=Once!] :: ArrayType t_afhA) ->
                 case ds_dfEy
                 of _ [Occ=Dead] { ArrayType _ [Occ=Dead] x_aaym [Occ=Once] ->
                 x_aaym
                 }}]
getV =
  \ (@ t_afhA) (ds_dfEy :: ArrayType t_afhA) ->
    case ds_dfEy of _ [Occ=Dead] { ArrayType ds1_dfEE x_aaym ->
    x_aaym
    }

-- RHS size: {terms: 2, types: 0, coercions: 0}
labelHP :: Double
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 20}]
labelHP = D# 1.0##

-- RHS size: {terms: 2,977, types: 1,446, coercions: 178}
$wlabelPrior [InlPrag=[0]]
  :: Int#
     -> MWC.GenIO
     -> State# RealWorld
     -> (# State# RealWorld, V.Vector Double #)

$wlabelPrior =
  \ (ww_syo5 :: Int#)
    (w_syo1 :: MWC.GenIO)
    (w1_syo2 [OS=OneShot] :: State# RealWorld) ->
    let {
      $j_stpi
        :: Int#
           -> (# State# RealWorld, V.Vector Double #)

      $j_stpi =
        \ (n#_aoGg [OS=OneShot] :: Int#) ->
          case newArray#
                 @ Double
                 @ (Control.Monad.Primitive.PrimState IO)
                 n#_aoGg
                 (Data.Vector.Mutable.uninitialised @ Double)
                 (w1_syo2 `cast` ...)
          of _ [Occ=Dead] { (# ipv_aoGm, ipv1_aoGn #) ->
          letrec {
            mainloop_sB63 [Occ=LoopBreaker]
              :: State# RealWorld
                 -> (# State# RealWorld, Double #)

            mainloop_sB63 =
              \ (s_XiKB [OS=OneShot] :: State# RealWorld) ->
                letrec {
                  innerloop_slmB [Occ=LoopBreaker]
                    :: State# RealWorld
                       -> (# State# RealWorld,
                             System.Random.MWC.Distributions.T #)

                  innerloop_slmB =
                    \ (s1_XiKz [OS=OneShot] :: State# RealWorld) ->
                      letrec {
                        loop_slk2 [Occ=LoopBreaker]
                          :: State# RealWorld
                             -> (# State# RealWorld, Double #)

                        loop_slk2 =
                          \ (s2_XiKA [OS=OneShot] :: State# RealWorld) ->
                            case w_syo1 `cast` ...
                            of _ [Occ=Dead]
                            { Data.Vector.Primitive.Mutable.MVector dt_ag4P dt1_ag4Q
                                                                    dt2_ag4R ->
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P 256#)
                                   (s2_XiKA `cast` ...)
                            of _ [Occ=Dead] { (# ipv2_ahoH, ipv3_ahoI #) ->
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P 257#)
                                   ipv2_ahoH
                            of _ [Occ=Dead] { (# ipv4_XhUY, ipv5_XhV0 #) ->
                            let {
                              ipv6_skXd [Dmd=<S,U>] :: Int#

                              ipv6_skXd =
                                word2Int#
                                  (narrow8Word# (plusWord# ipv3_ahoI 1##)) } in
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P ipv6_skXd)
                                   ipv4_XhUY
                            of _ [Occ=Dead] { (# ipv7_XhUZ, ipv8_XhV1 #) ->
                            let {
                              j_skXe [Dmd=<S,U>] :: Int#

                              j_skXe =
                                word2Int#
                                  (narrow8Word#
                                     (int2Word# (+# ipv6_skXd 1#))) } in
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P j_skXe)
                                   ipv7_XhUZ
                            of _ [Occ=Dead] { (# ipv9_XhV4, ipv10_XhV6 #) ->
                            let {
                              t1_skWS [Dmd=<S,U>] :: Word#

                              t1_skWS =
                                plusWord#
                                  (timesWord# 1540315826## ipv8_XhV1) ipv5_XhV0 } in
                            let {
                              c'_skWT [Dmd=<S,U>] :: Word#

                              c'_skWT =
                                narrow32Word#
                                  (uncheckedShiftRL# t1_skWS 32#) } in
                            let {
                              x1_skWU [Dmd=<S,U>] :: Word#

                              x1_skWU =
                                narrow32Word#
                                  (plusWord# (narrow32Word# t1_skWS) c'_skWT) } in
                            let {
                              $w$j_synl [InlPrag=[0]]
                                :: State# RealWorld
                                   -> Double#
                                   -> (# State# RealWorld, Double #)

                              $w$j_synl =
                                \ (w2_synf [OS=OneShot] :: State# RealWorld)
                                  (ww1_synj [OS=OneShot] :: Double#) ->
                                  case readWord32Array#
                                         @ (Control.Monad.Primitive.PrimState IO)
                                         dt2_ag4R
                                         (+# dt_ag4P 256#)
                                         (w2_synf `cast` ...)
                                  of _ [Occ=Dead] { (# ipv11_XhVN, ipv12_XhVP #) ->
                                  case readWord32Array#
                                         @ (Control.Monad.Primitive.PrimState IO)
                                         dt2_ag4R
                                         (+# dt_ag4P 257#)
                                         ipv11_XhVN
                                  of _ [Occ=Dead] { (# ipv13_XhUO, ipv14_XhUQ #) ->
                                  let {
                                    ipv15_skZV [Dmd=<S,U>] :: Int#

                                    ipv15_skZV =
                                      word2Int#
                                        (narrow8Word#
                                           (plusWord# ipv12_XhVP 1##)) } in
                                  case readWord32Array#
                                         @ (Control.Monad.Primitive.PrimState IO)
                                         dt2_ag4R
                                         (+# dt_ag4P ipv15_skZV)
                                         ipv13_XhUO
                                  of _ [Occ=Dead] { (# ipv16_XhUS, ipv17_XhUU #) ->
                                  let {
                                    t2_skZJ [Dmd=<S,U>] :: Word#

                                    t2_skZJ =
                                      plusWord#
                                        (timesWord# 1540315826## ipv17_XhUU)
                                        ipv14_XhUQ } in
                                  let {
                                    c'1_skZK [Dmd=<S,U>] :: Word#

                                    c'1_skZK =
                                      narrow32Word#
                                        (uncheckedShiftRL# t2_skZJ 32#) } in
                                  let {
                                    x2_skZL [Dmd=<S,U>] :: Word#

                                    x2_skZL =
                                      narrow32Word#
                                        (plusWord#
                                           (narrow32Word# t2_skZJ) c'1_skZK) } in
                                  let {
                                    $w$j1_syne [InlPrag=[0]]
                                      :: State# RealWorld
                                         -> Word#
                                         -> (# State# RealWorld, Double #)

                                    $w$j1_syne =
                                      \ (w3_syn8 [OS=OneShot] :: State# RealWorld)
                                        (ww2_sync [OS=OneShot] :: Word#) ->
                                        let {
                                          $j1_sljO
                                            :: Double#
                                               -> State# RealWorld
                                               -> (# State# RealWorld, Double #)

                                          $j1_sljO =
                                            \ (x_ahqq [OS=OneShot] :: Double#)
                                              (eta_Xy9 [OS=OneShot]
                                                 :: State# RealWorld) ->
                                              case System.Random.MWC.Distributions.ratios `cast` ...
                                              of _ [Occ=Dead]
                                              { Data.Vector.Primitive.Vector dt4_ajiC dt5_ajiD
                                                                             dt6_ajiE ->
                                              let {
                                                y_agKz [Dmd=<S,U>] :: Int#

                                                y_agKz =
                                                  word2Int#
                                                    (and# ww2_sync 127##) } in
                                              case indexDoubleArray#
                                                     dt6_ajiE (+# dt4_ajiC y_agKz)
                                              of wild2_ajiO { __DEFAULT ->
                                              case tagToEnum#
                                                     @ Bool (<## x_ahqq wild2_ajiO)
                                              of _ [Occ=Dead] {
                                                False ->
                                                  case y_agKz of wild4_X7L {
                                                    __DEFAULT ->
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P 256#)
                                                             (eta_Xy9 `cast` ...)
                                                      of _ [Occ=Dead]
                                                      { (# ipv18_XhVV, ipv19_XhVX #) ->
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P 257#)
                                                             ipv18_XhVV
                                                      of _ [Occ=Dead]
                                                      { (# ipv20_XhVl, ipv21_XhVn #) ->
                                                      let {
                                                        ipv22_sl48 [Dmd=<S,U>] :: Int#

                                                        ipv22_sl48 =
                                                          word2Int#
                                                            (narrow8Word#
                                                               (plusWord#
                                                                  ipv19_XhVX 1##)) } in
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P ipv22_sl48)
                                                             ipv20_XhVl
                                                      of _ [Occ=Dead]
                                                      { (# ipv23_XhVm, ipv24_XhVo #) ->
                                                      let {
                                                        j1_sl49 [Dmd=<S,U>] :: Int#

                                                        j1_sl49 =
                                                          word2Int#
                                                            (narrow8Word#
                                                               (int2Word#
                                                                  (+# ipv22_sl48 1#))) } in
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P j1_sl49)
                                                             ipv23_XhVm
                                                      of _ [Occ=Dead]
                                                      { (# ipv25_XhVr, ipv26_XhVt #) ->
                                                      let {
                                                        t3_sl3N [Dmd=<S,U>] :: Word#

                                                        t3_sl3N =
                                                          plusWord#
                                                            (timesWord#
                                                               1540315826## ipv24_XhVo)
                                                            ipv21_XhVn } in
                                                      let {
                                                        c'2_sl3O [Dmd=<S,U>] :: Word#

                                                        c'2_sl3O =
                                                          narrow32Word#
                                                            (uncheckedShiftRL#
                                                               t3_sl3N 32#) } in
                                                      let {
                                                        x3_sl3P [Dmd=<S,U>] :: Word#

                                                        x3_sl3P =
                                                          narrow32Word#
                                                            (plusWord#
                                                               (narrow32Word# t3_sl3N)
                                                               c'2_sl3O) } in
                                                      case tagToEnum#
                                                             @ Bool
                                                             (ltWord# x3_sl3P c'2_sl3O)
                                                      of _ [Occ=Dead] {
                                                        False ->
                                                          let {
                                                            u1_snpv [Dmd=<S,U>] :: Word#

                                                            u1_snpv =
                                                              plusWord#
                                                                (timesWord#
                                                                   1540315826## ipv26_XhVt)
                                                                c'2_sl3O } in
                                                          let {
                                                            d'_snpw [Dmd=<S,U>] :: Word#

                                                            d'_snpw =
                                                              narrow32Word#
                                                                (uncheckedShiftRL#
                                                                   u1_snpv 32#) } in
                                                          let {
                                                            y1_snpx [Dmd=<S,U>] :: Word#

                                                            y1_snpx =
                                                              narrow32Word#
                                                                (plusWord#
                                                                   (narrow32Word# u1_snpv)
                                                                   d'_snpw) } in
                                                          case tagToEnum#
                                                                 @ Bool
                                                                 (ltWord# y1_snpx d'_snpw)
                                                          of _ [Occ=Dead] {
                                                            False ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_sl48)
                                                                     x3_sl3P
                                                                     ipv25_XhVr
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_sl49)
                                                                     y1_snpx
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_sl49))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     d'_snpw
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqt
                                                                                             dt8_Xjqv
                                                                                             dt9_Xjqx ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt wild4_X7L)
                                                              of wild8_XjtJ { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_synj wild8_XjtJ } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt
                                                                        (+# wild4_X7L 1#))
                                                              of wild9_XjqN { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqN
                                                                                wild9_XjqN)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                x3_sl3P)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      y1_snpx)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtJ
                                                                                                wild8_XjtJ)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_slk2 (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              };
                                                            True ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_sl48)
                                                                     x3_sl3P
                                                                     ipv25_XhVr
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              let {
                                                                x#_ageT [Dmd=<S,U>]
                                                                  :: Word#

                                                                x#_ageT =
                                                                  narrow32Word#
                                                                    (plusWord#
                                                                       y1_snpx 1##) } in
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_sl49)
                                                                     x#_ageT
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_sl49))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     (narrow32Word#
                                                                        (plusWord#
                                                                           d'_snpw 1##))
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqt
                                                                                             dt8_Xjqv
                                                                                             dt9_Xjqx ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt wild4_X7L)
                                                              of wild8_XjtL { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_synj wild8_XjtL } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt
                                                                        (+# wild4_X7L 1#))
                                                              of wild9_XjqN { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqN
                                                                                wild9_XjqN)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                x3_sl3P)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      x#_ageT)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtL
                                                                                                wild8_XjtL)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_slk2 (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                          };
                                                        True ->
                                                          let {
                                                            u1_snpY [Dmd=<S,U>] :: Word#

                                                            u1_snpY =
                                                              plusWord#
                                                                (timesWord#
                                                                   1540315826## ipv26_XhVt)
                                                                (narrow32Word#
                                                                   (plusWord#
                                                                      c'2_sl3O 1##)) } in
                                                          let {
                                                            d'_snpZ [Dmd=<S,U>] :: Word#

                                                            d'_snpZ =
                                                              narrow32Word#
                                                                (uncheckedShiftRL#
                                                                   u1_snpY 32#) } in
                                                          let {
                                                            y1_snq0 [Dmd=<S,U>] :: Word#

                                                            y1_snq0 =
                                                              narrow32Word#
                                                                (plusWord#
                                                                   (narrow32Word# u1_snpY)
                                                                   d'_snpZ) } in
                                                          let {
                                                            ipv27_snpX [Dmd=<S,U>] :: Word#

                                                            ipv27_snpX =
                                                              narrow32Word#
                                                                (plusWord#
                                                                   x3_sl3P 1##) } in
                                                          case tagToEnum#
                                                                 @ Bool
                                                                 (ltWord# y1_snq0 d'_snpZ)
                                                          of _ [Occ=Dead] {
                                                            False ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_sl48)
                                                                     ipv27_snpX
                                                                     ipv25_XhVr
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_sl49)
                                                                     y1_snq0
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_sl49))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     d'_snpZ
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqt
                                                                                             dt8_Xjqv
                                                                                             dt9_Xjqx ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt wild4_X7L)
                                                              of wild8_XjtL { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_synj wild8_XjtL } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt
                                                                        (+# wild4_X7L 1#))
                                                              of wild9_XjqN { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqN
                                                                                wild9_XjqN)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                ipv27_snpX)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      y1_snq0)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtL
                                                                                                wild8_XjtL)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_slk2 (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              };
                                                            True ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_sl48)
                                                                     ipv27_snpX
                                                                     ipv25_XhVr
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              let {
                                                                x#_ageT [Dmd=<S,U>]
                                                                  :: Word#

                                                                x#_ageT =
                                                                  narrow32Word#
                                                                    (plusWord#
                                                                       y1_snq0 1##) } in
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_sl49)
                                                                     x#_ageT
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_sl49))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     (narrow32Word#
                                                                        (plusWord#
                                                                           d'_snpZ 1##))
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqt
                                                                                             dt8_Xjqv
                                                                                             dt9_Xjqx ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt wild4_X7L)
                                                              of wild8_XjtN { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_synj wild8_XjtN } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqx
                                                                     (+#
                                                                        dt7_Xjqt
                                                                        (+# wild4_X7L 1#))
                                                              of wild9_XjqN { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqN
                                                                                wild9_XjqN)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                ipv27_snpX)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      x#_ageT)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtN
                                                                                                wild8_XjtN)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_slk2 (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                          }
                                                      }
                                                      }
                                                      }
                                                      }
                                                      };
                                                    0# ->
                                                      letrec {
                                                        tailing_slfz [Occ=LoopBreaker]
                                                          :: State# RealWorld
                                                             -> (# State#
                                                                     RealWorld,
                                                                   Double #)

                                                        tailing_slfz =
                                                          \ (s3_XiM2 [OS=OneShot]
                                                               :: State#
                                                                    RealWorld) ->
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P 256#)
                                                                   (s3_XiM2 `cast` ...)
                                                            of _ [Occ=Dead]
                                                            { (# ipv18_XhVR, ipv19_XhVT #) ->
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P 257#)
                                                                   ipv18_XhVR
                                                            of _ [Occ=Dead]
                                                            { (# ipv20_XhVh, ipv21_XhVj #) ->
                                                            let {
                                                              ipv22_sl8E [Dmd=<S,U>]
                                                                :: Int#

                                                              ipv22_sl8E =
                                                                word2Int#
                                                                  (narrow8Word#
                                                                     (plusWord#
                                                                        ipv19_XhVT 1##)) } in
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P ipv22_sl8E)
                                                                   ipv20_XhVh
                                                            of _ [Occ=Dead]
                                                            { (# ipv23_XhVi, ipv24_XhVk #) ->
                                                            let {
                                                              j1_sl8F [Dmd=<S,U>] :: Int#

                                                              j1_sl8F =
                                                                word2Int#
                                                                  (narrow8Word#
                                                                     (int2Word#
                                                                        (+#
                                                                           ipv22_sl8E 1#))) } in
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P j1_sl8F)
                                                                   ipv23_XhVi
                                                            of _ [Occ=Dead]
                                                            { (# ipv25_XhVn, ipv26_XhVp #) ->
                                                            let {
                                                              t3_sl8j [Dmd=<S,U>] :: Word#

                                                              t3_sl8j =
                                                                plusWord#
                                                                  (timesWord#
                                                                     1540315826## ipv24_XhVk)
                                                                  ipv21_XhVj } in
                                                            let {
                                                              c'2_sl8k [Dmd=<S,U>] :: Word#

                                                              c'2_sl8k =
                                                                narrow32Word#
                                                                  (uncheckedShiftRL#
                                                                     t3_sl8j 32#) } in
                                                            let {
                                                              x3_sl8l [Dmd=<S,U>] :: Word#

                                                              x3_sl8l =
                                                                narrow32Word#
                                                                  (plusWord#
                                                                     (narrow32Word#
                                                                        t3_sl8j)
                                                                     c'2_sl8k) } in
                                                            let {
                                                              $w$j2_syn4 [InlPrag=[0]]
                                                                :: State#
                                                                     RealWorld
                                                                   -> Double#
                                                                   -> (# State#
                                                                           RealWorld,
                                                                         Double #)
                                                              [LclId,
                                                               Arity=2,

                                                              $w$j2_syn4 =
                                                                \ (w4_symY [OS=OneShot]
                                                                     :: State#
                                                                          RealWorld)
                                                                  (ww3_syn2 [OS=OneShot]
                                                                     :: Double#) ->
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+# dt_ag4P 256#)
                                                                         (w4_symY `cast` ...)
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv27_XhW8, ipv28_XhWa #) ->
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+# dt_ag4P 257#)
                                                                         ipv27_XhW8
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv29_Xit2, ipv30_XhVl #) ->
                                                                  let {
                                                                    ipv31_sld4 [Dmd=<S,U>]
                                                                      :: Int#

                                                                    ipv31_sld4 =
                                                                      word2Int#
                                                                        (narrow8Word#
                                                                           (plusWord#
                                                                              ipv28_XhWa 1##)) } in
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+#
                                                                            dt_ag4P ipv31_sld4)
                                                                         ipv29_Xit2
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv32_Xitf, ipv33_XhVm #) ->
                                                                  let {
                                                                    j2_sld5 [Dmd=<S,U>]
                                                                      :: Int#

                                                                    j2_sld5 =
                                                                      word2Int#
                                                                        (narrow8Word#
                                                                           (int2Word#
                                                                              (+#
                                                                                 ipv31_sld4
                                                                                 1#))) } in
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+#
                                                                            dt_ag4P j2_sld5)
                                                                         ipv32_Xitf
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv34_Xitw, ipv35_XhVr #) ->
                                                                  let {
                                                                    t4_slcJ [Dmd=<S,U>]
                                                                      :: Word#

                                                                    t4_slcJ =
                                                                      plusWord#
                                                                        (timesWord#
                                                                           1540315826## ipv33_XhVm)
                                                                        ipv30_XhVl } in
                                                                  let {
                                                                    c'3_slcK [Dmd=<S,U>]
                                                                      :: Word#

                                                                    c'3_slcK =
                                                                      narrow32Word#
                                                                        (uncheckedShiftRL#
                                                                           t4_slcJ 32#) } in
                                                                  let {
                                                                    x4_slcL [Dmd=<S,U>]
                                                                      :: Word#

                                                                    x4_slcL =
                                                                      narrow32Word#
                                                                        (plusWord#
                                                                           (narrow32Word#
                                                                              t4_slcJ)
                                                                           c'3_slcK) } in
                                                                  case tagToEnum#
                                                                         @ Bool
                                                                         (ltWord#
                                                                            x4_slcL c'3_slcK)
                                                                  of _ [Occ=Dead] {
                                                                    False ->
                                                                      let {
                                                                        u1_snrb [Dmd=<S,U>]
                                                                          :: Word#

                                                                        u1_snrb =
                                                                          plusWord#
                                                                            (timesWord#
                                                                               1540315826##
                                                                               ipv35_XhVr)
                                                                            c'3_slcK } in
                                                                      let {
                                                                        d'_snrc [Dmd=<S,U>]
                                                                          :: Word#

                                                                        d'_snrc =
                                                                          narrow32Word#
                                                                            (uncheckedShiftRL#
                                                                               u1_snrb 32#) } in
                                                                      let {
                                                                        y1_snrd [Dmd=<S,U>]
                                                                          :: Word#

                                                                        y1_snrd =
                                                                          narrow32Word#
                                                                            (plusWord#
                                                                               (narrow32Word#
                                                                                  u1_snrb)
                                                                               d'_snrc) } in
                                                                      case tagToEnum#
                                                                             @ Bool
                                                                             (ltWord#
                                                                                y1_snrd d'_snrc)
                                                                      of _ [Occ=Dead] {
                                                                        False ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sld4)
                                                                                 x4_slcL
                                                                                 ipv34_Xitw
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sld5)
                                                                                 y1_snrd
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sld5))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 d'_snrc
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x4_slcL)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         y1_snrd)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syn2
                                                                                       ww3_syn2))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_synj
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syn2) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syn2
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_slfz
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          };
                                                                        True ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sld4)
                                                                                 x4_slcL
                                                                                 ipv34_Xitw
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          let {
                                                                            x#_agmz [Dmd=<S,U>]
                                                                              :: Word#

                                                                            x#_agmz =
                                                                              narrow32Word#
                                                                                (plusWord#
                                                                                   y1_snrd 1##) } in
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sld5)
                                                                                 x#_agmz
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sld5))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 (narrow32Word#
                                                                                    (plusWord#
                                                                                       d'_snrc 1##))
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x4_slcL)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         x#_agmz)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syn2
                                                                                       ww3_syn2))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_synj
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syn2) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syn2
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_slfz
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                      };
                                                                    True ->
                                                                      let {
                                                                        u1_snrE [Dmd=<S,U>]
                                                                          :: Word#

                                                                        u1_snrE =
                                                                          plusWord#
                                                                            (timesWord#
                                                                               1540315826##
                                                                               ipv35_XhVr)
                                                                            (narrow32Word#
                                                                               (plusWord#
                                                                                  c'3_slcK
                                                                                  1##)) } in
                                                                      let {
                                                                        d'_snrF [Dmd=<S,U>]
                                                                          :: Word#

                                                                        d'_snrF =
                                                                          narrow32Word#
                                                                            (uncheckedShiftRL#
                                                                               u1_snrE 32#) } in
                                                                      let {
                                                                        y1_snrG [Dmd=<S,U>]
                                                                          :: Word#

                                                                        y1_snrG =
                                                                          narrow32Word#
                                                                            (plusWord#
                                                                               (narrow32Word#
                                                                                  u1_snrE)
                                                                               d'_snrF) } in
                                                                      let {
                                                                        ipv36_snrD [Dmd=<S,U>]
                                                                          :: Word#

                                                                        ipv36_snrD =
                                                                          narrow32Word#
                                                                            (plusWord#
                                                                               x4_slcL 1##) } in
                                                                      case tagToEnum#
                                                                             @ Bool
                                                                             (ltWord#
                                                                                y1_snrG d'_snrF)
                                                                      of _ [Occ=Dead] {
                                                                        False ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sld4)
                                                                                 ipv36_snrD
                                                                                 ipv34_Xitw
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sld5)
                                                                                 y1_snrG
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sld5))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 d'_snrF
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   ipv36_snrD)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         y1_snrG)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syn2
                                                                                       ww3_syn2))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_synj
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syn2) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syn2
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_slfz
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          };
                                                                        True ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sld4)
                                                                                 ipv36_snrD
                                                                                 ipv34_Xitw
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          let {
                                                                            x#_agmz [Dmd=<S,U>]
                                                                              :: Word#

                                                                            x#_agmz =
                                                                              narrow32Word#
                                                                                (plusWord#
                                                                                   y1_snrG 1##) } in
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sld5)
                                                                                 x#_agmz
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sld5))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 (narrow32Word#
                                                                                    (plusWord#
                                                                                       d'_snrF 1##))
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   ipv36_snrD)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         x#_agmz)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syn2
                                                                                       ww3_syn2))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_synj
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syn2) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syn2
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_slfz
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                      }
                                                                  }
                                                                  }
                                                                  }
                                                                  }
                                                                  } } in
                                                            case tagToEnum#
                                                                   @ Bool
                                                                   (ltWord#
                                                                      x3_sl8l c'2_sl8k)
                                                            of _ [Occ=Dead] {
                                                              False ->
                                                                let {
                                                                  u1_sns7 [Dmd=<S,U>]
                                                                    :: Word#

                                                                  u1_sns7 =
                                                                    plusWord#
                                                                      (timesWord#
                                                                         1540315826## ipv26_XhVp)
                                                                      c'2_sl8k } in
                                                                let {
                                                                  d'_sns8 [Dmd=<S,U>]
                                                                    :: Word#

                                                                  d'_sns8 =
                                                                    narrow32Word#
                                                                      (uncheckedShiftRL#
                                                                         u1_sns7 32#) } in
                                                                let {
                                                                  y1_sns9 [Dmd=<S,U>]
                                                                    :: Word#

                                                                  y1_sns9 =
                                                                    narrow32Word#
                                                                      (plusWord#
                                                                         (narrow32Word#
                                                                            u1_sns7)
                                                                         d'_sns8) } in
                                                                case tagToEnum#
                                                                       @ Bool
                                                                       (ltWord#
                                                                          y1_sns9 d'_sns8)
                                                                of _ [Occ=Dead] {
                                                                  False ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_sl8E)
                                                                           x3_sl8l
                                                                           ipv25_XhVn
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_sl8F)
                                                                           y1_sns9
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_sl8F))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           d'_sns8
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             x3_sl8l)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   y1_sns9)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syn4
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    };
                                                                  True ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_sl8E)
                                                                           x3_sl8l
                                                                           ipv25_XhVn
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    let {
                                                                      x#_agiP [Dmd=<S,U>]
                                                                        :: Word#

                                                                      x#_agiP =
                                                                        narrow32Word#
                                                                          (plusWord#
                                                                             y1_sns9 1##) } in
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_sl8F)
                                                                           x#_agiP
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_sl8F))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           (narrow32Word#
                                                                              (plusWord#
                                                                                 d'_sns8 1##))
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             x3_sl8l)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x#_agiP)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syn4
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                };
                                                              True ->
                                                                let {
                                                                  u1_snsr [Dmd=<S,U>]
                                                                    :: Word#

                                                                  u1_snsr =
                                                                    plusWord#
                                                                      (timesWord#
                                                                         1540315826## ipv26_XhVp)
                                                                      (narrow32Word#
                                                                         (plusWord#
                                                                            c'2_sl8k 1##)) } in
                                                                let {
                                                                  d'_snss [Dmd=<S,U>]
                                                                    :: Word#

                                                                  d'_snss =
                                                                    narrow32Word#
                                                                      (uncheckedShiftRL#
                                                                         u1_snsr 32#) } in
                                                                let {
                                                                  y1_snst [Dmd=<S,U>]
                                                                    :: Word#

                                                                  y1_snst =
                                                                    narrow32Word#
                                                                      (plusWord#
                                                                         (narrow32Word#
                                                                            u1_snsr)
                                                                         d'_snss) } in
                                                                let {
                                                                  ipv27_snsq [Dmd=<S,U>]
                                                                    :: Word#

                                                                  ipv27_snsq =
                                                                    narrow32Word#
                                                                      (plusWord#
                                                                         x3_sl8l 1##) } in
                                                                case tagToEnum#
                                                                       @ Bool
                                                                       (ltWord#
                                                                          y1_snst d'_snss)
                                                                of _ [Occ=Dead] {
                                                                  False ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_sl8E)
                                                                           ipv27_snsq
                                                                           ipv25_XhVn
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_sl8F)
                                                                           y1_snst
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_sl8F))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           d'_snss
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             ipv27_snsq)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   y1_snst)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syn4
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    };
                                                                  True ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_sl8E)
                                                                           ipv27_snsq
                                                                           ipv25_XhVn
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    let {
                                                                      x#_agiP [Dmd=<S,U>]
                                                                        :: Word#

                                                                      x#_agiP =
                                                                        narrow32Word#
                                                                          (plusWord#
                                                                             y1_snst 1##) } in
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_sl8F)
                                                                           x#_agiP
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_sl8F))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           (narrow32Word#
                                                                              (plusWord#
                                                                                 d'_snss 1##))
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             ipv27_snsq)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x#_agiP)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syn4
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                }
                                                            }
                                                            }
                                                            }
                                                            }
                                                            }; } in
                                                      tailing_slfz eta_Xy9
                                                  };
                                                True ->
                                                  case System.Random.MWC.Distributions.blocks
                                                       `cast` ...
                                                  of _ [Occ=Dead]
                                                  { Data.Vector.Primitive.Vector dt7_Xjsz dt8_XjsB
                                                                                 dt9_XjsD ->
                                                  case indexDoubleArray#
                                                         dt9_XjsD (+# dt7_Xjsz y_agKz)
                                                  of wild5_XjsR { __DEFAULT ->
                                                  (# eta_Xy9,
                                                     D#
                                                       (*## ww1_synj wild5_XjsR) #)
                                                  }
                                                  }
                                              }
                                              }
                                              } } in
                                        case tagToEnum#
                                               @ Bool (==## ww1_synj 0.0##)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case tagToEnum#
                                                   @ Bool (>## ww1_synj 0.0##)
                                            of _ [Occ=Dead] {
                                              False ->
                                                $j1_sljO (negateDouble# ww1_synj) w3_syn8;
                                              True -> $j1_sljO ww1_synj w3_syn8
                                            };
                                          True -> $j1_sljO 0.0## w3_syn8
                                        } } in
                                  case tagToEnum#
                                         @ Bool (ltWord# x2_skZL c'1_skZK)
                                  of _ [Occ=Dead] {
                                    False ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P ipv15_skZV)
                                             x2_skZL
                                             ipv16_XhUS
                                      of s'#_agb2 [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 256#)
                                             (narrow32Word#
                                                (int2Word# ipv15_skZV))
                                             s'#_agb2
                                      of s'#1_agbk [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 257#)
                                             c'1_skZK
                                             s'#1_agbk
                                      of s'#2_agbC [OS=OneShot] { __DEFAULT ->
                                      $w$j1_syne (s'#2_agbC `cast` ...) x2_skZL
                                      }
                                      }
                                      };
                                    True ->
                                      let {
                                        x#_agb0 [Dmd=<S,U>] :: Word#

                                        x#_agb0 =
                                          narrow32Word#
                                            (plusWord# x2_skZL 1##) } in
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P ipv15_skZV)
                                             x#_agb0
                                             ipv16_XhUS
                                      of s'#_agb2 [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 256#)
                                             (narrow32Word#
                                                (int2Word# ipv15_skZV))
                                             s'#_agb2
                                      of s'#1_agbk [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 257#)
                                             (narrow32Word#
                                                (plusWord# c'1_skZK 1##))
                                             s'#1_agbk
                                      of s'#2_agbC [OS=OneShot] { __DEFAULT ->
                                      $w$j1_syne (s'#2_agbC `cast` ...) x#_agb0
                                      }
                                      }
                                      }
                                  }
                                  }
                                  }
                                  } } in
                            case tagToEnum# @ Bool (ltWord# x1_skWU c'_skWT)
                            of _ [Occ=Dead] {
                              False ->
                                let {
                                  u_snsV [Dmd=<S,U>] :: Word#

                                  u_snsV =
                                    plusWord#
                                      (timesWord# 1540315826## ipv10_XhV6) c'_skWT } in
                                let {
                                  d'_snsW [Dmd=<S,U>] :: Word#

                                  d'_snsW =
                                    narrow32Word#
                                      (uncheckedShiftRL# u_snsV 32#) } in
                                let {
                                  y_snsX [Dmd=<S,U>] :: Word#

                                  y_snsX =
                                    narrow32Word#
                                      (plusWord#
                                         (narrow32Word# u_snsV) d'_snsW) } in
                                case tagToEnum# @ Bool (ltWord# y_snsX d'_snsW)
                                of _ [Occ=Dead] {
                                  False ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_skXd)
                                           x1_skWU
                                           ipv9_XhV4
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_skXe)
                                           y_snsX
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_skXe))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           d'_snsW
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv11_snt5 [Dmd=<S,U>] :: Double#

                                      ipv11_snt5 =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# x1_skWU)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# y_snsX)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_synl
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv11_snt5 ipv11_snt5) 1.0##)
                                    }
                                    }
                                    }
                                    };
                                  True ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_skXd)
                                           x1_skWU
                                           ipv9_XhV4
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    let {
                                      x#_ag7E [Dmd=<S,U>] :: Word#

                                      x#_ag7E =
                                        narrow32Word# (plusWord# y_snsX 1##) } in
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_skXe)
                                           x#_ag7E
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_skXe))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           (narrow32Word# (plusWord# d'_snsW 1##))
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv11_snta [Dmd=<S,U>] :: Double#

                                      ipv11_snta =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# x1_skWU)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# x#_ag7E)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_synl
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv11_snta ipv11_snta) 1.0##)
                                    }
                                    }
                                    }
                                    }
                                };
                              True ->
                                let {
                                  u_sntc [Dmd=<S,U>] :: Word#

                                  u_sntc =
                                    plusWord#
                                      (timesWord# 1540315826## ipv10_XhV6)
                                      (narrow32Word# (plusWord# c'_skWT 1##)) } in
                                let {
                                  d'_sntd [Dmd=<S,U>] :: Word#

                                  d'_sntd =
                                    narrow32Word#
                                      (uncheckedShiftRL# u_sntc 32#) } in
                                let {
                                  y_snte [Dmd=<S,U>] :: Word#

                                  y_snte =
                                    narrow32Word#
                                      (plusWord#
                                         (narrow32Word# u_sntc) d'_sntd) } in
                                let {
                                  ipv11_sntb [Dmd=<S,U>] :: Word#

                                  ipv11_sntb =
                                    narrow32Word# (plusWord# x1_skWU 1##) } in
                                case tagToEnum# @ Bool (ltWord# y_snte d'_sntd)
                                of _ [Occ=Dead] {
                                  False ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_skXd)
                                           ipv11_sntb
                                           ipv9_XhV4
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_skXe)
                                           y_snte
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_skXe))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           d'_sntd
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv12_sntm [Dmd=<S,U>] :: Double#

                                      ipv12_sntm =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# ipv11_sntb)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# y_snte)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_synl
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv12_sntm ipv12_sntm) 1.0##)
                                    }
                                    }
                                    }
                                    };
                                  True ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_skXd)
                                           ipv11_sntb
                                           ipv9_XhV4
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    let {
                                      x#_ag7E [Dmd=<S,U>] :: Word#

                                      x#_ag7E =
                                        narrow32Word# (plusWord# y_snte 1##) } in
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_skXe)
                                           x#_ag7E
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_skXe))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           (narrow32Word# (plusWord# d'_sntd 1##))
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv12_sntr [Dmd=<S,U>] :: Double#

                                      ipv12_sntr =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# ipv11_sntb)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# x#_ag7E)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_synl
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv12_sntr ipv12_sntr) 1.0##)
                                    }
                                    }
                                    }
                                    }
                                }
                            }
                            }
                            }
                            }
                            }
                            }; } in
                      case loop_slk2 s1_XiKz
                      of _ [Occ=Dead] { (# ipv2_aif4, ipv3_aif5 #) ->
                      case ipv3_aif5 of _ [Occ=Dead] { D# y_Xiiv ->
                      case /## 1.0## (sqrtDouble# 6.000000000000001##)
                      of wild2_aibS { __DEFAULT ->
                      let {
                        x_ahpC [Dmd=<S,U>] :: Double#

                        x_ahpC = +## 1.0## (*## wild2_aibS y_Xiiv) } in
                      case tagToEnum# @ Bool (<=## x_ahpC 0.0##)
                      of _ [Occ=Dead] {
                        False ->
                          (# ipv2_aif4,
                             System.Random.MWC.Distributions.T
                               y_Xiiv (*## (*## x_ahpC x_ahpC) x_ahpC) #);
                        True -> innerloop_slmB ipv2_aif4
                      }
                      }
                      }
                      }; } in
                case innerloop_slmB s_XiKB
                of _ [Occ=Dead] { (# ipv2_aif4, ipv3_aif5 #) ->
                case ipv3_aif5
                of _ [Occ=Dead]
                { System.Random.MWC.Distributions.T dt_agoc dt1_agod ->
                case w_syo1 `cast` ...
                of _ [Occ=Dead]
                { Data.Vector.Primitive.Mutable.MVector dt2_agol dt3_agom
                                                        dt4_agon ->
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol 256#)
                       (ipv2_aif4 `cast` ...)
                of _ [Occ=Dead] { (# ipv4_ahoH, ipv5_ahoI #) ->
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol 257#)
                       ipv4_ahoH
                of _ [Occ=Dead] { (# ipv6_XhV3, ipv7_XhV5 #) ->
                let {
                  ipv8_slqb [Dmd=<S,U>] :: Int#

                  ipv8_slqb =
                    word2Int#
                      (narrow8Word# (plusWord# ipv5_ahoI 1##)) } in
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol ipv8_slqb)
                       ipv6_XhV3
                of _ [Occ=Dead] { (# ipv9_XhV4, ipv10_XhV6 #) ->
                let {
                  j_slqc [Dmd=<S,U>] :: Int#

                  j_slqc =
                    word2Int#
                      (narrow8Word#
                         (int2Word# (+# ipv8_slqb 1#))) } in
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol j_slqc)
                       ipv9_XhV4
                of _ [Occ=Dead] { (# ipv11_XhV9, ipv12_XhVb #) ->
                let {
                  t1_slpQ [Dmd=<S,U>] :: Word#

                  t1_slpQ =
                    plusWord#
                      (timesWord# 1540315826## ipv10_XhV6) ipv7_XhV5 } in
                let {
                  c'_slpR [Dmd=<S,U>] :: Word#

                  c'_slpR =
                    narrow32Word#
                      (uncheckedShiftRL# t1_slpQ 32#) } in
                let {
                  x2_slpS [Dmd=<S,U>] :: Word#

                  x2_slpS =
                    narrow32Word#
                      (plusWord# (narrow32Word# t1_slpQ) c'_slpR) } in
                case tagToEnum# @ Bool (ltWord# x2_slpS c'_slpR)
                of _ [Occ=Dead] {
                  False ->
                    let {
                      u_snve [Dmd=<S,U>] :: Word#

                      u_snve =
                        plusWord#
                          (timesWord# 1540315826## ipv12_XhVb) c'_slpR } in
                    let {
                      d'_snvf [Dmd=<S,U>] :: Word#

                      d'_snvf =
                        narrow32Word# (uncheckedShiftRL# u_snve 32#) } in
                    let {
                      y_snvg [Dmd=<S,U>] :: Word#

                      y_snvg =
                        narrow32Word#
                          (plusWord# (narrow32Word# u_snve) d'_snvf) } in
                    case tagToEnum# @ Bool (ltWord# y_snvg d'_snvf)
                    of _ [Occ=Dead] {
                      False ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_slqb)
                               x2_slpS
                               ipv11_XhV9
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_slqc)
                               y_snvg
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_slqc))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               d'_snvf
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_slqv [Dmd=<S,U>] :: Double#

                          x1_slqv = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# x2_slpS)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# y_snvg)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_slqv x1_slqv))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIj { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_slqv)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIj))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB63 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        };
                      True ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_slqb)
                               x2_slpS
                               ipv11_XhV9
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        let {
                          x#_agr0 [Dmd=<S,U>] :: Word#

                          x#_agr0 =
                            narrow32Word# (plusWord# y_snvg 1##) } in
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_slqc)
                               x#_agr0
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_slqc))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               (narrow32Word# (plusWord# d'_snvf 1##))
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_slqv [Dmd=<S,U>] :: Double#

                          x1_slqv = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# x2_slpS)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# x#_agr0)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_slqv x1_slqv))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIj { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_slqv)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIj))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB63 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        }
                    };
                  True ->
                    let {
                      u_snvt [Dmd=<S,U>] :: Word#

                      u_snvt =
                        plusWord#
                          (timesWord# 1540315826## ipv12_XhVb)
                          (narrow32Word# (plusWord# c'_slpR 1##)) } in
                    let {
                      d'_snvu [Dmd=<S,U>] :: Word#

                      d'_snvu =
                        narrow32Word# (uncheckedShiftRL# u_snvt 32#) } in
                    let {
                      y_snvv [Dmd=<S,U>] :: Word#

                      y_snvv =
                        narrow32Word#
                          (plusWord# (narrow32Word# u_snvt) d'_snvu) } in
                    let {
                      ipv13_snvs [Dmd=<S,U>] :: Word#

                      ipv13_snvs =
                        narrow32Word# (plusWord# x2_slpS 1##) } in
                    case tagToEnum# @ Bool (ltWord# y_snvv d'_snvu)
                    of _ [Occ=Dead] {
                      False ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_slqb)
                               ipv13_snvs
                               ipv11_XhV9
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_slqc)
                               y_snvv
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_slqc))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               d'_snvu
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_slqv [Dmd=<S,U>] :: Double#

                          x1_slqv = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# ipv13_snvs)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# y_snvv)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_slqv x1_slqv))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIj { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_slqv)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIj))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB63 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        };
                      True ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_slqb)
                               ipv13_snvs
                               ipv11_XhV9
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        let {
                          x#_agr0 [Dmd=<S,U>] :: Word#

                          x#_agr0 =
                            narrow32Word# (plusWord# y_snvv 1##) } in
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_slqc)
                               x#_agr0
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_slqc))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               (narrow32Word# (plusWord# d'_snvu 1##))
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_slqv [Dmd=<S,U>] :: Double#

                          x1_slqv = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# ipv13_snvs)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# x#_agr0)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_slqv x1_slqv))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIj { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_slqv)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIj))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB63 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        }
                    }
                }
                }
                }
                }
                }
                }
                }
                }; } in
          letrec {
            $s$wfoldlM'_loop_sEry [Occ=LoopBreaker]
              :: State# RealWorld
                 -> Int#
                 -> Int#
                 -> (# State# RealWorld, Int #)

            $s$wfoldlM'_loop_sEry =
              \ (sc_sErx [OS=OneShot] :: State# RealWorld)
                (sc1_sErv :: Int#)
                (sc2_sEru :: Int#) ->
                case tagToEnum# @ Bool (<# sc1_sErv ww_syo5)
                of _ [Occ=Dead] {
                  False -> (# sc_sErx, I# sc2_sEru #);
                  True ->
                    case mainloop_sB63 sc_sErx
                    of _ [Occ=Dead] { (# ipv2_XilZ, ipv3_Xim1 #) ->
                    case writeArray#
                           @ (Control.Monad.Primitive.PrimState IO)
                           @ Double
                           ipv1_aoGn
                           sc2_sEru
                           ipv3_Xim1
                           (ipv2_XilZ `cast` ...)
                    of s'#_aoH3 [OS=OneShot] { __DEFAULT ->
                    $s$wfoldlM'_loop_sEry
                      (s'#_aoH3 `cast` ...)
                      (+# sc1_sErv 1#)
                      (+# sc2_sEru 1#)
                    }
                    }
                }; } in
          case $s$wfoldlM'_loop_sEry (ipv_aoGm `cast` ...) 0# 0#
          of _ [Occ=Dead] { (# ipv2_Xim4, ipv3_Xim6 #) ->
          case ipv3_Xim6 of _ [Occ=Dead] { I# dt6_aoHy ->
          case unsafeFreezeArray#
                 @ (Control.Monad.Primitive.PrimState IO)
                 @ Double
                 ipv1_aoGn
                 (ipv2_Xim4 `cast` ...)
          of _ [Occ=Dead] { (# ipv4_aoIe, ipv5_aoIf #) ->
          (# ipv4_aoIe `cast` ...,
             case runRW#
                    @ 'PtrRepLifted
                    @ (V.Vector Double)
                    (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                       case newArray#
                              @ Double
                              @ (Control.Monad.Primitive.PrimState
                                   (ST RealWorld))
                              dt6_aoHy
                              (Data.Vector.Mutable.uninitialised @ Double)
                              (s1_ah7U `cast` ...)
                       of _ [Occ=Dead] { (# ipv6_XoPs, ipv7_XoPu #) ->
                       let {
                         ds_agwf [Dmd=<L,U(U)>] :: Double

                         ds_agwf =
                           letrec {
                             $w$s$wfoldrM_loop_synL [InlPrag=[0], Occ=LoopBreaker]
                               :: Double# -> Int# -> Double#

                             $w$s$wfoldrM_loop_synL =
                               \ (ww1_synG :: Double#) (w2_synD :: Int#) ->
                                 case tagToEnum# @ Bool (>=# w2_synD dt6_aoHy)
                                 of _ [Occ=Dead] {
                                   False ->
                                     case indexArray# @ Double ipv5_aoIf w2_synD
                                     of _ [Occ=Dead] { (# ipv8_asRm #) ->
                                     case ipv8_asRm of _ [Occ=Dead] { D# y_aibp ->
                                     $w$s$wfoldrM_loop_synL
                                       (+## ww1_synG y_aibp) (+# w2_synD 1#)
                                     }
                                     };
                                   True -> ww1_synG
                                 }; } in
                           case $w$s$wfoldrM_loop_synL 0.0## 0# of ww1_synK { __DEFAULT ->
                           D# ww1_synK
                           } } in
                       letrec {
                         $s$wfoldlM'_loop1_sErp [Occ=LoopBreaker]
                           :: State# RealWorld
                              -> Int#
                              -> Int#
                              -> (# State# RealWorld, Int #)

                         $s$wfoldlM'_loop1_sErp =
                           \ (sc_sEro [OS=OneShot] :: State# RealWorld)
                             (sc1_sErm :: Int#)
                             (sc2_sErl :: Int#) ->
                             case tagToEnum# @ Bool (>=# sc1_sErm dt6_aoHy)
                             of _ [Occ=Dead] {
                               False ->
                                 case writeArray#
                                        @ (Control.Monad.Primitive.PrimState
                                             (ST RealWorld))
                                        @ Double
                                        ipv7_XoPu
                                        sc2_sErl
                                        (case indexArray# @ Double ipv5_aoIf sc1_sErm
                                         of _ [Occ=Dead] { (# ipv8_ai43 #) ->
                                         case ipv8_ai43 of _ [Occ=Dead] { D# x_aibM ->
                                         case ds_agwf of _ [Occ=Dead] { D# y_aibQ ->
                                         case /## x_aibM y_aibQ
                                         of wild3_aibS { __DEFAULT ->
                                         D# wild3_aibS
                                         }
                                         }
                                         }
                                         })
                                        (sc_sEro `cast` ...)
                                 of s'#_aoH3 [OS=OneShot] { __DEFAULT ->
                                 $s$wfoldlM'_loop1_sErp
                                   (s'#_aoH3 `cast` ...)
                                   (+# sc1_sErm 1#)
                                   (+# sc2_sErl 1#)
                                 };
                               True -> (# sc_sEro, I# sc2_sErl #)
                             }; } in
                       case $s$wfoldlM'_loop1_sErp (ipv6_XoPs `cast` ...) 0# 0#
                       of _ [Occ=Dead] { (# ipv8_ah4v, ipv9_ah4w #) ->
                       case ipv9_ah4w of _ [Occ=Dead] { I# dt2_XoR4 ->
                       case unsafeFreezeArray#
                              @ (Control.Monad.Primitive.PrimState
                                   (ST RealWorld))
                              @ Double
                              ipv7_XoPu
                              (ipv8_ah4v `cast` ...)
                       of _ [Occ=Dead] { (# ipv10_XoQ1, ipv11_XoQ3 #) ->
                       (# ipv10_XoQ1 `cast` ...,
                          Data.Vector.Vector @ Double 0# dt2_XoR4 ipv11_XoQ3 #)
                       }
                       }
                       }
                       })
             of _ [Occ=Dead] { (# ipv6_ah84, ipv7_ah85 #) ->
             ipv7_ah85
             } #)
          }
          }
          }
          } } in
    case tagToEnum# @ Bool (<=# ww_syo5 0#)
    of _ [Occ=Dead] {
      False -> $j_stpi ww_syo5;
      True -> $j_stpi 0#
    }

-- RHS size: {terms: 10, types: 6, coercions: 0}
labelPrior1 [InlPrag=INLINE[0]]
  :: Int
     -> MWC.GenIO
     -> State# RealWorld
     -> (# State# RealWorld, V.Vector Double #)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_syo0 [Occ=Once!] :: Int)
                 (w1_syo1 [Occ=Once] :: MWC.GenIO)
                 (w2_syo2 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case w_syo0 of _ [Occ=Dead] { I# ww1_syo5 [Occ=Once] ->
                 $wlabelPrior ww1_syo5 w1_syo1 w2_syo2
                 }}]
labelPrior1 =
  \ (w_syo0 :: Int)
    (w1_syo1 :: MWC.GenIO)
    (w2_syo2 [OS=OneShot] :: State# RealWorld) ->
    case w_syo0 of _ [Occ=Dead] { I# ww1_syo5 ->
    $wlabelPrior ww1_syo5 w1_syo1 w2_syo2
    }

-- RHS size: {terms: 1, types: 0, coercions: 8}
labelPrior :: Int -> MWC.GenIO -> IO (V.Vector Double)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= labelPrior1 `cast` ...}]
labelPrior = labelPrior1 `cast` ...

-- RHS size: {terms: 1, types: 0, coercions: 0}
vocabHP :: Double
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)}]
vocabHP = labelHP

-- RHS size: {terms: 2,977, types: 1,446, coercions: 178}
$wvocabPrior [InlPrag=[0]]
  :: Int#
     -> MWC.GenIO
     -> State# RealWorld
     -> (# State# RealWorld, V.Vector Double #)

$wvocabPrior =
  \ (ww_sypf :: Int#)
    (w_sypb :: MWC.GenIO)
    (w1_sypc [OS=OneShot] :: State# RealWorld) ->
    let {
      $j_stuk
        :: Int#
           -> (# State# RealWorld, V.Vector Double #)

      $j_stuk =
        \ (n#_aoGg [OS=OneShot] :: Int#) ->
          case newArray#
                 @ Double
                 @ (Control.Monad.Primitive.PrimState IO)
                 n#_aoGg
                 (Data.Vector.Mutable.uninitialised @ Double)
                 (w1_sypc `cast` ...)
          of _ [Occ=Dead] { (# ipv_aoGm, ipv1_aoGn #) ->
          letrec {
            mainloop_sB65 [Occ=LoopBreaker]
              :: State# RealWorld
                 -> (# State# RealWorld, Double #)

            mainloop_sB65 =
              \ (s_XiKw [OS=OneShot] :: State# RealWorld) ->
                letrec {
                  innerloop_skxg [Occ=LoopBreaker]
                    :: State# RealWorld
                       -> (# State# RealWorld,
                             System.Random.MWC.Distributions.T #)

                  innerloop_skxg =
                    \ (s1_XiKu [OS=OneShot] :: State# RealWorld) ->
                      letrec {
                        loop_skuH [Occ=LoopBreaker]
                          :: State# RealWorld
                             -> (# State# RealWorld, Double #)

                        loop_skuH =
                          \ (s2_XiKv [OS=OneShot] :: State# RealWorld) ->
                            case w_sypb `cast` ...
                            of _ [Occ=Dead]
                            { Data.Vector.Primitive.Mutable.MVector dt_ag4P dt1_ag4Q
                                                                    dt2_ag4R ->
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P 256#)
                                   (s2_XiKv `cast` ...)
                            of _ [Occ=Dead] { (# ipv2_ahoH, ipv3_ahoI #) ->
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P 257#)
                                   ipv2_ahoH
                            of _ [Occ=Dead] { (# ipv4_XhUT, ipv5_XhUV #) ->
                            let {
                              ipv6_sk7S [Dmd=<S,U>] :: Int#

                              ipv6_sk7S =
                                word2Int#
                                  (narrow8Word# (plusWord# ipv3_ahoI 1##)) } in
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P ipv6_sk7S)
                                   ipv4_XhUT
                            of _ [Occ=Dead] { (# ipv7_XhUU, ipv8_XhUW #) ->
                            let {
                              j_sk7T [Dmd=<S,U>] :: Int#

                              j_sk7T =
                                word2Int#
                                  (narrow8Word#
                                     (int2Word# (+# ipv6_sk7S 1#))) } in
                            case readWord32Array#
                                   @ (Control.Monad.Primitive.PrimState IO)
                                   dt2_ag4R
                                   (+# dt_ag4P j_sk7T)
                                   ipv7_XhUU
                            of _ [Occ=Dead] { (# ipv9_XhUZ, ipv10_XhV1 #) ->
                            let {
                              t1_sk7x [Dmd=<S,U>] :: Word#

                              t1_sk7x =
                                plusWord#
                                  (timesWord# 1540315826## ipv8_XhUW) ipv5_XhUV } in
                            let {
                              c'_sk7y [Dmd=<S,U>] :: Word#

                              c'_sk7y =
                                narrow32Word#
                                  (uncheckedShiftRL# t1_sk7x 32#) } in
                            let {
                              x1_sk7z [Dmd=<S,U>] :: Word#

                              x1_sk7z =
                                narrow32Word#
                                  (plusWord# (narrow32Word# t1_sk7x) c'_sk7y) } in
                            let {
                              $w$j_syov [InlPrag=[0]]
                                :: State# RealWorld
                                   -> Double#
                                   -> (# State# RealWorld, Double #)

                              $w$j_syov =
                                \ (w2_syop [OS=OneShot] :: State# RealWorld)
                                  (ww1_syot [OS=OneShot] :: Double#) ->
                                  case readWord32Array#
                                         @ (Control.Monad.Primitive.PrimState IO)
                                         dt2_ag4R
                                         (+# dt_ag4P 256#)
                                         (w2_syop `cast` ...)
                                  of _ [Occ=Dead] { (# ipv11_XhVI, ipv12_XhVK #) ->
                                  case readWord32Array#
                                         @ (Control.Monad.Primitive.PrimState IO)
                                         dt2_ag4R
                                         (+# dt_ag4P 257#)
                                         ipv11_XhVI
                                  of _ [Occ=Dead] { (# ipv13_XhUJ, ipv14_XhUL #) ->
                                  let {
                                    ipv15_skaA [Dmd=<S,U>] :: Int#

                                    ipv15_skaA =
                                      word2Int#
                                        (narrow8Word#
                                           (plusWord# ipv12_XhVK 1##)) } in
                                  case readWord32Array#
                                         @ (Control.Monad.Primitive.PrimState IO)
                                         dt2_ag4R
                                         (+# dt_ag4P ipv15_skaA)
                                         ipv13_XhUJ
                                  of _ [Occ=Dead] { (# ipv16_XhUN, ipv17_XhUP #) ->
                                  let {
                                    t2_skao [Dmd=<S,U>] :: Word#

                                    t2_skao =
                                      plusWord#
                                        (timesWord# 1540315826## ipv17_XhUP)
                                        ipv14_XhUL } in
                                  let {
                                    c'1_skap [Dmd=<S,U>] :: Word#

                                    c'1_skap =
                                      narrow32Word#
                                        (uncheckedShiftRL# t2_skao 32#) } in
                                  let {
                                    x2_skaq [Dmd=<S,U>] :: Word#

                                    x2_skaq =
                                      narrow32Word#
                                        (plusWord#
                                           (narrow32Word# t2_skao) c'1_skap) } in
                                  let {
                                    $w$j1_syoo [InlPrag=[0]]
                                      :: State# RealWorld
                                         -> Word#
                                         -> (# State# RealWorld, Double #)

                                    $w$j1_syoo =
                                      \ (w3_syoi [OS=OneShot] :: State# RealWorld)
                                        (ww2_syom [OS=OneShot] :: Word#) ->
                                        let {
                                          $j1_skut
                                            :: Double#
                                               -> State# RealWorld
                                               -> (# State# RealWorld, Double #)

                                          $j1_skut =
                                            \ (x_ahqq [OS=OneShot] :: Double#)
                                              (eta_Xy4 [OS=OneShot]
                                                 :: State# RealWorld) ->
                                              case System.Random.MWC.Distributions.ratios `cast` ...
                                              of _ [Occ=Dead]
                                              { Data.Vector.Primitive.Vector dt4_ajiC dt5_ajiD
                                                                             dt6_ajiE ->
                                              let {
                                                y_agKz [Dmd=<S,U>] :: Int#

                                                y_agKz =
                                                  word2Int#
                                                    (and# ww2_syom 127##) } in
                                              case indexDoubleArray#
                                                     dt6_ajiE (+# dt4_ajiC y_agKz)
                                              of wild2_ajiO { __DEFAULT ->
                                              case tagToEnum#
                                                     @ Bool (<## x_ahqq wild2_ajiO)
                                              of _ [Occ=Dead] {
                                                False ->
                                                  case y_agKz of wild4_X7I {
                                                    __DEFAULT ->
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P 256#)
                                                             (eta_Xy4 `cast` ...)
                                                      of _ [Occ=Dead]
                                                      { (# ipv18_XhVQ, ipv19_XhVS #) ->
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P 257#)
                                                             ipv18_XhVQ
                                                      of _ [Occ=Dead]
                                                      { (# ipv20_XhVg, ipv21_XhVi #) ->
                                                      let {
                                                        ipv22_skeN [Dmd=<S,U>] :: Int#

                                                        ipv22_skeN =
                                                          word2Int#
                                                            (narrow8Word#
                                                               (plusWord#
                                                                  ipv19_XhVS 1##)) } in
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P ipv22_skeN)
                                                             ipv20_XhVg
                                                      of _ [Occ=Dead]
                                                      { (# ipv23_XhVh, ipv24_XhVj #) ->
                                                      let {
                                                        j1_skeO [Dmd=<S,U>] :: Int#

                                                        j1_skeO =
                                                          word2Int#
                                                            (narrow8Word#
                                                               (int2Word#
                                                                  (+# ipv22_skeN 1#))) } in
                                                      case readWord32Array#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  IO)
                                                             dt2_ag4R
                                                             (+# dt_ag4P j1_skeO)
                                                             ipv23_XhVh
                                                      of _ [Occ=Dead]
                                                      { (# ipv25_XhVm, ipv26_XhVo #) ->
                                                      let {
                                                        t3_skes [Dmd=<S,U>] :: Word#

                                                        t3_skes =
                                                          plusWord#
                                                            (timesWord#
                                                               1540315826## ipv24_XhVj)
                                                            ipv21_XhVi } in
                                                      let {
                                                        c'2_sket [Dmd=<S,U>] :: Word#

                                                        c'2_sket =
                                                          narrow32Word#
                                                            (uncheckedShiftRL#
                                                               t3_skes 32#) } in
                                                      let {
                                                        x3_skeu [Dmd=<S,U>] :: Word#

                                                        x3_skeu =
                                                          narrow32Word#
                                                            (plusWord#
                                                               (narrow32Word# t3_skes)
                                                               c'2_sket) } in
                                                      case tagToEnum#
                                                             @ Bool
                                                             (ltWord# x3_skeu c'2_sket)
                                                      of _ [Occ=Dead] {
                                                        False ->
                                                          let {
                                                            u1_snif [Dmd=<S,U>] :: Word#

                                                            u1_snif =
                                                              plusWord#
                                                                (timesWord#
                                                                   1540315826## ipv26_XhVo)
                                                                c'2_sket } in
                                                          let {
                                                            d'_snig [Dmd=<S,U>] :: Word#

                                                            d'_snig =
                                                              narrow32Word#
                                                                (uncheckedShiftRL#
                                                                   u1_snif 32#) } in
                                                          let {
                                                            y1_snih [Dmd=<S,U>] :: Word#

                                                            y1_snih =
                                                              narrow32Word#
                                                                (plusWord#
                                                                   (narrow32Word# u1_snif)
                                                                   d'_snig) } in
                                                          case tagToEnum#
                                                                 @ Bool
                                                                 (ltWord# y1_snih d'_snig)
                                                          of _ [Occ=Dead] {
                                                            False ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_skeN)
                                                                     x3_skeu
                                                                     ipv25_XhVm
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_skeO)
                                                                     y1_snih
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_skeO))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     d'_snig
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqq
                                                                                             dt8_Xjqs
                                                                                             dt9_Xjqu ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq wild4_X7I)
                                                              of wild8_XjtM { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_syot wild8_XjtM } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq
                                                                        (+# wild4_X7I 1#))
                                                              of wild9_XjqK { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqK
                                                                                wild9_XjqK)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                x3_skeu)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      y1_snih)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtM
                                                                                                wild8_XjtM)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_skuH (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              };
                                                            True ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_skeN)
                                                                     x3_skeu
                                                                     ipv25_XhVm
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              let {
                                                                x#_ageT [Dmd=<S,U>]
                                                                  :: Word#

                                                                x#_ageT =
                                                                  narrow32Word#
                                                                    (plusWord#
                                                                       y1_snih 1##) } in
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_skeO)
                                                                     x#_ageT
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_skeO))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     (narrow32Word#
                                                                        (plusWord#
                                                                           d'_snig 1##))
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqq
                                                                                             dt8_Xjqs
                                                                                             dt9_Xjqu ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq wild4_X7I)
                                                              of wild8_XjtO { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_syot wild8_XjtO } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq
                                                                        (+# wild4_X7I 1#))
                                                              of wild9_XjqK { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqK
                                                                                wild9_XjqK)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                x3_skeu)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      x#_ageT)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtO
                                                                                                wild8_XjtO)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_skuH (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                          };
                                                        True ->
                                                          let {
                                                            u1_sniI [Dmd=<S,U>] :: Word#

                                                            u1_sniI =
                                                              plusWord#
                                                                (timesWord#
                                                                   1540315826## ipv26_XhVo)
                                                                (narrow32Word#
                                                                   (plusWord#
                                                                      c'2_sket 1##)) } in
                                                          let {
                                                            d'_sniJ [Dmd=<S,U>] :: Word#

                                                            d'_sniJ =
                                                              narrow32Word#
                                                                (uncheckedShiftRL#
                                                                   u1_sniI 32#) } in
                                                          let {
                                                            y1_sniK [Dmd=<S,U>] :: Word#

                                                            y1_sniK =
                                                              narrow32Word#
                                                                (plusWord#
                                                                   (narrow32Word# u1_sniI)
                                                                   d'_sniJ) } in
                                                          let {
                                                            ipv27_sniH [Dmd=<S,U>] :: Word#

                                                            ipv27_sniH =
                                                              narrow32Word#
                                                                (plusWord#
                                                                   x3_skeu 1##) } in
                                                          case tagToEnum#
                                                                 @ Bool
                                                                 (ltWord# y1_sniK d'_sniJ)
                                                          of _ [Occ=Dead] {
                                                            False ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_skeN)
                                                                     ipv27_sniH
                                                                     ipv25_XhVm
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_skeO)
                                                                     y1_sniK
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_skeO))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     d'_sniJ
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqq
                                                                                             dt8_Xjqs
                                                                                             dt9_Xjqu ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq wild4_X7I)
                                                              of wild8_XjtO { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_syot wild8_XjtO } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq
                                                                        (+# wild4_X7I 1#))
                                                              of wild9_XjqK { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqK
                                                                                wild9_XjqK)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                ipv27_sniH)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      y1_sniK)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtO
                                                                                                wild8_XjtO)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_skuH (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              };
                                                            True ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+#
                                                                        dt_ag4P ipv22_skeN)
                                                                     ipv27_sniH
                                                                     ipv25_XhVm
                                                              of s'#_ageD [OS=OneShot]
                                                              { __DEFAULT ->
                                                              let {
                                                                x#_ageT [Dmd=<S,U>]
                                                                  :: Word#

                                                                x#_ageT =
                                                                  narrow32Word#
                                                                    (plusWord#
                                                                       y1_sniK 1##) } in
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P j1_skeO)
                                                                     x#_ageT
                                                                     s'#_ageD
                                                              of s'#1_ageV [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 256#)
                                                                     (narrow32Word#
                                                                        (int2Word#
                                                                           j1_skeO))
                                                                     s'#1_ageV
                                                              of s'#2_agfd [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case writeWord32Array#
                                                                     @ (Control.Monad.Primitive.PrimState
                                                                          IO)
                                                                     dt2_ag4R
                                                                     (+# dt_ag4P 257#)
                                                                     (narrow32Word#
                                                                        (plusWord#
                                                                           d'_sniJ 1##))
                                                                     s'#2_agfd
                                                              of s'#3_agfv [OS=OneShot]
                                                              { __DEFAULT ->
                                                              case System.Random.MWC.Distributions.blocks
                                                                   `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt7_Xjqq
                                                                                             dt8_Xjqs
                                                                                             dt9_Xjqu ->
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq wild4_X7I)
                                                              of wild8_XjtQ { __DEFAULT ->
                                                              let {
                                                                x4_aict [Dmd=<S,U>]
                                                                  :: Double#

                                                                x4_aict =
                                                                  *##
                                                                    ww1_syot wild8_XjtQ } in
                                                              let {
                                                                y2_aicn [Dmd=<S,U>]
                                                                  :: Double#

                                                                y2_aicn =
                                                                  *## x4_aict x4_aict } in
                                                              case indexDoubleArray#
                                                                     dt9_Xjqu
                                                                     (+#
                                                                        dt7_Xjqq
                                                                        (+# wild4_X7I 1#))
                                                              of wild9_XjqK { __DEFAULT ->
                                                              let {
                                                                x5_aibl [Dmd=<S,U>]
                                                                  :: Double#

                                                                x5_aibl =
                                                                  expDouble#
                                                                    (negateDouble#
                                                                       (*##
                                                                          0.5##
                                                                          (-##
                                                                             (*##
                                                                                wild9_XjqK
                                                                                wild9_XjqK)
                                                                             y2_aicn))) } in
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<##
                                                                        (+##
                                                                           x5_aibl
                                                                           (*##
                                                                              (+##
                                                                                 (+##
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (narrow32Int#
                                                                                             (word2Int#
                                                                                                ipv27_sniH)))
                                                                                       2.3283064365386963e-10##)
                                                                                    0.5000000000000001##)
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (word2Int#
                                                                                          (and#
                                                                                             (int2Word#
                                                                                                (narrow32Int#
                                                                                                   (word2Int#
                                                                                                      x#_ageT)))
                                                                                             1048575##)))
                                                                                    2.220446049250313e-16##))
                                                                              (-##
                                                                                 (expDouble#
                                                                                    (negateDouble#
                                                                                       (*##
                                                                                          0.5##
                                                                                          (-##
                                                                                             (*##
                                                                                                wild8_XjtQ
                                                                                                wild8_XjtQ)
                                                                                             y2_aicn))))
                                                                                 x5_aibl)))
                                                                        1.0##)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  loop_skuH (s'#3_agfv `cast` ...);
                                                                True ->
                                                                  (# s'#3_agfv `cast` ...,
                                                                     D# x4_aict #)
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                              }
                                                          }
                                                      }
                                                      }
                                                      }
                                                      }
                                                      };
                                                    0# ->
                                                      letrec {
                                                        tailing_skqe [Occ=LoopBreaker]
                                                          :: State# RealWorld
                                                             -> (# State#
                                                                     RealWorld,
                                                                   Double #)

                                                        tailing_skqe =
                                                          \ (s3_XiLX [OS=OneShot]
                                                               :: State#
                                                                    RealWorld) ->
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P 256#)
                                                                   (s3_XiLX `cast` ...)
                                                            of _ [Occ=Dead]
                                                            { (# ipv18_XhVM, ipv19_XhVO #) ->
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P 257#)
                                                                   ipv18_XhVM
                                                            of _ [Occ=Dead]
                                                            { (# ipv20_XhVc, ipv21_XhVe #) ->
                                                            let {
                                                              ipv22_skjj [Dmd=<S,U>]
                                                                :: Int#

                                                              ipv22_skjj =
                                                                word2Int#
                                                                  (narrow8Word#
                                                                     (plusWord#
                                                                        ipv19_XhVO 1##)) } in
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P ipv22_skjj)
                                                                   ipv20_XhVc
                                                            of _ [Occ=Dead]
                                                            { (# ipv23_XhVd, ipv24_XhVf #) ->
                                                            let {
                                                              j1_skjk [Dmd=<S,U>] :: Int#

                                                              j1_skjk =
                                                                word2Int#
                                                                  (narrow8Word#
                                                                     (int2Word#
                                                                        (+#
                                                                           ipv22_skjj 1#))) } in
                                                            case readWord32Array#
                                                                   @ (Control.Monad.Primitive.PrimState
                                                                        IO)
                                                                   dt2_ag4R
                                                                   (+# dt_ag4P j1_skjk)
                                                                   ipv23_XhVd
                                                            of _ [Occ=Dead]
                                                            { (# ipv25_XhVi, ipv26_XhVk #) ->
                                                            let {
                                                              t3_skiY [Dmd=<S,U>] :: Word#

                                                              t3_skiY =
                                                                plusWord#
                                                                  (timesWord#
                                                                     1540315826## ipv24_XhVf)
                                                                  ipv21_XhVe } in
                                                            let {
                                                              c'2_skiZ [Dmd=<S,U>] :: Word#

                                                              c'2_skiZ =
                                                                narrow32Word#
                                                                  (uncheckedShiftRL#
                                                                     t3_skiY 32#) } in
                                                            let {
                                                              x3_skj0 [Dmd=<S,U>] :: Word#

                                                              x3_skj0 =
                                                                narrow32Word#
                                                                  (plusWord#
                                                                     (narrow32Word#
                                                                        t3_skiY)
                                                                     c'2_skiZ) } in
                                                            let {
                                                              $w$j2_syoe [InlPrag=[0]]
                                                                :: State#
                                                                     RealWorld
                                                                   -> Double#
                                                                   -> (# State#
                                                                           RealWorld,
                                                                         Double #)
                                                              [LclId,
                                                               Arity=2,

                                                              $w$j2_syoe =
                                                                \ (w4_syo8 [OS=OneShot]
                                                                     :: State#
                                                                          RealWorld)
                                                                  (ww3_syoc [OS=OneShot]
                                                                     :: Double#) ->
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+# dt_ag4P 256#)
                                                                         (w4_syo8 `cast` ...)
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv27_XhW3, ipv28_XhW5 #) ->
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+# dt_ag4P 257#)
                                                                         ipv27_XhW3
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv29_XisS, ipv30_XhVg #) ->
                                                                  let {
                                                                    ipv31_sknJ [Dmd=<S,U>]
                                                                      :: Int#

                                                                    ipv31_sknJ =
                                                                      word2Int#
                                                                        (narrow8Word#
                                                                           (plusWord#
                                                                              ipv28_XhW5 1##)) } in
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+#
                                                                            dt_ag4P ipv31_sknJ)
                                                                         ipv29_XisS
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv32_Xit5, ipv33_XhVh #) ->
                                                                  let {
                                                                    j2_sknK [Dmd=<S,U>]
                                                                      :: Int#

                                                                    j2_sknK =
                                                                      word2Int#
                                                                        (narrow8Word#
                                                                           (int2Word#
                                                                              (+#
                                                                                 ipv31_sknJ
                                                                                 1#))) } in
                                                                  case readWord32Array#
                                                                         @ (Control.Monad.Primitive.PrimState
                                                                              IO)
                                                                         dt2_ag4R
                                                                         (+#
                                                                            dt_ag4P j2_sknK)
                                                                         ipv32_Xit5
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv34_Xitm, ipv35_XhVm #) ->
                                                                  let {
                                                                    t4_skno [Dmd=<S,U>]
                                                                      :: Word#

                                                                    t4_skno =
                                                                      plusWord#
                                                                        (timesWord#
                                                                           1540315826## ipv33_XhVh)
                                                                        ipv30_XhVg } in
                                                                  let {
                                                                    c'3_sknp [Dmd=<S,U>]
                                                                      :: Word#

                                                                    c'3_sknp =
                                                                      narrow32Word#
                                                                        (uncheckedShiftRL#
                                                                           t4_skno 32#) } in
                                                                  let {
                                                                    x4_sknq [Dmd=<S,U>]
                                                                      :: Word#

                                                                    x4_sknq =
                                                                      narrow32Word#
                                                                        (plusWord#
                                                                           (narrow32Word#
                                                                              t4_skno)
                                                                           c'3_sknp) } in
                                                                  case tagToEnum#
                                                                         @ Bool
                                                                         (ltWord#
                                                                            x4_sknq c'3_sknp)
                                                                  of _ [Occ=Dead] {
                                                                    False ->
                                                                      let {
                                                                        u1_snjV [Dmd=<S,U>]
                                                                          :: Word#

                                                                        u1_snjV =
                                                                          plusWord#
                                                                            (timesWord#
                                                                               1540315826##
                                                                               ipv35_XhVm)
                                                                            c'3_sknp } in
                                                                      let {
                                                                        d'_snjW [Dmd=<S,U>]
                                                                          :: Word#

                                                                        d'_snjW =
                                                                          narrow32Word#
                                                                            (uncheckedShiftRL#
                                                                               u1_snjV 32#) } in
                                                                      let {
                                                                        y1_snjX [Dmd=<S,U>]
                                                                          :: Word#

                                                                        y1_snjX =
                                                                          narrow32Word#
                                                                            (plusWord#
                                                                               (narrow32Word#
                                                                                  u1_snjV)
                                                                               d'_snjW) } in
                                                                      case tagToEnum#
                                                                             @ Bool
                                                                             (ltWord#
                                                                                y1_snjX d'_snjW)
                                                                      of _ [Occ=Dead] {
                                                                        False ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sknJ)
                                                                                 x4_sknq
                                                                                 ipv34_Xitm
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sknK)
                                                                                 y1_snjX
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sknK))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 d'_snjW
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x4_sknq)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         y1_snjX)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syoc
                                                                                       ww3_syoc))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_syot
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syoc) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syoc
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_skqe
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          };
                                                                        True ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sknJ)
                                                                                 x4_sknq
                                                                                 ipv34_Xitm
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          let {
                                                                            x#_agmz [Dmd=<S,U>]
                                                                              :: Word#

                                                                            x#_agmz =
                                                                              narrow32Word#
                                                                                (plusWord#
                                                                                   y1_snjX 1##) } in
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sknK)
                                                                                 x#_agmz
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sknK))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 (narrow32Word#
                                                                                    (plusWord#
                                                                                       d'_snjW 1##))
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x4_sknq)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         x#_agmz)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syoc
                                                                                       ww3_syoc))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_syot
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syoc) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syoc
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_skqe
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                      };
                                                                    True ->
                                                                      let {
                                                                        u1_snko [Dmd=<S,U>]
                                                                          :: Word#

                                                                        u1_snko =
                                                                          plusWord#
                                                                            (timesWord#
                                                                               1540315826##
                                                                               ipv35_XhVm)
                                                                            (narrow32Word#
                                                                               (plusWord#
                                                                                  c'3_sknp
                                                                                  1##)) } in
                                                                      let {
                                                                        d'_snkp [Dmd=<S,U>]
                                                                          :: Word#

                                                                        d'_snkp =
                                                                          narrow32Word#
                                                                            (uncheckedShiftRL#
                                                                               u1_snko 32#) } in
                                                                      let {
                                                                        y1_snkq [Dmd=<S,U>]
                                                                          :: Word#

                                                                        y1_snkq =
                                                                          narrow32Word#
                                                                            (plusWord#
                                                                               (narrow32Word#
                                                                                  u1_snko)
                                                                               d'_snkp) } in
                                                                      let {
                                                                        ipv36_snkn [Dmd=<S,U>]
                                                                          :: Word#

                                                                        ipv36_snkn =
                                                                          narrow32Word#
                                                                            (plusWord#
                                                                               x4_sknq 1##) } in
                                                                      case tagToEnum#
                                                                             @ Bool
                                                                             (ltWord#
                                                                                y1_snkq d'_snkp)
                                                                      of _ [Occ=Dead] {
                                                                        False ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sknJ)
                                                                                 ipv36_snkn
                                                                                 ipv34_Xitm
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sknK)
                                                                                 y1_snkq
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sknK))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 d'_snkp
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   ipv36_snkn)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         y1_snkq)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syoc
                                                                                       ww3_syoc))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_syot
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syoc) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syoc
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_skqe
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          };
                                                                        True ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P
                                                                                    ipv31_sknJ)
                                                                                 ipv36_snkn
                                                                                 ipv34_Xitm
                                                                          of s'#_agmj [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          let {
                                                                            x#_agmz [Dmd=<S,U>]
                                                                              :: Word#

                                                                            x#_agmz =
                                                                              narrow32Word#
                                                                                (plusWord#
                                                                                   y1_snkq 1##) } in
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P j2_sknK)
                                                                                 x#_agmz
                                                                                 s'#_agmj
                                                                          of s'#1_agmB [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 256#)
                                                                                 (narrow32Word#
                                                                                    (int2Word#
                                                                                       j2_sknK))
                                                                                 s'#1_agmB
                                                                          of s'#2_agmT [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case writeWord32Array#
                                                                                 @ (Control.Monad.Primitive.PrimState
                                                                                      IO)
                                                                                 dt2_ag4R
                                                                                 (+#
                                                                                    dt_ag4P 257#)
                                                                                 (narrow32Word#
                                                                                    (plusWord#
                                                                                       d'_snkp 1##))
                                                                                 s'#2_agmT
                                                                          of s'#3_agnb [OS=OneShot]
                                                                          { __DEFAULT ->
                                                                          case logDouble#
                                                                                 (+##
                                                                                    (+##
                                                                                       (*##
                                                                                          (int2Double#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   ipv36_snkn)))
                                                                                          2.3283064365386963e-10##)
                                                                                       0.5000000000000001##)
                                                                                    (*##
                                                                                       (int2Double#
                                                                                          (word2Int#
                                                                                             (and#
                                                                                                (int2Word#
                                                                                                   (narrow32Int#
                                                                                                      (word2Int#
                                                                                                         x#_agmz)))
                                                                                                1048575##)))
                                                                                       2.220446049250313e-16##))
                                                                          of wild7_aibY
                                                                          { __DEFAULT ->
                                                                          case tagToEnum#
                                                                                 @ Bool
                                                                                 (<##
                                                                                    (*##
                                                                                       wild7_aibY
                                                                                       -2.0##)
                                                                                    (*##
                                                                                       ww3_syoc
                                                                                       ww3_syoc))
                                                                          of _ [Occ=Dead] {
                                                                            False ->
                                                                              case tagToEnum#
                                                                                     @ Bool
                                                                                     (<##
                                                                                        ww1_syot
                                                                                        0.0##)
                                                                              of _ [Occ=Dead] {
                                                                                False ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          3.442619855899##
                                                                                          ww3_syoc) #);
                                                                                True ->
                                                                                  (# s'#3_agnb
                                                                                     `cast` ...,
                                                                                     D#
                                                                                       (-##
                                                                                          ww3_syoc
                                                                                          3.442619855899##) #)
                                                                              };
                                                                            True ->
                                                                              tailing_skqe
                                                                                (s'#3_agnb
                                                                                 `cast` ...)
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                          }
                                                                      }
                                                                  }
                                                                  }
                                                                  }
                                                                  }
                                                                  } } in
                                                            case tagToEnum#
                                                                   @ Bool
                                                                   (ltWord#
                                                                      x3_skj0 c'2_skiZ)
                                                            of _ [Occ=Dead] {
                                                              False ->
                                                                let {
                                                                  u1_snkR [Dmd=<S,U>]
                                                                    :: Word#

                                                                  u1_snkR =
                                                                    plusWord#
                                                                      (timesWord#
                                                                         1540315826## ipv26_XhVk)
                                                                      c'2_skiZ } in
                                                                let {
                                                                  d'_snkS [Dmd=<S,U>]
                                                                    :: Word#

                                                                  d'_snkS =
                                                                    narrow32Word#
                                                                      (uncheckedShiftRL#
                                                                         u1_snkR 32#) } in
                                                                let {
                                                                  y1_snkT [Dmd=<S,U>]
                                                                    :: Word#

                                                                  y1_snkT =
                                                                    narrow32Word#
                                                                      (plusWord#
                                                                         (narrow32Word#
                                                                            u1_snkR)
                                                                         d'_snkS) } in
                                                                case tagToEnum#
                                                                       @ Bool
                                                                       (ltWord#
                                                                          y1_snkT d'_snkS)
                                                                of _ [Occ=Dead] {
                                                                  False ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_skjj)
                                                                           x3_skj0
                                                                           ipv25_XhVi
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_skjk)
                                                                           y1_snkT
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_skjk))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           d'_snkS
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             x3_skj0)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   y1_snkT)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syoe
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    };
                                                                  True ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_skjj)
                                                                           x3_skj0
                                                                           ipv25_XhVi
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    let {
                                                                      x#_agiP [Dmd=<S,U>]
                                                                        :: Word#

                                                                      x#_agiP =
                                                                        narrow32Word#
                                                                          (plusWord#
                                                                             y1_snkT 1##) } in
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_skjk)
                                                                           x#_agiP
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_skjk))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           (narrow32Word#
                                                                              (plusWord#
                                                                                 d'_snkS 1##))
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             x3_skj0)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x#_agiP)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syoe
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                };
                                                              True ->
                                                                let {
                                                                  u1_snlb [Dmd=<S,U>]
                                                                    :: Word#

                                                                  u1_snlb =
                                                                    plusWord#
                                                                      (timesWord#
                                                                         1540315826## ipv26_XhVk)
                                                                      (narrow32Word#
                                                                         (plusWord#
                                                                            c'2_skiZ 1##)) } in
                                                                let {
                                                                  d'_snlc [Dmd=<S,U>]
                                                                    :: Word#

                                                                  d'_snlc =
                                                                    narrow32Word#
                                                                      (uncheckedShiftRL#
                                                                         u1_snlb 32#) } in
                                                                let {
                                                                  y1_snld [Dmd=<S,U>]
                                                                    :: Word#

                                                                  y1_snld =
                                                                    narrow32Word#
                                                                      (plusWord#
                                                                         (narrow32Word#
                                                                            u1_snlb)
                                                                         d'_snlc) } in
                                                                let {
                                                                  ipv27_snla [Dmd=<S,U>]
                                                                    :: Word#

                                                                  ipv27_snla =
                                                                    narrow32Word#
                                                                      (plusWord#
                                                                         x3_skj0 1##) } in
                                                                case tagToEnum#
                                                                       @ Bool
                                                                       (ltWord#
                                                                          y1_snld d'_snlc)
                                                                of _ [Occ=Dead] {
                                                                  False ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_skjj)
                                                                           ipv27_snla
                                                                           ipv25_XhVi
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_skjk)
                                                                           y1_snld
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_skjk))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           d'_snlc
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             ipv27_snla)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   y1_snld)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syoe
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    };
                                                                  True ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P ipv22_skjj)
                                                                           ipv27_snla
                                                                           ipv25_XhVi
                                                                    of s'#_agiz [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    let {
                                                                      x#_agiP [Dmd=<S,U>]
                                                                        :: Word#

                                                                      x#_agiP =
                                                                        narrow32Word#
                                                                          (plusWord#
                                                                             y1_snld 1##) } in
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P j1_skjk)
                                                                           x#_agiP
                                                                           s'#_agiz
                                                                    of s'#1_agiR [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 256#)
                                                                           (narrow32Word#
                                                                              (int2Word#
                                                                                 j1_skjk))
                                                                           s'#1_agiR
                                                                    of s'#2_agj9 [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case writeWord32Array#
                                                                           @ (Control.Monad.Primitive.PrimState
                                                                                IO)
                                                                           dt2_ag4R
                                                                           (+#
                                                                              dt_ag4P 257#)
                                                                           (narrow32Word#
                                                                              (plusWord#
                                                                                 d'_snlc 1##))
                                                                           s'#2_agj9
                                                                    of s'#3_agjr [OS=OneShot]
                                                                    { __DEFAULT ->
                                                                    case logDouble#
                                                                           (+##
                                                                              (+##
                                                                                 (*##
                                                                                    (int2Double#
                                                                                       (narrow32Int#
                                                                                          (word2Int#
                                                                                             ipv27_snla)))
                                                                                    2.3283064365386963e-10##)
                                                                                 0.5000000000000001##)
                                                                              (*##
                                                                                 (int2Double#
                                                                                    (word2Int#
                                                                                       (and#
                                                                                          (int2Word#
                                                                                             (narrow32Int#
                                                                                                (word2Int#
                                                                                                   x#_agiP)))
                                                                                          1048575##)))
                                                                                 2.220446049250313e-16##))
                                                                    of wild7_aibY { __DEFAULT ->
                                                                    $w$j2_syoe
                                                                      (s'#3_agjr `cast` ...)
                                                                      (/##
                                                                         wild7_aibY
                                                                         3.442619855899##)
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                    }
                                                                }
                                                            }
                                                            }
                                                            }
                                                            }
                                                            }; } in
                                                      tailing_skqe eta_Xy4
                                                  };
                                                True ->
                                                  case System.Random.MWC.Distributions.blocks
                                                       `cast` ...
                                                  of _ [Occ=Dead]
                                                  { Data.Vector.Primitive.Vector dt7_XjsC dt8_XjsE
                                                                                 dt9_XjsG ->
                                                  case indexDoubleArray#
                                                         dt9_XjsG (+# dt7_XjsC y_agKz)
                                                  of wild5_XjsU { __DEFAULT ->
                                                  (# eta_Xy4,
                                                     D#
                                                       (*## ww1_syot wild5_XjsU) #)
                                                  }
                                                  }
                                              }
                                              }
                                              } } in
                                        case tagToEnum#
                                               @ Bool (==## ww1_syot 0.0##)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case tagToEnum#
                                                   @ Bool (>## ww1_syot 0.0##)
                                            of _ [Occ=Dead] {
                                              False ->
                                                $j1_skut (negateDouble# ww1_syot) w3_syoi;
                                              True -> $j1_skut ww1_syot w3_syoi
                                            };
                                          True -> $j1_skut 0.0## w3_syoi
                                        } } in
                                  case tagToEnum#
                                         @ Bool (ltWord# x2_skaq c'1_skap)
                                  of _ [Occ=Dead] {
                                    False ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P ipv15_skaA)
                                             x2_skaq
                                             ipv16_XhUN
                                      of s'#_agb2 [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 256#)
                                             (narrow32Word#
                                                (int2Word# ipv15_skaA))
                                             s'#_agb2
                                      of s'#1_agbk [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 257#)
                                             c'1_skap
                                             s'#1_agbk
                                      of s'#2_agbC [OS=OneShot] { __DEFAULT ->
                                      $w$j1_syoo (s'#2_agbC `cast` ...) x2_skaq
                                      }
                                      }
                                      };
                                    True ->
                                      let {
                                        x#_agb0 [Dmd=<S,U>] :: Word#

                                        x#_agb0 =
                                          narrow32Word#
                                            (plusWord# x2_skaq 1##) } in
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P ipv15_skaA)
                                             x#_agb0
                                             ipv16_XhUN
                                      of s'#_agb2 [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 256#)
                                             (narrow32Word#
                                                (int2Word# ipv15_skaA))
                                             s'#_agb2
                                      of s'#1_agbk [OS=OneShot] { __DEFAULT ->
                                      case writeWord32Array#
                                             @ (Control.Monad.Primitive.PrimState IO)
                                             dt2_ag4R
                                             (+# dt_ag4P 257#)
                                             (narrow32Word#
                                                (plusWord# c'1_skap 1##))
                                             s'#1_agbk
                                      of s'#2_agbC [OS=OneShot] { __DEFAULT ->
                                      $w$j1_syoo (s'#2_agbC `cast` ...) x#_agb0
                                      }
                                      }
                                      }
                                  }
                                  }
                                  }
                                  } } in
                            case tagToEnum# @ Bool (ltWord# x1_sk7z c'_sk7y)
                            of _ [Occ=Dead] {
                              False ->
                                let {
                                  u_snlF [Dmd=<S,U>] :: Word#

                                  u_snlF =
                                    plusWord#
                                      (timesWord# 1540315826## ipv10_XhV1) c'_sk7y } in
                                let {
                                  d'_snlG [Dmd=<S,U>] :: Word#

                                  d'_snlG =
                                    narrow32Word#
                                      (uncheckedShiftRL# u_snlF 32#) } in
                                let {
                                  y_snlH [Dmd=<S,U>] :: Word#

                                  y_snlH =
                                    narrow32Word#
                                      (plusWord#
                                         (narrow32Word# u_snlF) d'_snlG) } in
                                case tagToEnum# @ Bool (ltWord# y_snlH d'_snlG)
                                of _ [Occ=Dead] {
                                  False ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_sk7S)
                                           x1_sk7z
                                           ipv9_XhUZ
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_sk7T)
                                           y_snlH
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_sk7T))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           d'_snlG
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv11_snlP [Dmd=<S,U>] :: Double#

                                      ipv11_snlP =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# x1_sk7z)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# y_snlH)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_syov
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv11_snlP ipv11_snlP) 1.0##)
                                    }
                                    }
                                    }
                                    };
                                  True ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_sk7S)
                                           x1_sk7z
                                           ipv9_XhUZ
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    let {
                                      x#_ag7E [Dmd=<S,U>] :: Word#

                                      x#_ag7E =
                                        narrow32Word# (plusWord# y_snlH 1##) } in
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_sk7T)
                                           x#_ag7E
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_sk7T))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           (narrow32Word# (plusWord# d'_snlG 1##))
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv11_snlU [Dmd=<S,U>] :: Double#

                                      ipv11_snlU =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# x1_sk7z)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# x#_ag7E)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_syov
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv11_snlU ipv11_snlU) 1.0##)
                                    }
                                    }
                                    }
                                    }
                                };
                              True ->
                                let {
                                  u_snlW [Dmd=<S,U>] :: Word#

                                  u_snlW =
                                    plusWord#
                                      (timesWord# 1540315826## ipv10_XhV1)
                                      (narrow32Word# (plusWord# c'_sk7y 1##)) } in
                                let {
                                  d'_snlX [Dmd=<S,U>] :: Word#

                                  d'_snlX =
                                    narrow32Word#
                                      (uncheckedShiftRL# u_snlW 32#) } in
                                let {
                                  y_snlY [Dmd=<S,U>] :: Word#

                                  y_snlY =
                                    narrow32Word#
                                      (plusWord#
                                         (narrow32Word# u_snlW) d'_snlX) } in
                                let {
                                  ipv11_snlV [Dmd=<S,U>] :: Word#

                                  ipv11_snlV =
                                    narrow32Word# (plusWord# x1_sk7z 1##) } in
                                case tagToEnum# @ Bool (ltWord# y_snlY d'_snlX)
                                of _ [Occ=Dead] {
                                  False ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_sk7S)
                                           ipv11_snlV
                                           ipv9_XhUZ
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_sk7T)
                                           y_snlY
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_sk7T))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           d'_snlX
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv12_snm6 [Dmd=<S,U>] :: Double#

                                      ipv12_snm6 =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# ipv11_snlV)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# y_snlY)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_syov
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv12_snm6 ipv12_snm6) 1.0##)
                                    }
                                    }
                                    }
                                    };
                                  True ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P ipv6_sk7S)
                                           ipv11_snlV
                                           ipv9_XhUZ
                                    of s'#_ag7o [OS=OneShot] { __DEFAULT ->
                                    let {
                                      x#_ag7E [Dmd=<S,U>] :: Word#

                                      x#_ag7E =
                                        narrow32Word# (plusWord# y_snlY 1##) } in
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P j_sk7T)
                                           x#_ag7E
                                           s'#_ag7o
                                    of s'#1_ag7G [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 256#)
                                           (narrow32Word# (int2Word# j_sk7T))
                                           s'#1_ag7G
                                    of s'#2_ag7Y [OS=OneShot] { __DEFAULT ->
                                    case writeWord32Array#
                                           @ (Control.Monad.Primitive.PrimState IO)
                                           dt2_ag4R
                                           (+# dt_ag4P 257#)
                                           (narrow32Word# (plusWord# d'_snlX 1##))
                                           s'#2_ag7Y
                                    of s'#3_ag8g [OS=OneShot] { __DEFAULT ->
                                    let {
                                      ipv12_snmb [Dmd=<S,U>] :: Double#

                                      ipv12_snmb =
                                        +##
                                          (+##
                                             (*##
                                                (int2Double#
                                                   (narrow32Int#
                                                      (word2Int# ipv11_snlV)))
                                                2.3283064365386963e-10##)
                                             0.5000000000000001##)
                                          (*##
                                             (int2Double#
                                                (word2Int#
                                                   (and#
                                                      (int2Word#
                                                         (narrow32Int#
                                                            (word2Int# x#_ag7E)))
                                                      1048575##)))
                                             2.220446049250313e-16##) } in
                                    $w$j_syov
                                      (s'#3_ag8g `cast` ...)
                                      (-## (+## ipv12_snmb ipv12_snmb) 1.0##)
                                    }
                                    }
                                    }
                                    }
                                }
                            }
                            }
                            }
                            }
                            }
                            }; } in
                      case loop_skuH s1_XiKu
                      of _ [Occ=Dead] { (# ipv2_aif4, ipv3_aif5 #) ->
                      case ipv3_aif5 of _ [Occ=Dead] { D# y_Xiis ->
                      case /## 1.0## (sqrtDouble# 6.000000000000001##)
                      of wild2_aibS { __DEFAULT ->
                      let {
                        x_ahpC [Dmd=<S,U>] :: Double#

                        x_ahpC = +## 1.0## (*## wild2_aibS y_Xiis) } in
                      case tagToEnum# @ Bool (<=## x_ahpC 0.0##)
                      of _ [Occ=Dead] {
                        False ->
                          (# ipv2_aif4,
                             System.Random.MWC.Distributions.T
                               y_Xiis (*## (*## x_ahpC x_ahpC) x_ahpC) #);
                        True -> innerloop_skxg ipv2_aif4
                      }
                      }
                      }
                      }; } in
                case innerloop_skxg s_XiKw
                of _ [Occ=Dead] { (# ipv2_aif4, ipv3_aif5 #) ->
                case ipv3_aif5
                of _ [Occ=Dead]
                { System.Random.MWC.Distributions.T dt_agoc dt1_agod ->
                case w_sypb `cast` ...
                of _ [Occ=Dead]
                { Data.Vector.Primitive.Mutable.MVector dt2_agol dt3_agom
                                                        dt4_agon ->
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol 256#)
                       (ipv2_aif4 `cast` ...)
                of _ [Occ=Dead] { (# ipv4_ahoH, ipv5_ahoI #) ->
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol 257#)
                       ipv4_ahoH
                of _ [Occ=Dead] { (# ipv6_XhUY, ipv7_XhV0 #) ->
                let {
                  ipv8_skAQ [Dmd=<S,U>] :: Int#

                  ipv8_skAQ =
                    word2Int#
                      (narrow8Word# (plusWord# ipv5_ahoI 1##)) } in
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol ipv8_skAQ)
                       ipv6_XhUY
                of _ [Occ=Dead] { (# ipv9_XhUZ, ipv10_XhV1 #) ->
                let {
                  j_skAR [Dmd=<S,U>] :: Int#

                  j_skAR =
                    word2Int#
                      (narrow8Word#
                         (int2Word# (+# ipv8_skAQ 1#))) } in
                case readWord32Array#
                       @ (Control.Monad.Primitive.PrimState IO)
                       dt4_agon
                       (+# dt2_agol j_skAR)
                       ipv9_XhUZ
                of _ [Occ=Dead] { (# ipv11_XhV4, ipv12_XhV6 #) ->
                let {
                  t1_skAv [Dmd=<S,U>] :: Word#

                  t1_skAv =
                    plusWord#
                      (timesWord# 1540315826## ipv10_XhV1) ipv7_XhV0 } in
                let {
                  c'_skAw [Dmd=<S,U>] :: Word#

                  c'_skAw =
                    narrow32Word#
                      (uncheckedShiftRL# t1_skAv 32#) } in
                let {
                  x2_skAx [Dmd=<S,U>] :: Word#

                  x2_skAx =
                    narrow32Word#
                      (plusWord# (narrow32Word# t1_skAv) c'_skAw) } in
                case tagToEnum# @ Bool (ltWord# x2_skAx c'_skAw)
                of _ [Occ=Dead] {
                  False ->
                    let {
                      u_snnY [Dmd=<S,U>] :: Word#

                      u_snnY =
                        plusWord#
                          (timesWord# 1540315826## ipv12_XhV6) c'_skAw } in
                    let {
                      d'_snnZ [Dmd=<S,U>] :: Word#

                      d'_snnZ =
                        narrow32Word# (uncheckedShiftRL# u_snnY 32#) } in
                    let {
                      y_sno0 [Dmd=<S,U>] :: Word#

                      y_sno0 =
                        narrow32Word#
                          (plusWord# (narrow32Word# u_snnY) d'_snnZ) } in
                    case tagToEnum# @ Bool (ltWord# y_sno0 d'_snnZ)
                    of _ [Occ=Dead] {
                      False ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_skAQ)
                               x2_skAx
                               ipv11_XhV4
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_skAR)
                               y_sno0
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_skAR))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               d'_snnZ
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_skBa [Dmd=<S,U>] :: Double#

                          x1_skBa = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# x2_skAx)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# y_sno0)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_skBa x1_skBa))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIe { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_skBa)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIe))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB65 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        };
                      True ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_skAQ)
                               x2_skAx
                               ipv11_XhV4
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        let {
                          x#_agr0 [Dmd=<S,U>] :: Word#

                          x#_agr0 =
                            narrow32Word# (plusWord# y_sno0 1##) } in
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_skAR)
                               x#_agr0
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_skAR))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               (narrow32Word# (plusWord# d'_snnZ 1##))
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_skBa [Dmd=<S,U>] :: Double#

                          x1_skBa = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# x2_skAx)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# x#_agr0)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_skBa x1_skBa))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIe { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_skBa)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIe))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB65 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        }
                    };
                  True ->
                    let {
                      u_snod [Dmd=<S,U>] :: Word#

                      u_snod =
                        plusWord#
                          (timesWord# 1540315826## ipv12_XhV6)
                          (narrow32Word# (plusWord# c'_skAw 1##)) } in
                    let {
                      d'_snoe [Dmd=<S,U>] :: Word#

                      d'_snoe =
                        narrow32Word# (uncheckedShiftRL# u_snod 32#) } in
                    let {
                      y_snof [Dmd=<S,U>] :: Word#

                      y_snof =
                        narrow32Word#
                          (plusWord# (narrow32Word# u_snod) d'_snoe) } in
                    let {
                      ipv13_snoc [Dmd=<S,U>] :: Word#

                      ipv13_snoc =
                        narrow32Word# (plusWord# x2_skAx 1##) } in
                    case tagToEnum# @ Bool (ltWord# y_snof d'_snoe)
                    of _ [Occ=Dead] {
                      False ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_skAQ)
                               ipv13_snoc
                               ipv11_XhV4
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_skAR)
                               y_snof
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_skAR))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               d'_snoe
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_skBa [Dmd=<S,U>] :: Double#

                          x1_skBa = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# ipv13_snoc)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# y_snof)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_skBa x1_skBa))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIe { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_skBa)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIe))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB65 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        };
                      True ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol ipv8_skAQ)
                               ipv13_snoc
                               ipv11_XhV4
                        of s'#_agqK [OS=OneShot] { __DEFAULT ->
                        let {
                          x#_agr0 [Dmd=<S,U>] :: Word#

                          x#_agr0 =
                            narrow32Word# (plusWord# y_snof 1##) } in
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol j_skAR)
                               x#_agr0
                               s'#_agqK
                        of s'#1_agr2 [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 256#)
                               (narrow32Word# (int2Word# j_skAR))
                               s'#1_agr2
                        of s'#2_agrk [OS=OneShot] { __DEFAULT ->
                        case writeWord32Array#
                               @ (Control.Monad.Primitive.PrimState IO)
                               dt4_agon
                               (+# dt2_agol 257#)
                               (narrow32Word# (plusWord# d'_snoe 1##))
                               s'#2_agrk
                        of s'#3_agrC [OS=OneShot] { __DEFAULT ->
                        let {
                          x1_skBa [Dmd=<S,U>] :: Double#

                          x1_skBa = *## dt_agoc dt_agoc } in
                        let {
                          x_ahoX [Dmd=<S,U>] :: Double#

                          x_ahoX =
                            +##
                              (+##
                                 (*##
                                    (int2Double#
                                       (narrow32Int# (word2Int# ipv13_snoc)))
                                    2.3283064365386963e-10##)
                                 0.5000000000000001##)
                              (*##
                                 (int2Double#
                                    (word2Int#
                                       (and#
                                          (int2Word#
                                             (narrow32Int# (word2Int# x#_agr0)))
                                          1048575##)))
                                 2.220446049250313e-16##) } in
                        case tagToEnum#
                               @ Bool
                               (>##
                                  x_ahoX
                                  (-##
                                     1.0## (*## 0.331## (*## x1_skBa x1_skBa))))
                        of _ [Occ=Dead] {
                          False ->
                            (# s'#3_agrC `cast` ...,
                               D# (*## 0.6666666666666667## dt1_agod) #);
                          True ->
                            case logDouble# x_ahoX of wild5_aibY { __DEFAULT ->
                            case logDouble# dt1_agod of wild6_XiIe { __DEFAULT ->
                            case tagToEnum#
                                   @ Bool
                                   (>##
                                      wild5_aibY
                                      (+##
                                         (*## 0.5## x1_skBa)
                                         (*##
                                            0.6666666666666667##
                                            (+##
                                               (-## 1.0## dt1_agod) wild6_XiIe))))
                            of _ [Occ=Dead] {
                              False ->
                                (# s'#3_agrC `cast` ...,
                                   D# (*## 0.6666666666666667## dt1_agod) #);
                              True -> mainloop_sB65 (s'#3_agrC `cast` ...)
                            }
                            }
                            }
                        }
                        }
                        }
                        }
                        }
                    }
                }
                }
                }
                }
                }
                }
                }
                }; } in
          letrec {
            $s$wfoldlM'_loop_sEq7 [Occ=LoopBreaker]
              :: State# RealWorld
                 -> Int#
                 -> Int#
                 -> (# State# RealWorld, Int #)

            $s$wfoldlM'_loop_sEq7 =
              \ (sc_sEq6 [OS=OneShot] :: State# RealWorld)
                (sc1_sEq4 :: Int#)
                (sc2_sEq3 :: Int#) ->
                case tagToEnum# @ Bool (<# sc1_sEq4 ww_sypf)
                of _ [Occ=Dead] {
                  False -> (# sc_sEq6, I# sc2_sEq3 #);
                  True ->
                    case mainloop_sB65 sc_sEq6
                    of _ [Occ=Dead] { (# ipv2_XilW, ipv3_XilY #) ->
                    case writeArray#
                           @ (Control.Monad.Primitive.PrimState IO)
                           @ Double
                           ipv1_aoGn
                           sc2_sEq3
                           ipv3_XilY
                           (ipv2_XilW `cast` ...)
                    of s'#_aoH3 [OS=OneShot] { __DEFAULT ->
                    $s$wfoldlM'_loop_sEq7
                      (s'#_aoH3 `cast` ...)
                      (+# sc1_sEq4 1#)
                      (+# sc2_sEq3 1#)
                    }
                    }
                }; } in
          case $s$wfoldlM'_loop_sEq7 (ipv_aoGm `cast` ...) 0# 0#
          of _ [Occ=Dead] { (# ipv2_Xim1, ipv3_Xim3 #) ->
          case ipv3_Xim3 of _ [Occ=Dead] { I# dt6_aoHy ->
          case unsafeFreezeArray#
                 @ (Control.Monad.Primitive.PrimState IO)
                 @ Double
                 ipv1_aoGn
                 (ipv2_Xim1 `cast` ...)
          of _ [Occ=Dead] { (# ipv4_aoIe, ipv5_aoIf #) ->
          (# ipv4_aoIe `cast` ...,
             case runRW#
                    @ 'PtrRepLifted
                    @ (V.Vector Double)
                    (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                       case newArray#
                              @ Double
                              @ (Control.Monad.Primitive.PrimState
                                   (ST RealWorld))
                              dt6_aoHy
                              (Data.Vector.Mutable.uninitialised @ Double)
                              (s1_ah7U `cast` ...)
                       of _ [Occ=Dead] { (# ipv6_XoPv, ipv7_XoPx #) ->
                       let {
                         ds_agwf [Dmd=<L,U(U)>] :: Double

                         ds_agwf =
                           letrec {
                             $w$s$wfoldrM_loop_syoV [InlPrag=[0], Occ=LoopBreaker]
                               :: Double# -> Int# -> Double#

                             $w$s$wfoldrM_loop_syoV =
                               \ (ww1_syoQ :: Double#) (w2_syoN :: Int#) ->
                                 case tagToEnum# @ Bool (>=# w2_syoN dt6_aoHy)
                                 of _ [Occ=Dead] {
                                   False ->
                                     case indexArray# @ Double ipv5_aoIf w2_syoN
                                     of _ [Occ=Dead] { (# ipv8_asRm #) ->
                                     case ipv8_asRm of _ [Occ=Dead] { D# y_aibp ->
                                     $w$s$wfoldrM_loop_syoV
                                       (+## ww1_syoQ y_aibp) (+# w2_syoN 1#)
                                     }
                                     };
                                   True -> ww1_syoQ
                                 }; } in
                           case $w$s$wfoldrM_loop_syoV 0.0## 0# of ww1_syoU { __DEFAULT ->
                           D# ww1_syoU
                           } } in
                       letrec {
                         $s$wfoldlM'_loop1_sEpY [Occ=LoopBreaker]
                           :: State# RealWorld
                              -> Int#
                              -> Int#
                              -> (# State# RealWorld, Int #)

                         $s$wfoldlM'_loop1_sEpY =
                           \ (sc_sEpX [OS=OneShot] :: State# RealWorld)
                             (sc1_sEpV :: Int#)
                             (sc2_sEpU :: Int#) ->
                             case tagToEnum# @ Bool (>=# sc1_sEpV dt6_aoHy)
                             of _ [Occ=Dead] {
                               False ->
                                 case writeArray#
                                        @ (Control.Monad.Primitive.PrimState
                                             (ST RealWorld))
                                        @ Double
                                        ipv7_XoPx
                                        sc2_sEpU
                                        (case indexArray# @ Double ipv5_aoIf sc1_sEpV
                                         of _ [Occ=Dead] { (# ipv8_ai43 #) ->
                                         case ipv8_ai43 of _ [Occ=Dead] { D# x_aibM ->
                                         case ds_agwf of _ [Occ=Dead] { D# y_aibQ ->
                                         case /## x_aibM y_aibQ
                                         of wild3_aibS { __DEFAULT ->
                                         D# wild3_aibS
                                         }
                                         }
                                         }
                                         })
                                        (sc_sEpX `cast` ...)
                                 of s'#_aoH3 [OS=OneShot] { __DEFAULT ->
                                 $s$wfoldlM'_loop1_sEpY
                                   (s'#_aoH3 `cast` ...)
                                   (+# sc1_sEpV 1#)
                                   (+# sc2_sEpU 1#)
                                 };
                               True -> (# sc_sEpX, I# sc2_sEpU #)
                             }; } in
                       case $s$wfoldlM'_loop1_sEpY (ipv6_XoPv `cast` ...) 0# 0#
                       of _ [Occ=Dead] { (# ipv8_ah4v, ipv9_ah4w #) ->
                       case ipv9_ah4w of _ [Occ=Dead] { I# dt2_XoR7 ->
                       case unsafeFreezeArray#
                              @ (Control.Monad.Primitive.PrimState
                                   (ST RealWorld))
                              @ Double
                              ipv7_XoPx
                              (ipv8_ah4v `cast` ...)
                       of _ [Occ=Dead] { (# ipv10_XoPY, ipv11_XoQ0 #) ->
                       (# ipv10_XoPY `cast` ...,
                          Data.Vector.Vector @ Double 0# dt2_XoR7 ipv11_XoQ0 #)
                       }
                       }
                       }
                       })
             of _ [Occ=Dead] { (# ipv6_ah84, ipv7_ah85 #) ->
             ipv7_ah85
             } #)
          }
          }
          }
          } } in
    case tagToEnum# @ Bool (<=# ww_sypf 0#)
    of _ [Occ=Dead] {
      False -> $j_stuk ww_sypf;
      True -> $j_stuk 0#
    }

-- RHS size: {terms: 10, types: 6, coercions: 0}
vocabPrior1 [InlPrag=INLINE[0]]
  :: Int
     -> MWC.GenIO
     -> State# RealWorld
     -> (# State# RealWorld, V.Vector Double #)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_sypa [Occ=Once!] :: Int)
                 (w1_sypb [Occ=Once] :: MWC.GenIO)
                 (w2_sypc [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 case w_sypa of _ [Occ=Dead] { I# ww1_sypf [Occ=Once] ->
                 $wvocabPrior ww1_sypf w1_sypb w2_sypc
                 }}]
vocabPrior1 =
  \ (w_sypa :: Int)
    (w1_sypb :: MWC.GenIO)
    (w2_sypc [OS=OneShot] :: State# RealWorld) ->
    case w_sypa of _ [Occ=Dead] { I# ww1_sypf ->
    $wvocabPrior ww1_sypf w1_sypb w2_sypc
    }

-- RHS size: {terms: 1, types: 0, coercions: 8}
vocabPrior :: Int -> MWC.GenIO -> IO (V.Vector Double)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= vocabPrior1 `cast` ...}]
vocabPrior = vocabPrior1 `cast` ...

-- RHS size: {terms: 1, types: 0, coercions: 3}
now :: IO Time
[GblId,
 Arity=1,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= Data.Time.Clock.POSIX.getCurrentTime1 `cast` ...}]
now = Data.Time.Clock.POSIX.getCurrentTime1 `cast` ...

-- RHS size: {terms: 2, types: 1, coercions: 0}
lvl71_rIrJ :: Maybe Double

lvl71_rIrJ = Just @ Double labelHP

-- RHS size: {terms: 7, types: 3, coercions: 0}
lvl72_rIrK :: Bool -> Maybe Double

lvl72_rIrK =
  \ (p_ajZS :: Bool) ->
    case p_ajZS of _ [Occ=Dead] {
      False -> Nothing @ Double;
      True -> lvl71_rIrJ
    }

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl73_rIrL :: Double

lvl73_rIrL = D# 0.0##

-- RHS size: {terms: 360, types: 187, coercions: 80}
dirichlet0_rIrM
  :: Data.Vector.Unboxed.Base.Vector Double
     -> Measure (Data.Vector.Unboxed.Base.Vector Double)

dirichlet0_rIrM =
  \ (as1_abVT :: Data.Vector.Unboxed.Base.Vector Double) ->
    let {
      lvl102_sB69 :: Int

      lvl102_sB69 = $s!_$slength2 (as1_abVT `cast` ...) } in
    let {
      m_sk1b [Dmd=<L,C(C(U(U,U)))>] :: Measure (MayBoxVec Double Double)

      m_sk1b =
        let {
          $wlvl_sypq [InlPrag=[0]] :: Int# -> Double#

          $wlvl_sypq =
            \ (ww_sypl :: Int#) ->
              case tagToEnum# @ Bool (>=# ww_sypl 0#)
              of _ [Occ=Dead] {
                False ->
                  case lvl102_sB69 of _ [Occ=Dead] { I# n#_ahl6 ->
                  case lvl50_rIrh n#_ahl6 ww_sypl of wild_00 { }
                  };
                True ->
                  case lvl102_sB69 of _ [Occ=Dead] { I# y_ahlc ->
                  case tagToEnum# @ Bool (<# ww_sypl y_ahlc)
                  of _ [Occ=Dead] {
                    False -> case lvl50_rIrh y_ahlc ww_sypl of wild_00 { };
                    True ->
                      case as1_abVT `cast` ...
                      of _ [Occ=Dead]
                      { Data.Vector.Primitive.Vector dt_ajiC dt1_ajiD dt2_ajiE ->
                      indexDoubleArray# dt2_ajiE (+# dt_ajiC ww_sypl)
                      }
                  }
                  }
              } } in
        plate
          @ Double
          (Data.Vector.Unboxed.Base.$fVectorVectorDouble `cast` ...)
          (case lvl102_sB69 of _ [Occ=Dead] { I# x_agKv ->
           I# (+# x_agKv -1#)
           })
          ((\ (i3_abWN :: Int) ->
              let {
                w_ak0e [Dmd=<L,U(U)>] :: Double

                w_ak0e =
                  case i3_abWN of _ [Occ=Dead] { I# x_agKv ->
                  case lvl102_sB69 of _ [Occ=Dead] { I# ww3_agDQ ->
                  let {
                    ww_ahgv [Dmd=<S,U>] :: Int#

                    ww_ahgv = +# x_agKv 1# } in
                  let {
                    y_ahgy [Dmd=<S,U>] :: Int#

                    y_ahgy = -# ww3_agDQ 1# } in
                  case tagToEnum# @ Bool (># ww_ahgv y_ahgy)
                  of _ [Occ=Dead] {
                    False ->
                      letrec {
                        $wgo3_sypA [InlPrag=[0], Occ=LoopBreaker]
                          :: Int# -> Double# -> Double#

                        $wgo3_sypA =
                          \ (w1_sypr :: Int#)
                            (ww1_sypv [OS=OneShot] :: Double#) ->
                            case tagToEnum# @ Bool (==# w1_sypr y_ahgy)
                            of _ [Occ=Dead] {
                              False ->
                                case $wlvl_sypq w1_sypr of ww5_sypp { __DEFAULT ->
                                $wgo3_sypA
                                  (+# w1_sypr 1#) (+## ww1_sypv ww5_sypp)
                                };
                              True ->
                                case $wlvl_sypq w1_sypr of ww5_sypp { __DEFAULT ->
                                +## ww1_sypv ww5_sypp
                                }
                            }; } in
                      case $wgo3_sypA ww_ahgv 0.0## of ww1_sypz { __DEFAULT ->
                      D# ww1_sypz
                      };
                    True -> lvl73_rIrL
                  }
                  }
                  } } in
              let {
                w1_ak0f [Dmd=<L,U(U)>] :: Double

                w1_ak0f =
                  case i3_abWN of _ [Occ=Dead] { I# x_ahkY ->
                  case tagToEnum# @ Bool (>=# x_ahkY 0#)
                  of _ [Occ=Dead] {
                    False ->
                      case lvl102_sB69 of _ [Occ=Dead] { I# n#_ahl6 ->
                      lvl50_rIrh n#_ahl6 x_ahkY
                      };
                    True ->
                      case lvl102_sB69 of _ [Occ=Dead] { I# y_ahlc ->
                      case tagToEnum# @ Bool (<# x_ahkY y_ahlc)
                      of _ [Occ=Dead] {
                        False -> lvl50_rIrh y_ahlc x_ahkY;
                        True ->
                          case as1_abVT `cast` ...
                          of _ [Occ=Dead]
                          { Data.Vector.Primitive.Vector dt_ajiC dt1_ajiD dt2_ajiE ->
                          case indexDoubleArray#
                                 dt2_ajiE (+# dt_ajiC x_ahkY)
                          of wild5_ajiO { __DEFAULT ->
                          D# wild5_ajiO
                          }
                          }
                      }
                      }
                  }
                  } } in
              \ (w2_ak0g :: MWC.GenIO)
                (w3_ak0h [OS=OneShot] :: State# RealWorld) ->
                case w_ak0e of _ [Occ=Dead] { D# ww1_ak0k ->
                case w2_ak0g `cast` ...
                of _ [Occ=Dead]
                { Data.Vector.Primitive.Mutable.MVector ww3_ak0o ww4_ak0p
                                                        ww5_ak0q ->
                Language.Hakaru.Runtime.Prelude.$wbeta
                  ww1_ak0k w1_ak0f ww3_ak0o ww5_ak0q w3_ak0h
                }
                })
           `cast` ...) } in
    (\ (g_ak0V :: MWC.GenIO)
       (s_ak0W [OS=OneShot] :: State# RealWorld) ->
       case (((m_sk1b `cast` ...) g_ak0V) `cast` ...) s_ak0W
       of _ [Occ=Dead] { (# ipv_ak0Z, ipv1_ak10 #) ->
       case ipv1_ak10 of _ [Occ=Dead] {
         Nothing ->
           Language.Hakaru.Runtime.Prelude.$fApplicativeMeasure2
             @ (Data.Vector.Unboxed.Base.Vector Double) ipv_ak0Z;
         Just x_ak17 ->
           let {
             lvl103_sB6v :: Int

             lvl103_sB6v = $s!_$slength2 (x_ak17 `cast` ...) } in
           (# ipv_ak0Z,
              Just
                @ (Data.Vector.Unboxed.Base.Vector Double)
                (case lvl102_sB69 of _ [Occ=Dead] { I# ww1_agE0 ->
                 let {
                   $wlvl_sypJ [InlPrag=[0]] :: Int# -> Double#

                   $wlvl_sypJ =
                     \ (ww2_sypE :: Int#) ->
                       case tagToEnum# @ Bool (>=# ww2_sypE 0#)
                       of _ [Occ=Dead] {
                         False ->
                           case lvl103_sB6v of _ [Occ=Dead] { I# n#_ahl6 ->
                           case lvl50_rIrh n#_ahl6 ww2_sypE of wild4_00 { }
                           };
                         True ->
                           case lvl103_sB6v of _ [Occ=Dead] { I# y_ahlc ->
                           case tagToEnum# @ Bool (<# ww2_sypE y_ahlc)
                           of _ [Occ=Dead] {
                             False -> case lvl50_rIrh y_ahlc ww2_sypE of wild4_00 { };
                             True ->
                               case x_ak17 `cast` ...
                               of _ [Occ=Dead]
                               { Data.Vector.Primitive.Vector dt_ajiC dt1_ajiD dt2_ajiE ->
                               indexDoubleArray# dt2_ajiE (+# dt_ajiC ww2_sypE)
                               }
                           }
                           }
                       } } in
                 (Language.Hakaru.Runtime.Prelude.$warray
                    @ Double
                    (Data.Vector.Unboxed.Base.$fVectorVectorDouble `cast` ...)
                    ww1_agE0
                    (\ (i5_abWQ :: Int) ->
                       case i5_abWQ of _ [Occ=Dead] { I# ww3_agEl ->
                       let {
                         y_ahjK [Dmd=<S,U>] :: Int#

                         y_ahjK = -# ww3_agEl 1# } in
                       let {
                         $w$j_sypO [InlPrag=[0]] :: Double# -> Double#

                         $w$j_sypO =
                           \ (w_sypK [OS=OneShot] :: Double#) ->
                             case Language.Hakaru.Runtime.Prelude.case_1
                                    @ Double
                                    @ Bool
                                    (tagToEnum#
                                       @ Bool (==# (+# ww3_agEl 1#) ww1_agE0))
                                    (:
                                       @ (Branch Bool Double)
                                       (lvl72_rIrK `cast` ...)
                                       (:
                                          @ (Branch Bool Double)
                                          (let {
                                             b1_Xk9Q :: Double

                                             b1_Xk9Q =
                                               case tagToEnum#
                                                      @ Bool (>=# ww3_agEl 0#)
                                               of _ [Occ=Dead] {
                                                 False ->
                                                   case lvl103_sB6v
                                                   of _ [Occ=Dead] { I# n#_ahl6 ->
                                                   lvl50_rIrh n#_ahl6 ww3_agEl
                                                   };
                                                 True ->
                                                   case lvl103_sB6v
                                                   of _ [Occ=Dead] { I# y1_ahlc ->
                                                   case tagToEnum#
                                                          @ Bool (<# ww3_agEl y1_ahlc)
                                                   of _ [Occ=Dead] {
                                                     False -> lvl50_rIrh y1_ahlc ww3_agEl;
                                                     True ->
                                                       case x_ak17 `cast` ...
                                                       of _ [Occ=Dead]
                                                       { Data.Vector.Primitive.Vector dt_ajiC
                                                                                      dt1_ajiD
                                                                                      dt2_ajiE ->
                                                       case indexDoubleArray#
                                                              dt2_ajiE
                                                              (+# dt_ajiC ww3_agEl)
                                                       of wild5_ajiO { __DEFAULT ->
                                                       D#
                                                         (+##
                                                            1.0##
                                                            (negateDouble# wild5_ajiO))
                                                       }
                                                       }
                                                   }
                                                   }
                                               } } in
                                           let {
                                             lvl104_sB6N :: Maybe Double

                                             lvl104_sB6N = Just @ Double b1_Xk9Q } in
                                           (\ (p_ak0x :: Bool) ->
                                              case p_ak0x of _ [Occ=Dead] {
                                                False -> lvl104_sB6N;
                                                True -> Nothing @ Double
                                              })
                                           `cast` ...)
                                          ([] @ (Branch Bool Double))))
                             of _ [Occ=Dead] { D# y1_aicx ->
                             *## w_sypK y1_aicx
                             } } in
                       case tagToEnum# @ Bool (># 0# y_ahjK)
                       of _ [Occ=Dead] {
                         False ->
                           letrec {
                             $wgo3_sypY [InlPrag=[0], Occ=LoopBreaker]
                               :: Int# -> Double# -> Double#

                             $wgo3_sypY =
                               \ (w_sypP :: Int#)
                                 (ww4_sypT [OS=OneShot] :: Double#) ->
                                 case tagToEnum# @ Bool (==# w_sypP y_ahjK)
                                 of _ [Occ=Dead] {
                                   False ->
                                     case $wlvl_sypJ w_sypP of ww5_sypI { __DEFAULT ->
                                     $wgo3_sypY
                                       (+# w_sypP 1#) (*## ww4_sypT ww5_sypI)
                                     };
                                   True ->
                                     case $wlvl_sypJ w_sypP of ww5_sypI { __DEFAULT ->
                                     *## ww4_sypT ww5_sypI
                                     }
                                 }; } in
                           case $wgo3_sypY 0# 1.0## of ww4_sypX { __DEFAULT ->
                           case $w$j_sypO ww4_sypX of ww5_sypN { __DEFAULT ->
                           D# ww5_sypN
                           }
                           };
                         True ->
                           case $w$j_sypO 1.0## of ww4_sypN { __DEFAULT ->
                           D# ww4_sypN
                           }
                       }
                       }))
                 `cast` ...
                 }) #)
       }
       })
    `cast` ...

-- RHS size: {terms: 1, types: 0, coercions: 0}
i_rIrN :: Integer

i_rIrN = 1

-- RHS size: {terms: 3, types: 0, coercions: 4}
lvl74_rIrO :: Double

lvl74_rIrO =
  Language.Hakaru.Runtime.Prelude.$wprob_
    (i_rIrN `cast` ...) ($fEnumRatio1 `cast` ...)

-- RHS size: {terms: 2, types: 1, coercions: 0}
lvl75_rIrP :: Int -> Double

lvl75_rIrP = \ _ [Occ=Dead] -> lvl74_rIrO

-- RHS size: {terms: 233, types: 230, coercions: 156}
generateDataset
  :: Int
     -> Int
     -> Int
     -> Data.Vector.Unboxed.Base.Vector Int
     -> Measure
          (Data.Vector.Unboxed.Base.Vector Int,
           Data.Vector.Unboxed.Base.Vector Int)

generateDataset =
  \ (k9_abWU :: Int) ->
    let {
      lvl102_sB6R :: Measure (Data.Vector.Unboxed.Base.Vector Double)

      lvl102_sB6R =
        dirichlet0_rIrM
          (case k9_abWU of _ [Occ=Dead] { I# ww1_agE0 ->
           (Language.Hakaru.Runtime.Prelude.$warray
              @ Double
              (Data.Vector.Unboxed.Base.$fVectorVectorDouble `cast` ...)
              ww1_agE0
              lvl75_rIrP)
           `cast` ...
           }) } in
    \ (v10_abWV :: Int) ->
      let {
        lvl103_sB6P :: Measure (Data.Vector.Unboxed.Base.Vector Double)

        lvl103_sB6P =
          dirichlet0_rIrM
            (case v10_abWV of _ [Occ=Dead] { I# ww1_agE0 ->
             (Language.Hakaru.Runtime.Prelude.$warray
                @ Double
                (Data.Vector.Unboxed.Base.$fVectorVectorDouble `cast` ...)
                ww1_agE0
                lvl75_rIrP)
             `cast` ...
             }) } in
      let {
        lvl104_sB6S
          :: Int -> Measure (Data.Vector.Unboxed.Base.Vector Double)

        lvl104_sB6S = \ _ [Occ=Dead] -> lvl103_sB6P } in
      \ (z11_abWW :: Int)
        (eta_B1 :: Data.Vector.Unboxed.Base.Vector Int) ->
        let {
          lvl105_sB6V :: Int

          lvl105_sB6V = $s!_$slength1 (eta_B1 `cast` ...) } in
        (\ (g_ak0V :: MWC.GenIO)
           (s_ak0W [OS=OneShot] :: State# RealWorld) ->
           case (((lvl102_sB6R `cast` ...) g_ak0V) `cast` ...) s_ak0W
           of _ [Occ=Dead] { (# ipv_ak0Z, ipv1_ak10 #) ->
           case ipv1_ak10 of _ [Occ=Dead] {
             Nothing ->
               Language.Hakaru.Runtime.Prelude.$fApplicativeMeasure2
                 @ (Data.Vector.Unboxed.Base.Vector Int,
                    Data.Vector.Unboxed.Base.Vector Int)
                 ipv_ak0Z;
             Just x_ak17 ->
               case ((((plate
                          @ (Data.Vector.Unboxed.Base.Vector Double)
                          ((Data.Vector.$fVectorVectora
                              @ (Data.Vector.Unboxed.Base.Vector Double))
                           `cast` ...)
                          k9_abWU
                          lvl104_sB6S)
                       `cast` ...)
                        g_ak0V)
                     `cast` ...)
                      ipv_ak0Z
               of _ [Occ=Dead] { (# ipv2_Xkvv, ipv3_Xkvx #) ->
               case ipv3_Xkvx of _ [Occ=Dead] {
                 Nothing ->
                   Language.Hakaru.Runtime.Prelude.$fApplicativeMeasure2
                     @ (Data.Vector.Unboxed.Base.Vector Int,
                        Data.Vector.Unboxed.Base.Vector Int)
                     ipv2_Xkvv;
                 Just x1_XkvH ->
                   let {
                     lvl106_sB7f :: Int

                     lvl106_sB7f = $s!_$slength (x1_XkvH `cast` ...) } in
                   case ((((plate
                              @ Int
                              (Data.Vector.Unboxed.Base.$fVectorVectorInt `cast` ...)
                              z11_abWW
                              ((\ _ [Occ=Dead]
                                  (eta1_X9v :: MWC.GenIO)
                                  (eta2_Xuk [OS=OneShot] :: State# RealWorld) ->
                                  Language.Hakaru.Runtime.Prelude.categorical1
                                    (x_ak17 `cast` ...) eta1_X9v eta2_Xuk)
                               `cast` ...))
                           `cast` ...)
                            g_ak0V)
                         `cast` ...)
                          ipv2_Xkvv
                   of _ [Occ=Dead] { (# ipv4_Xl0f, ipv5_Xkvz #) ->
                   case ipv5_Xkvz of _ [Occ=Dead] {
                     Nothing ->
                       Language.Hakaru.Runtime.Prelude.$fApplicativeMeasure2
                         @ (Data.Vector.Unboxed.Base.Vector Int,
                            Data.Vector.Unboxed.Base.Vector Int)
                         ipv4_Xl0f;
                     Just x2_XkvJ ->
                       let {
                         lvl107_sB75 :: Int

                         lvl107_sB75 = $s!_$slength1 (x2_XkvJ `cast` ...) } in
                       case ((((plate
                                  @ Int
                                  (Data.Vector.Unboxed.Base.$fVectorVectorInt `cast` ...)
                                  lvl105_sB6V
                                  ((\ (n23_abX7 :: Int) ->
                                      let {
                                        w_ak1m [Dmd=<L,U(U,U,U)>] :: MayBoxVec Double Double

                                        w_ak1m =
                                          case n23_abX7 of _ [Occ=Dead] { I# x3_ahkY ->
                                          case tagToEnum# @ Bool (>=# x3_ahkY 0#)
                                          of _ [Occ=Dead] {
                                            False ->
                                              case lvl105_sB6V
                                              of _ [Occ=Dead] { I# n#_ahl6 ->
                                              case lvl49_rIrg n#_ahl6 x3_ahkY of wild6_00 { }
                                              };
                                            True ->
                                              case lvl105_sB6V
                                              of _ [Occ=Dead] { I# y_ahlc ->
                                              case tagToEnum#
                                                     @ Bool (<# x3_ahkY y_ahlc)
                                              of _ [Occ=Dead] {
                                                False ->
                                                  case lvl49_rIrg y_ahlc x3_ahkY of wild7_00 { };
                                                True ->
                                                  case eta_B1 `cast` ...
                                                  of _ [Occ=Dead]
                                                  { Data.Vector.Primitive.Vector dt_ai1q dt1_ai1r
                                                                                 dt2_ai1s ->
                                                  case indexIntArray#
                                                         dt2_ai1s (+# dt_ai1q x3_ahkY)
                                                  of wild8_ai1B { __DEFAULT ->
                                                  case tagToEnum#
                                                         @ Bool (>=# wild8_ai1B 0#)
                                                  of _ [Occ=Dead] {
                                                    False ->
                                                      case lvl107_sB75
                                                      of _ [Occ=Dead] { I# n#_ahl6 ->
                                                      case lvl49_rIrg n#_ahl6 wild8_ai1B
                                                      of wild11_00 {
                                                      }
                                                      };
                                                    True ->
                                                      case lvl107_sB75
                                                      of _ [Occ=Dead] { I# y1_XhuQ ->
                                                      case tagToEnum#
                                                             @ Bool (<# wild8_ai1B y1_XhuQ)
                                                      of _ [Occ=Dead] {
                                                        False ->
                                                          case lvl49_rIrg y1_XhuQ wild8_ai1B
                                                          of wild12_00 {
                                                          };
                                                        True ->
                                                          case x2_XkvJ `cast` ...
                                                          of _ [Occ=Dead]
                                                          { Data.Vector.Primitive.Vector dt4_Xib9
                                                                                         dt5_Xibb
                                                                                         dt6_Xibd ->
                                                          case indexIntArray#
                                                                 dt6_Xibd
                                                                 (+# dt4_Xib9 wild8_ai1B)
                                                          of wild13_Xibp { __DEFAULT ->
                                                          case tagToEnum#
                                                                 @ Bool
                                                                 (>=# wild13_Xibp 0#)
                                                          of _ [Occ=Dead] {
                                                            False ->
                                                              case lvl106_sB7f
                                                              of _ [Occ=Dead]
                                                              { I# n#_ahl6 ->
                                                              (lvl48_rIrf n#_ahl6 wild13_Xibp)
                                                              `cast` ...
                                                              };
                                                            True ->
                                                              case lvl106_sB7f
                                                              of _ [Occ=Dead]
                                                              { I# y2_Xhv6 ->
                                                              case tagToEnum#
                                                                     @ Bool
                                                                     (<#
                                                                        wild13_Xibp y2_Xhv6)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  (lvl48_rIrf y2_Xhv6 wild13_Xibp)
                                                                  `cast` ...;
                                                                True ->
                                                                  case x1_XkvH
                                                                  of _ [Occ=Dead]
                                                                  { Data.Vector.Vector dt7_ai3T
                                                                                       dt8_ai3U
                                                                                       dt9_ai3V ->
                                                                  case indexArray#
                                                                         @ (Data.Vector.Unboxed.Base.Vector
                                                                              Double)
                                                                         dt9_ai3V
                                                                         (+#
                                                                            dt7_ai3T wild13_Xibp)
                                                                  of _ [Occ=Dead]
                                                                  { (# ipv6_ai43 #) ->
                                                                  ipv6_ai43 `cast` ...
                                                                  }
                                                                  }
                                                              }
                                                              }
                                                          }
                                                          }
                                                          }
                                                      }
                                                      }
                                                  }
                                                  }
                                                  }
                                              }
                                              }
                                          }
                                          } } in
                                      \ (w1_ak1n :: MWC.GenIO)
                                        (w2_ak1o [OS=OneShot]
                                           :: State# RealWorld) ->
                                        case w_ak1m `cast` ...
                                        of _ [Occ=Dead]
                                        { Data.Vector.Primitive.Vector ww1_ak1r ww2_ak1s ww3_ak1t ->
                                        case w1_ak1n `cast` ...
                                        of _ [Occ=Dead]
                                        { Data.Vector.Primitive.Mutable.MVector ww5_ak1x ww6_ak1y
                                                                                ww7_ak1z ->
                                        Language.Hakaru.Runtime.Prelude.$wcategorical
                                          ww1_ak1r ww2_ak1s ww3_ak1t ww5_ak1x ww7_ak1z w2_ak1o
                                        }
                                        })
                                   `cast` ...))
                               `cast` ...)
                                g_ak0V)
                             `cast` ...)
                              ipv4_Xl0f
                       of _ [Occ=Dead] { (# ipv6_Xkvy, ipv7_XkvA #) ->
                       case ipv7_XkvA of _ [Occ=Dead] {
                         Nothing ->
                           Language.Hakaru.Runtime.Prelude.$fApplicativeMeasure2
                             @ (Data.Vector.Unboxed.Base.Vector Int,
                                Data.Vector.Unboxed.Base.Vector Int)
                             ipv6_Xkvy;
                         Just x3_XkvK ->
                           (# ipv6_Xkvy,
                              Just
                                @ (Data.Vector.Unboxed.Base.Vector Int,
                                   Data.Vector.Unboxed.Base.Vector Int)
                                (x2_XkvJ, x3_XkvK) #)
                       }
                       }
                   }
                   }
               }
               }
           }
           })
        `cast` ...

-- RHS size: {terms: 2, types: 1, coercions: 0}
lvl76_rIrQ :: Maybe Int

lvl76_rIrQ = Just @ Int $fShowArrayType1

-- RHS size: {terms: 7, types: 3, coercions: 0}
lvl77_rIrR :: Bool -> Maybe Int

lvl77_rIrR =
  \ (p_ak0x :: Bool) ->
    case p_ak0x of _ [Occ=Dead] {
      False -> lvl76_rIrQ;
      True -> Nothing @ Int
    }

-- RHS size: {terms: 3, types: 6, coercions: 4}
lvl78_rIrS :: [Branch Bool Int]

lvl78_rIrS =
  :
    @ (Branch Bool Int)
    (lvl77_rIrR `cast` ...)
    ([] @ (Branch Bool Int))

-- RHS size: {terms: 2, types: 0, coercions: 0}
b_rIrT :: Int

b_rIrT = I# 1#

-- RHS size: {terms: 2, types: 1, coercions: 0}
lvl79_rIrU :: Maybe Int

lvl79_rIrU = Just @ Int b_rIrT

-- RHS size: {terms: 7, types: 3, coercions: 0}
lvl80_rIrV :: Bool -> Maybe Int

lvl80_rIrV =
  \ (p_XktT :: Bool) ->
    case p_XktT of _ [Occ=Dead] {
      False -> Nothing @ Int;
      True -> lvl79_rIrU
    }

-- RHS size: {terms: 3, types: 3, coercions: 4}
lvl81_rIrW :: [Branch Bool Int]

lvl81_rIrW =
  : @ (Branch Bool Int) (lvl80_rIrV `cast` ...) lvl78_rIrS

-- RHS size: {terms: 7, types: 3, coercions: 0}
lvl82_rIrX :: Bool -> Maybe Int

lvl82_rIrX =
  \ (p_XktX :: Bool) ->
    case p_XktX of _ [Occ=Dead] {
      False -> Nothing @ Int;
      True -> lvl76_rIrQ
    }

-- RHS size: {terms: 7, types: 3, coercions: 0}
lvl83_rIrY :: Bool -> Maybe Int

lvl83_rIrY =
  \ (p_XkA7 :: Bool) ->
    case p_XkA7 of _ [Occ=Dead] {
      False -> lvl79_rIrU;
      True -> Nothing @ Int
    }

-- RHS size: {terms: 3, types: 6, coercions: 4}
lvl84_rIrZ :: [Branch Bool Int]

lvl84_rIrZ =
  :
    @ (Branch Bool Int)
    (lvl83_rIrY `cast` ...)
    ([] @ (Branch Bool Int))

-- RHS size: {terms: 3, types: 3, coercions: 4}
lvl85_rIs0 :: [Branch Bool Int]

lvl85_rIs0 =
  : @ (Branch Bool Int) (lvl82_rIrX `cast` ...) lvl84_rIrZ

-- RHS size: {terms: 1,571, types: 679, coercions: 138}
gibbs
  :: Data.Vector.Unboxed.Base.Vector Double
     -> Data.Vector.Unboxed.Base.Vector Double
     -> Data.Vector.Unboxed.Base.Vector Int
     -> Data.Vector.Unboxed.Base.Vector Int
     -> Data.Vector.Unboxed.Base.Vector Int
     -> Int
     -> Measure Int

gibbs =
  \ (topic_prior0_abXa :: Data.Vector.Unboxed.Base.Vector Double) ->
    let {
      topic_prior_length :: Int

      topic_prior_length =
        $s!_$slength2 (topic_prior0_abXa `cast` ...) } in
    let {
      lvl103_shTb [Dmd=<L,U(U)>] :: Double

      lvl103_shTb =
        case topic_prior_length of _ [Occ=Dead] { I# ww3_agDQ ->
        let {
          topic_prior_len_sub1 [Dmd=<S,U>] :: Int#

          topic_prior_len_sub1 = -# ww3_agDQ 1# } in
        case tagToEnum# @ Bool (># 0# topic_prior_length_)
        of _ [Occ=Dead] {
          False ->
            let {
              $ww1_syq9 [InlPrag=[0]] :: Int# -> Double#

              $ww1_syq9 =
                \ (ww_syq4 :: Int#) ->
                  case tagToEnum# @ Bool (>=# ww_syq4 0#)
                  of _ [Occ=Dead] {
                    False -> case lvl50_rIrh ww3_agDQ ww_syq4 of wild2_00 { };
                    True ->
                      case tagToEnum# @ Bool (<# ww_syq4 ww3_agDQ)
                      of _ [Occ=Dead] {
                        False -> case lvl50_rIrh ww3_agDQ ww_syq4 of wild2_00 { };
                        True ->
                          case topic_prior0_abXa `cast` ...
                          of _ [Occ=Dead]
                          { Data.Vector.Primitive.Vector dt_offset dt_len dt_bytearray ->
                          indexDoubleArray# dt_bytearray (+# dt_offset ww_syq4)
                          }
                      }
                  } } in
            letrec {
              $wgo3_syqj [InlPrag=[0], Occ=LoopBreaker]
                :: Int# -> Double# -> Double#

              $wgo3_syqj =
                \ (w_syqa :: Int#)
                  (ww_syqe [OS=OneShot] :: Double#) ->
                  case tagToEnum# @ Bool (==# w_syqa topic_prior_length_)
                  of _ [Occ=Dead] {
                    False ->
                      case $ww1_syq9 w_syqa of ww1_syq8 { __DEFAULT ->
                      $wgo3_syqj (+# w_syqa 1#) (+## ww_syqe ww1_syq8)
                      };
                    True ->
                      case $ww1_syq9 w_syqa of ww1_syq8 { __DEFAULT ->
                      +## ww_syqe ww1_syq8
                      }
                  }; } in
            case $wgo3_syqj 0# 0.0## of ww_syqi { __DEFAULT ->
            D# ww_syqi
            };
          True -> lvl73_rIrL
        }
        } } in
    \ (word_prior1_abXb :: Data.Vector.Unboxed.Base.Vector Double) ->
      let {
        lvl104_sB7S :: Int

        lvl104_sB7S = $s!_$slength2 (word_prior1_abXb `cast` ...) } in
      let {
        lvl105_shTO [Dmd=<L,U(U)>] :: Double

        lvl105_shTO =
          case lvl104_sB7S of _ [Occ=Dead] { I# ww3_agDQ ->
          let {
            topic_prior_length_ [Dmd=<S,U>] :: Int#

            topic_prior_length_ = -# ww3_agDQ 1# } in
          case tagToEnum# @ Bool (># 0# topic_prior_length_)
          of _ [Occ=Dead] {
            False ->
              let {
                $ww1_syqs [InlPrag=[0]] :: Int# -> Double#

                $ww1_syqs =
                  \ (ww_syqn :: Int#) ->
                    case tagToEnum# @ Bool (>=# ww_syqn 0#)
                    of _ [Occ=Dead] {
                      False -> case lvl50_rIrh ww3_agDQ ww_syqn of wild2_00 { };
                      True ->
                        case tagToEnum# @ Bool (<# ww_syqn ww3_agDQ)
                        of _ [Occ=Dead] {
                          False -> case lvl50_rIrh ww3_agDQ ww_syqn of wild2_00 { };
                          True ->
                            case word_prior1_abXb `cast` ...
                            of _ [Occ=Dead]
                            { Data.Vector.Primitive.Vector dt_offset dt_len dt_bytearray ->
                            indexDoubleArray# dt_bytearray (+# dt_offset ww_syqn)
                            }
                        }
                    } } in
              letrec {
                $wgo3_syqC [InlPrag=[0], Occ=LoopBreaker]
                  :: Int# -> Double# -> Double#

                $wgo3_syqC =
                  \ (w_syqt :: Int#)
                    (ww_syqx [OS=OneShot] :: Double#) ->
                    case tagToEnum# @ Bool (==# w_syqt topic_prior_length_)
                    of _ [Occ=Dead] {
                      False ->
                        case $ww1_syqs w_syqt of ww1_syqr { __DEFAULT ->
                        $wgo3_syqC (+# w_syqt 1#) (+## ww_syqx ww1_syqr)
                        };
                      True ->
                        case $ww1_syqs w_syqt of ww1_syqr { __DEFAULT ->
                        +## ww_syqx ww1_syqr
                        }
                    }; } in
              case $wgo3_syqC 0# 0.0## of ww_syqB { __DEFAULT ->
              D# ww_syqB
              };
            True -> lvl73_rIrL
          }
          } } in
      \ (z2_abXc :: Data.Vector.Unboxed.Base.Vector Int) ->
        let {
          lvl106_sB83 :: Int

          lvl106_sB83 = $s!_$slength1 (z2_abXc `cast` ...) } in
        \ (w3_abXd :: Data.Vector.Unboxed.Base.Vector Int) ->
          let {
            lvl107_sB9W :: Int

            lvl107_sB9W = $s!_$slength1 (w3_abXd `cast` ...) } in
          \ (doc4_abXe :: Data.Vector.Unboxed.Base.Vector Int)
            (docUpdate5_abXf :: Int) ->
            let {
              lvl108_sB8e :: Int

              lvl108_sB8e = $s!_$slength1 (doc4_abXe `cast` ...) } in
            let {
              w_ak1m [Dmd=<L,U(U,U,U)>] :: MayBoxVec Double Double

              w_ak1m =
                case topic_prior_length of _ [Occ=Dead] { I# ww1_agE0 ->
                let {
                  lvl109_shTc [Dmd=<L,U(U)>] :: Double

                  lvl109_shTc =
                    case lvl106_sB83 of _ [Occ=Dead] { I# ww3_agDQ ->
                    let {
                      topic_prior_length_ [Dmd=<S,U>] :: Int#

                      topic_prior_length_ = -# ww3_agDQ 1# } in
                    case tagToEnum# @ Bool (># 0# topic_prior_length_)
                    of _ [Occ=Dead] {
                      False ->
                        let {
                          $sw1_sEm0 :: Int# -> Int

                          $sw1_sEm0 =
                            \ (sc_sElY :: Int#) ->
                              Language.Hakaru.Runtime.Prelude.case_1
                                @ Int
                                @ Bool
                                (case docUpdate5_abXf of _ [Occ=Dead] { I# y1_agII ->
                                 tagToEnum# @ Bool (==# sc_sElY y1_agII)
                                 })
                                (:
                                   @ (Branch Bool Int)
                                   (lvl82_rIrX `cast` ...)
                                   (:
                                      @ (Branch Bool Int)
                                      (let {
                                         b1_XkvM :: Int

                                         b1_XkvM =
                                           Language.Hakaru.Runtime.Prelude.case_1
                                             @ Int
                                             @ Bool
                                             (case tagToEnum#
                                                     @ Bool (>=# sc_sElY 0#)
                                              of _ [Occ=Dead] {
                                                False ->
                                                  case lvl49_rIrg ww3_agDQ sc_sElY of wild2_00 { };
                                                True ->
                                                  case tagToEnum#
                                                         @ Bool (<# sc_sElY ww3_agDQ)
                                                  of _ [Occ=Dead] {
                                                    False ->
                                                      case lvl49_rIrg ww3_agDQ sc_sElY of wild2_00 {
                                                      };
                                                    True ->
                                                      case z2_abXc `cast` ...
                                                      of _ [Occ=Dead]
                                                      { Data.Vector.Primitive.Vector dt_ai1q
                                                                                     dt1_ai1r
                                                                                     dt2_ai1s ->
                                                      case indexIntArray#
                                                             dt2_ai1s (+# dt_ai1q sc_sElY)
                                                      of wild4_ai1B { __DEFAULT ->
                                                      tagToEnum#
                                                        @ Bool (<# wild4_ai1B 0#)
                                                      }
                                                      }
                                                  }
                                              })
                                             lvl85_rIs0 } in
                                       let {
                                         lvl110_sB8b :: Maybe Int

                                         lvl110_sB8b = Just @ Int b1_XkvM } in
                                       (\ (p_XkvP :: Bool) ->
                                          case p_XkvP of _ [Occ=Dead] {
                                            False -> lvl110_sB8b;
                                            True -> Nothing @ Int
                                          })
                                       `cast` ...)
                                      ([] @ (Branch Bool Int)))) } in
                        letrec {
                          $wgo3_syqN [InlPrag=[0], Occ=LoopBreaker]
                            :: Int# -> Int# -> Int#

                          $wgo3_syqN =
                            \ (w1_syqE :: Int#)
                              (ww4_syqI [OS=OneShot] :: Int#) ->
                              case tagToEnum# @ Bool (==# w1_syqE topic_prior_length_)
                              of _ [Occ=Dead] {
                                False ->
                                  case $sw1_sEm0 w1_syqE of _ [Occ=Dead] { I# y1_agKz ->
                                  $wgo3_syqN (+# w1_syqE 1#) (+# ww4_syqI y1_agKz)
                                  };
                                True ->
                                  case $sw1_sEm0 w1_syqE of _ [Occ=Dead] { I# y1_agKz ->
                                  +# ww4_syqI y1_agKz
                                  }
                              }; } in
                        case $wgo3_syqN 0# 0# of ww4_syqM { __DEFAULT ->
                        case lvl103_shTb of _ [Occ=Dead] { D# y1_aibp ->
                        case /##
                               1.0## (+## (int2Double# ww4_syqM) y1_aibp)
                        of wild2_agLC { __DEFAULT ->
                        D# wild2_agLC
                        }
                        }
                        };
                      True ->
                        case lvl103_shTb of _ [Occ=Dead] { D# y1_aibp ->
                        case /## 1.0## y1_aibp of wild2_agLC { __DEFAULT ->
                        D# wild2_agLC
                        }
                        }
                    }
                    } } in
                Language.Hakaru.Runtime.Prelude.$warray
                  @ Double
                  (Data.Vector.Unboxed.Base.$fVectorVectorDouble `cast` ...)
                  ww1_agE0
                  (\ (zNew丏6_abXg :: Int) ->
                     let {
                       y_ahjK [Dmd=<S,U>] :: Int#

                       y_ahjK = -# ww1_agE0 1# } in
                     case tagToEnum# @ Bool (># 0# y_ahjK)
                     of _ [Occ=Dead] {
                       False ->
                         let {
                           lvl110_shSS [Dmd=<L,U(U)>] :: Double

                           lvl110_shSS =
                             case lvl106_sB83 of _ [Occ=Dead] { I# ww3_agDQ ->
                             let {
                               y1_ahgy [Dmd=<S,U>] :: Int#

                               y1_ahgy = -# ww3_agDQ 1# } in
                             let {
                               $w$j_syrM [InlPrag=[0]] :: Int# -> Double#

                               $w$j_syrM =
                                 \ (w1_syrI [OS=OneShot] :: Int#) ->
                                   case zNew6_abXg of _ [Occ=Dead] { I# x_ahkY ->
                                   case tagToEnum# @ Bool (>=# x_ahkY 0#)
                                   of _ [Occ=Dead] {
                                     False -> case lvl50_rIrh ww1_agE0 x_ahkY of wild3_00 { };
                                     True ->
                                       case tagToEnum# @ Bool (<# x_ahkY ww1_agE0)
                                       of _ [Occ=Dead] {
                                         False -> case lvl50_rIrh ww1_agE0 x_ahkY of wild4_00 { };
                                         True ->
                                           case topic_prior0_abXa `cast` ...
                                           of _ [Occ=Dead]
                                           { Data.Vector.Primitive.Vector dt_offset dt_len
                                                                          dt_bytearray ->
                                           case indexDoubleArray#
                                                  dt_bytearray (+# dt_offset x_ahkY)
                                           of wild5_ajiO { __DEFAULT ->
                                           +## (int2Double# w1_syrI) wild5_ajiO
                                           }
                                           }
                                       }
                                   }
                                   } } in
                             case tagToEnum# @ Bool (># 0# y1_ahgy)
                             of _ [Occ=Dead] {
                               False ->
                                 let {
                                   $sw1_sEm7 :: Int# -> Int

                                   $sw1_sEm7 =
                                     \ (sc_sEm5 :: Int#) ->
                                       Language.Hakaru.Runtime.Prelude.case_1
                                         @ Int
                                         @ Bool
                                         (case docUpdate5_abXf
                                          of _ [Occ=Dead] { I# y2_agII ->
                                          tagToEnum# @ Bool (==# sc_sEm5 y2_agII)
                                          })
                                         (:
                                            @ (Branch Bool Int)
                                            (lvl82_rIrX `cast` ...)
                                            (:
                                               @ (Branch Bool Int)
                                               (let {
                                                  b1_XkvP :: Int

                                                  b1_XkvP =
                                                    Language.Hakaru.Runtime.Prelude.case_1
                                                      @ Int
                                                      @ Bool
                                                      (case zNew丏6_abXg
                                                       of _ [Occ=Dead] { I# x_agIE ->
                                                       case tagToEnum#
                                                              @ Bool (>=# sc_sEm5 0#)
                                                       of _ [Occ=Dead] {
                                                         False ->
                                                           case lvl49_rIrg ww3_agDQ sc_sEm5
                                                           of wild4_00 {
                                                           };
                                                         True ->
                                                           case tagToEnum#
                                                                  @ Bool
                                                                  (<# sc_sEm5 ww3_agDQ)
                                                           of _ [Occ=Dead] {
                                                             False ->
                                                               case lvl49_rIrg ww3_agDQ sc_sEm5
                                                               of wild5_00 {
                                                               };
                                                             True ->
                                                               case z2_abXc `cast` ...
                                                               of _ [Occ=Dead]
                                                               { Data.Vector.Primitive.Vector dt_ai1q
                                                                                              dt1_ai1r
                                                                                              dt2_ai1s ->
                                                               case indexIntArray#
                                                                      dt2_ai1s
                                                                      (+# dt_ai1q sc_sEm5)
                                                               of wild6_ai1B { __DEFAULT ->
                                                               tagToEnum#
                                                                 @ Bool
                                                                 (==# x_agIE wild6_ai1B)
                                                               }
                                                               }
                                                           }
                                                       }
                                                       })
                                                      lvl81_rIrW } in
                                                let {
                                                  lvl111_sB9f :: Maybe Int

                                                  lvl111_sB9f = Just @ Int b1_XkvP } in
                                                (\ (p_XkvS :: Bool) ->
                                                   case p_XkvS of _ [Occ=Dead] {
                                                     False -> lvl111_sB9f;
                                                     True -> Nothing @ Int
                                                   })
                                                `cast` ...)
                                               ([] @ (Branch Bool Int)))) } in
                                 letrec {
                                   $wgo3_syrX [InlPrag=[0], Occ=LoopBreaker]
                                     :: Int# -> Int# -> Int#

                                   $wgo3_syrX =
                                     \ (w1_syrO :: Int#)
                                       (ww4_syrS [OS=OneShot] :: Int#) ->
                                       case tagToEnum#
                                              @ Bool (==# w1_syrO y1_ahgy)
                                       of _ [Occ=Dead] {
                                         False ->
                                           case $sw1_sEm7 w1_syrO
                                           of _ [Occ=Dead] { I# y2_agKz ->
                                           $wgo3_syrX
                                             (+# w1_syrO 1#) (+# ww4_syrS y2_agKz)
                                           };
                                         True ->
                                           case $sw1_sEm7 w1_syrO
                                           of _ [Occ=Dead] { I# y2_agKz ->
                                           +# ww4_syrS y2_agKz
                                           }
                                       }; } in
                                 case $wgo3_syrX 0# 0# of ww4_syrW { __DEFAULT ->
                                 case $w$j_syrM ww4_syrW of ww5_syrL { __DEFAULT ->
                                 D# ww5_syrL
                                 }
                                 };
                               True ->
                                 case $w$j_syrM 0# of ww4_syrL { __DEFAULT ->
                                 D# ww4_syrL
                                 }
                             }
                             } } in
                         let {
                           lvl111_shTP [Dmd=<L,U(U)>] :: Double

                           lvl111_shTP =
                             let {
                               y1_Xhtx [Dmd=<S,U>] :: Int#

                               y1_Xhtx = -# ww1_agE0 1# } in
                             case tagToEnum# @ Bool (># 0# y1_Xhtx)
                             of _ [Occ=Dead] {
                               False ->
                                 let {
                                   $s$ww1_sEmC :: Int# -> Double#

                                   $s$ww1_sEmC =
                                     \ (sc_sEmA :: Int#) ->
                                       case lvl107_sB9W of _ [Occ=Dead] { I# ww3_agDQ ->
                                       let {
                                         y2_ahgy [Dmd=<S,U>] :: Int#

                                         y2_ahgy = -# ww3_agDQ 1# } in
                                       let {
                                         $w$j_syrh [InlPrag=[0]]
                                           :: Int# -> Double#

                                         $w$j_syrh =
                                           \ (w1_syrd [OS=OneShot] :: Int#) ->
                                             let {
                                               y3_XhtL [Dmd=<S,U>] :: Int#

                                               y3_XhtL = -# w1_syrd 1# } in
                                             case tagToEnum#
                                                    @ Bool (># 0# y3_XhtL)
                                             of _ [Occ=Dead] {
                                               False ->
                                                 let {
                                                   y4_XhqI [Dmd=<S,U>] :: Int#

                                                   y4_XhqI = -# ww3_agDQ 1# } in
                                                 let {
                                                   $j_szg3 :: Double# -> Double#

                                                   $j_szg3 =
                                                     \ (ww4_syr1 [OS=OneShot]
                                                          :: Double#) ->
                                                       case y3_XhtL of wild3_X9W {
                                                         __DEFAULT ->
                                                           case lvl105_shTO
                                                           of _ [Occ=Dead] { D# y5_XiGX ->
                                                           letrec {
                                                             $wgo3_syrc [InlPrag=[0],
                                                                         Occ=LoopBreaker]
                                                               :: Int#
                                                                  -> Double#
                                                                  -> Double#
                                                             [LclId,
                                                              Arity=2,

                                                             $wgo3_syrc =
                                                               \ (w2_XyB5 :: Int#)
                                                                 (ww5_XyBa [OS=OneShot]
                                                                    :: Double#) ->
                                                                 case tagToEnum#
                                                                        @ Bool
                                                                        (==#
                                                                           w2_XyB5 wild3_X9W)
                                                                 of _ [Occ=Dead] {
                                                                   False ->
                                                                     $wgo3_syrc
                                                                       (+# w2_XyB5 1#)
                                                                       (*##
                                                                          ww5_XyBa
                                                                          (+##
                                                                             (+##
                                                                                ww4_syr1
                                                                                (int2Double#
                                                                                   w2_XyB5))
                                                                             y5_XiGX));
                                                                   True ->
                                                                     *##
                                                                       ww5_XyBa
                                                                       (+##
                                                                          (+##
                                                                             ww4_syr1
                                                                             (int2Double#
                                                                                w2_XyB5))
                                                                          y5_XiGX)
                                                                 }; } in
                                                           $wgo3_syrc
                                                             1# (+## ww4_syr1 y5_XiGX)
                                                           };
                                                         0# ->
                                                           case lvl105_shTO
                                                           of _ [Occ=Dead] { D# y5_XiGX ->
                                                           +## ww4_syr1 y5_XiGX
                                                           }
                                                       } } in
                                                 case tagToEnum#
                                                        @ Bool (># 0# y4_XhqI)
                                                 of _ [Occ=Dead] {
                                                   False ->
                                                     let {
                                                       $sw1_sEmN :: Int# -> Int

                                                       $sw1_sEmN =
                                                         \ (sc1_sEmL :: Int#) ->
                                                           Language.Hakaru.Runtime.Prelude.case_1
                                                             @ Int
                                                             @ Bool
                                                             (case tagToEnum#
                                                                     @ Bool
                                                                     (>=# sc1_sEmL 0#)
                                                              of _ [Occ=Dead] {
                                                                False ->
                                                                  case lvl108_sB8e
                                                                  of _ [Occ=Dead]
                                                                  { I# n#_ahl6 ->
                                                                  case lvl49_rIrg n#_ahl6 sc1_sEmL
                                                                  of wild6_00 {
                                                                  }
                                                                  };
                                                                True ->
                                                                  case lvl108_sB8e
                                                                  of _ [Occ=Dead]
                                                                  { I# y5_ahlc ->
                                                                  case tagToEnum#
                                                                         @ Bool
                                                                         (<#
                                                                            sc1_sEmL y5_ahlc)
                                                                  of _ [Occ=Dead] {
                                                                    False ->
                                                                      case lvl49_rIrg
                                                                             y5_ahlc sc1_sEmL
                                                                      of wild7_00 {
                                                                      };
                                                                    True ->
                                                                      case doc4_abXe `cast` ...
                                                                      of _ [Occ=Dead]
                                                                      { Data.Vector.Primitive.Vector dt_ai1q
                                                                                                     dt1_ai1r
                                                                                                     dt2_ai1s ->
                                                                      case docUpdate5_abXf
                                                                      of _ [Occ=Dead]
                                                                      { I# y6_agII ->
                                                                      case indexIntArray#
                                                                             dt2_ai1s
                                                                             (+#
                                                                                dt_ai1q sc1_sEmL)
                                                                      of wild9_ai1B { __DEFAULT ->
                                                                      tagToEnum#
                                                                        @ Bool
                                                                        (==#
                                                                           wild9_ai1B y6_agII)
                                                                      }
                                                                      }
                                                                      }
                                                                  }
                                                                  }
                                                              })
                                                             (:
                                                                @ (Branch Bool Int)
                                                                (lvl82_rIrX `cast` ...)
                                                                (:
                                                                   @ (Branch Bool Int)
                                                                   (let {
                                                                      b1_XkvV :: Int

                                                                      b1_XkvV =
                                                                        Language.Hakaru.Runtime.Prelude.case_1
                                                                          @ Int
                                                                          @ Bool
                                                                          (case tagToEnum#
                                                                                  @ Bool
                                                                                  (>=#
                                                                                     sc1_sEmL 0#)
                                                                           of _ [Occ=Dead] {
                                                                             False ->
                                                                               case lvl108_sB8e
                                                                               of _ [Occ=Dead]
                                                                               { I# n#_ahl6 ->
                                                                               case lvl49_rIrg
                                                                                      n#_ahl6
                                                                                      sc1_sEmL
                                                                               of wild6_00 {
                                                                               }
                                                                               };
                                                                             True ->
                                                                               case lvl108_sB8e
                                                                               of _ [Occ=Dead]
                                                                               { I# y5_ahlc ->
                                                                               case tagToEnum#
                                                                                      @ Bool
                                                                                      (<#
                                                                                         sc1_sEmL
                                                                                         y5_ahlc)
                                                                               of _ [Occ=Dead] {
                                                                                 False ->
                                                                                   case lvl49_rIrg
                                                                                          y5_ahlc
                                                                                          sc1_sEmL
                                                                                   of wild7_00 {
                                                                                   };
                                                                                 True ->
                                                                                   case doc4_abXe
                                                                                        `cast` ...
                                                                                   of _ [Occ=Dead]
                                                                                   { Data.Vector.Primitive.Vector dt_ai1q
                                                                                                                  dt1_ai1r
                                                                                                                  dt2_ai1s ->
                                                                                   case indexIntArray#
                                                                                          dt2_ai1s
                                                                                          (+#
                                                                                             dt_ai1q
                                                                                             sc1_sEmL)
                                                                                   of wild8_ai1B
                                                                                   { __DEFAULT ->
                                                                                   case tagToEnum#
                                                                                          @ Bool
                                                                                          (>=#
                                                                                             wild8_ai1B
                                                                                             0#)
                                                                                   of _ [Occ=Dead] {
                                                                                     False ->
                                                                                       case lvl106_sB83
                                                                                       of _ [Occ=Dead]
                                                                                       { I# n#_ahl6 ->
                                                                                       case lvl49_rIrg
                                                                                              n#_ahl6
                                                                                              wild8_ai1B
                                                                                       of wild11_00 {
                                                                                       }
                                                                                       };
                                                                                     True ->
                                                                                       case lvl106_sB83
                                                                                       of _ [Occ=Dead]
                                                                                       { I# y6_XhwA ->
                                                                                       case tagToEnum#
                                                                                              @ Bool
                                                                                              (<#
                                                                                                 wild8_ai1B
                                                                                                 y6_XhwA)
                                                                                       of _ [Occ=Dead] {
                                                                                         False ->
                                                                                           case lvl49_rIrg
                                                                                                  y6_XhwA
                                                                                                  wild8_ai1B
                                                                                           of wild12_00 {
                                                                                           };
                                                                                         True ->
                                                                                           case z2_abXc
                                                                                                `cast` ...
                                                                                           of _ [Occ=Dead]
                                                                                           { Data.Vector.Primitive.Vector dt4_XicT
                                                                                                                          dt5_XicV
                                                                                                                          dt6_XicX ->
                                                                                           case indexIntArray#
                                                                                                  dt6_XicX
                                                                                                  (+#
                                                                                                     dt4_XicT
                                                                                                     wild8_ai1B)
                                                                                           of wild13_Xid9
                                                                                           { __DEFAULT ->
                                                                                           tagToEnum#
                                                                                             @ Bool
                                                                                             (==#
                                                                                                sc_sEmA
                                                                                                wild13_Xid9)
                                                                                           }
                                                                                           }
                                                                                       }
                                                                                       }
                                                                                   }
                                                                                   }
                                                                                   }
                                                                               }
                                                                               }
                                                                           })
                                                                          lvl81_rIrW } in
                                                                    let {
                                                                      lvl112_sB8G :: Maybe Int

                                                                      lvl112_sB8G =
                                                                        Just
                                                                          @ Int b1_XkvV } in
                                                                    (\ (p_XkvY :: Bool) ->
                                                                       case p_XkvY of _ [Occ=Dead] {
                                                                         False -> lvl112_sB8G;
                                                                         True ->
                                                                           Nothing @ Int
                                                                       })
                                                                    `cast` ...)
                                                                   ([]
                                                                      @ (Branch Bool Int)))) } in
                                                     letrec {
                                                       $wgo3_syqY [InlPrag=[0], Occ=LoopBreaker]
                                                         :: Int#
                                                            -> Int# -> Int#

                                                       $wgo3_syqY =
                                                         \ (w2_syqP :: Int#)
                                                           (ww4_syqT [OS=OneShot]
                                                              :: Int#) ->
                                                           case tagToEnum#
                                                                  @ Bool
                                                                  (==# w2_syqP y4_XhqI)
                                                           of _ [Occ=Dead] {
                                                             False ->
                                                               case $sw1_sEmN w2_syqP
                                                               of _ [Occ=Dead]
                                                               { I# y5_agKz ->
                                                               $wgo3_syqY
                                                                 (+# w2_syqP 1#)
                                                                 (+# ww4_syqT y5_agKz)
                                                               };
                                                             True ->
                                                               case $sw1_sEmN w2_syqP
                                                               of _ [Occ=Dead]
                                                               { I# y5_agKz ->
                                                               +# ww4_syqT y5_agKz
                                                               }
                                                           }; } in
                                                     case $wgo3_syqY 0# 0#
                                                     of ww4_syqX { __DEFAULT ->
                                                     $j_szg3 (int2Double# ww4_syqX)
                                                     };
                                                   True -> $j_szg3 0.0##
                                                 };
                                               True -> 1.0##
                                             } } in
                                       case tagToEnum# @ Bool (># 0# y2_ahgy)
                                       of _ [Occ=Dead] {
                                         False ->
                                           let {
                                             b1_XkFV :: Int

                                             b1_XkFV =
                                               Language.Hakaru.Runtime.Prelude.case_1
                                                 @ Int
                                                 @ Bool
                                                 (case zNew丏6_abXg
                                                  of _ [Occ=Dead] { I# y3_agII ->
                                                  tagToEnum#
                                                    @ Bool (==# sc_sEmA y3_agII)
                                                  })
                                                 lvl81_rIrW } in
                                           let {
                                             lvl112_sB8J :: Maybe Int

                                             lvl112_sB8J = Just @ Int b1_XkFV } in
                                           let {
                                             lvl113_sqLf :: Bool -> Maybe Int
                                             [LclId,
                                              Arity=1,

                                              Unf=Unf{Src=InlineStable, TopLvl=False, Value=True,
                                                      ConLike=True, WorkFree=True, Expandable=True,
                                                      Guidance=ALWAYS_IF(arity=1,unsat_ok=True,boring_ok=False)
                                                      Tmpl= \ (p_XkG0 [Occ=Once!] :: Bool) ->
                                                              case p_XkG0 of _ [Occ=Dead] {
                                                                False -> Nothing @ Int;
                                                                True -> Just @ Int b1_XkFV
                                                              }}]
                                             lvl113_sqLf =
                                               \ (p_XkG0 :: Bool) ->
                                                 case p_XkG0 of _ [Occ=Dead] {
                                                   False -> Nothing @ Int;
                                                   True -> lvl112_sB8J
                                                 } } in
                                           let {
                                             lvl114_shTw :: [Branch Bool Int]

                                             lvl114_shTw =
                                               :
                                                 @ (Branch Bool Int)
                                                 (lvl113_sqLf `cast` ...)
                                                 lvl78_rIrS } in
                                           let {
                                             $sw1_sEmW :: Int# -> Int

                                             $sw1_sEmW =
                                               \ (sc1_sEmU :: Int#) ->
                                                 Language.Hakaru.Runtime.Prelude.case_1
                                                   @ Int
                                                   @ Bool
                                                   (case docUpdate5_abXf
                                                    of _ [Occ=Dead] { I# x_agIE ->
                                                    case tagToEnum#
                                                           @ Bool (>=# sc1_sEmU 0#)
                                                    of _ [Occ=Dead] {
                                                      False ->
                                                        case lvl108_sB8e
                                                        of _ [Occ=Dead] { I# n#_ahl6 ->
                                                        case lvl49_rIrg n#_ahl6 sc1_sEmU
                                                        of wild6_00 {
                                                        }
                                                        };
                                                      True ->
                                                        case lvl108_sB8e
                                                        of _ [Occ=Dead] { I# y3_ahlc ->
                                                        case tagToEnum#
                                                               @ Bool (<# sc1_sEmU y3_ahlc)
                                                        of _ [Occ=Dead] {
                                                          False ->
                                                            case lvl49_rIrg y3_ahlc sc1_sEmU
                                                            of wild7_00 {
                                                            };
                                                          True ->
                                                            case doc4_abXe `cast` ...
                                                            of _ [Occ=Dead]
                                                            { Data.Vector.Primitive.Vector dt_ai1q
                                                                                           dt1_ai1r
                                                                                           dt2_ai1s ->
                                                            case indexIntArray#
                                                                   dt2_ai1s
                                                                   (+# dt_ai1q sc1_sEmU)
                                                            of wild8_ai1B { __DEFAULT ->
                                                            tagToEnum#
                                                              @ Bool
                                                              (==# x_agIE wild8_ai1B)
                                                            }
                                                            }
                                                        }
                                                        }
                                                    }
                                                    })
                                                   lvl114_shTw } in
                                           letrec {
                                             $wgo3_syrs [InlPrag=[0], Occ=LoopBreaker]
                                               :: Int# -> Int# -> Int#

                                             $wgo3_syrs =
                                               \ (w1_syrj :: Int#)
                                                 (ww4_syrn [OS=OneShot] :: Int#) ->
                                                 case tagToEnum#
                                                        @ Bool (==# w1_syrj y2_ahgy)
                                                 of _ [Occ=Dead] {
                                                   False ->
                                                     case $sw1_sEmW w1_syrj
                                                     of _ [Occ=Dead] { I# y3_agKz ->
                                                     $wgo3_syrs
                                                       (+# w1_syrj 1#)
                                                       (+# ww4_syrn y3_agKz)
                                                     };
                                                   True ->
                                                     case $sw1_sEmW w1_syrj
                                                     of _ [Occ=Dead] { I# y3_agKz ->
                                                     +# ww4_syrn y3_agKz
                                                     }
                                                 }; } in
                                           case $wgo3_syrs 0# 0# of ww4_syrr { __DEFAULT ->
                                           $w$j_syrh ww4_syrr
                                           };
                                         True -> $w$j_syrh 0#
                                       }
                                       } } in
                                 letrec {
                                   $wgo3_syrH [InlPrag=[0], Occ=LoopBreaker]
                                     :: Int# -> Double# -> Double#

                                   $wgo3_syrH =
                                     \ (w1_syry :: Int#)
                                       (ww2_syrC [OS=OneShot] :: Double#) ->
                                       case tagToEnum#
                                              @ Bool (==# w1_syry y1_Xhtx)
                                       of _ [Occ=Dead] {
                                         False ->
                                           case $s$ww1_sEmC w1_syry of ww3_syrw { __DEFAULT ->
                                           $wgo3_syrH
                                             (+# w1_syry 1#)
                                             (*## ww2_syrC ww3_syrw)
                                           };
                                         True ->
                                           case $s$ww1_sEmC w1_syry of ww3_syrw { __DEFAULT ->
                                           *## ww2_syrC ww3_syrw
                                           }
                                       }; } in
                                 case $wgo3_syrH 0# 1.0## of ww2_syrG { __DEFAULT ->
                                 case /## 1.0## ww2_syrG of wild2_agLC { __DEFAULT ->
                                 D# wild2_agLC
                                 }
                                 };
                               True -> labelHP
                             } } in
                         let {
                           $s$ww1_sEnU :: Int# -> Double#

                           $s$ww1_sEnU =
                             \ (sc_sEnS :: Int#) ->
                               case lvl104_sB7S of _ [Occ=Dead] { I# ww3_XgQG ->
                               let {
                                 y1_XhtC [Dmd=<S,U>] :: Int#

                                 y1_XhtC = -# ww3_XgQG 1# } in
                               case tagToEnum# @ Bool (># 0# y1_XhtC)
                               of _ [Occ=Dead] {
                                 False ->
                                   let {
                                     lvl112_shSc :: Bool

                                     lvl112_shSc =
                                       case zNew6_abXg of _ [Occ=Dead] { I# y2_agII ->
                                       tagToEnum# @ Bool (==# sc_sEnS y2_agII)
                                       } } in
                                   let {
                                     $s$ww2_sEon :: Int# -> Double#

                                     $s$ww2_sEon =
                                       \ (sc1_sEol :: Int#) ->
                                         case lvl107_sB9W of _ [Occ=Dead] { I# ww5_agDQ ->
                                         let {
                                           y2_ahgy [Dmd=<S,U>] :: Int#

                                           y2_ahgy = -# ww5_agDQ 1# } in
                                         let {
                                           $w$j_sysz [InlPrag=[0]]
                                             :: Int# -> Double#

                                           $w$j_sysz =
                                             \ (w1_sysv [OS=OneShot] :: Int#) ->
                                               let {
                                                 y3_XhtS [Dmd=<S,U>] :: Int#

                                                 y3_XhtS = -# w1_sysv 1# } in
                                               case tagToEnum#
                                                      @ Bool (># 0# y3_XhtS)
                                               of _ [Occ=Dead] {
                                                 False ->
                                                   case tagToEnum#
                                                          @ Bool (>=# sc1_sEol 0#)
                                                   of _ [Occ=Dead] {
                                                     False ->
                                                       case lvl50_rIrh ww3_XgQG sc1_sEol
                                                       of wild4_00 {
                                                       };
                                                     True ->
                                                       case tagToEnum#
                                                              @ Bool (<# sc1_sEol ww3_XgQG)
                                                       of _ [Occ=Dead] {
                                                         False ->
                                                           case lvl50_rIrh ww3_XgQG sc1_sEol
                                                           of wild5_00 {
                                                           };
                                                         True ->
                                                           case word_prior1_abXb `cast` ...
                                                           of _ [Occ=Dead]
                                                           { Data.Vector.Primitive.Vector dt_offset
                                                                                          dt_len
                                                                                          dt_bytearray ->
                                                           let {
                                                             y4_XhqR [Dmd=<S,U>] :: Int#

                                                             y4_XhqR = -# ww5_agDQ 1# } in
                                                           let {
                                                             $j_szhZ
                                                               :: Double#
                                                                  -> Double#

                                                             $j_szhZ =
                                                               \ (ww6_sysf [OS=OneShot]
                                                                    :: Double#) ->
                                                                 case indexDoubleArray#
                                                                        dt_bytearray
                                                                        (+#
                                                                           dt_offset sc1_sEol)
                                                                 of wild6_ajiO { __DEFAULT ->
                                                                 case lvl110_shSS
                                                                 of _ [Occ=Dead]
                                                                 { D# y5_aicx ->
                                                                 case lvl109_shTc
                                                                 of _ [Occ=Dead]
                                                                 { D# y6_XiIj ->
                                                                 case lvl111_shTP
                                                                 of _ [Occ=Dead]
                                                                 { D# y7_XiIo ->
                                                                 case y3_XhtS of wild10_XaB {
                                                                   __DEFAULT ->
                                                                     letrec {
                                                                       $wgo3_sysu [InlPrag=[0],
                                                                                   Occ=LoopBreaker]
                                                                         :: Int#
                                                                            -> Double#
                                                                            -> Double#
                                                                       [LclId,
                                                                        Arity=2,

                                                                       $wgo3_sysu =
                                                                         \ (w2_XyCX
                                                                              :: Int#)
                                                                           (ww7_XyD2 [OS=OneShot]
                                                                              :: Double#) ->
                                                                           case tagToEnum#
                                                                                  @ Bool
                                                                                  (==#
                                                                                     w2_XyCX
                                                                                     wild10_XaB)
                                                                           of _ [Occ=Dead] {
                                                                             False ->
                                                                               $wgo3_sysu
                                                                                 (+#
                                                                                    w2_XyCX 1#)
                                                                                 (*##
                                                                                    ww7_XyD2
                                                                                    (*##
                                                                                       (*##
                                                                                          (*##
                                                                                             (+##
                                                                                                (+##
                                                                                                   ww6_sysf
                                                                                                   (int2Double#
                                                                                                      w2_XyCX))
                                                                                                wild6_ajiO)
                                                                                             y5_aicx)
                                                                                          y6_XiIj)
                                                                                       y7_XiIo));
                                                                             True ->
                                                                               *##
                                                                                 ww7_XyD2
                                                                                 (*##
                                                                                    (*##
                                                                                       (*##
                                                                                          (+##
                                                                                             (+##
                                                                                                ww6_sysf
                                                                                                (int2Double#
                                                                                                   w2_XyCX))
                                                                                             wild6_ajiO)
                                                                                          y5_aicx)
                                                                                       y6_XiIj)
                                                                                    y7_XiIo)
                                                                           }; } in
                                                                     $wgo3_sysu
                                                                       1#
                                                                       (*##
                                                                          (*##
                                                                             (*##
                                                                                (+##
                                                                                   ww6_sysf
                                                                                   wild6_ajiO)
                                                                                y5_aicx)
                                                                             y6_XiIj)
                                                                          y7_XiIo);
                                                                   0# ->
                                                                     *##
                                                                       (*##
                                                                          (*##
                                                                             (+##
                                                                                ww6_sysf wild6_ajiO)
                                                                             y5_aicx)
                                                                          y6_XiIj)
                                                                       y7_XiIo
                                                                 }
                                                                 }
                                                                 }
                                                                 }
                                                                 } } in
                                                           case tagToEnum#
                                                                  @ Bool (># 0# y4_XhqR)
                                                           of _ [Occ=Dead] {
                                                             False ->
                                                               let {
                                                                 w2_ahgx :: Int -> Int
                                                                 [LclId,
                                                                  Arity=1,

                                                                 w2_ahgx =
                                                                   \ (i丙11_abXl :: Int) ->
                                                                     Language.Hakaru.Runtime.Prelude.case_1
                                                                       @ Int
                                                                       @ Bool
                                                                       (case i丙11_abXl
                                                                        of _ [Occ=Dead]
                                                                        { I# x_XhvT ->
                                                                        case tagToEnum#
                                                                               @ Bool
                                                                               (>=#
                                                                                  x_XhvT 0#)
                                                                        of _ [Occ=Dead] {
                                                                          False ->
                                                                            case lvl108_sB8e
                                                                            of _ [Occ=Dead]
                                                                            { I# n#_ahl6 ->
                                                                            case lvl49_rIrg
                                                                                   n#_ahl6 x_XhvT
                                                                            of wild10_00 {
                                                                            }
                                                                            };
                                                                          True ->
                                                                            case lvl108_sB8e
                                                                            of _ [Occ=Dead]
                                                                            { I# y5_Xhwc ->
                                                                            case tagToEnum#
                                                                                   @ Bool
                                                                                   (<#
                                                                                      x_XhvT
                                                                                      y5_Xhwc)
                                                                            of _ [Occ=Dead] {
                                                                              False ->
                                                                                case lvl49_rIrg
                                                                                       y5_Xhwc
                                                                                       x_XhvT
                                                                                of wild11_00 {
                                                                                };
                                                                              True ->
                                                                                case doc4_abXe
                                                                                     `cast` ...
                                                                                of _ [Occ=Dead]
                                                                                { Data.Vector.Primitive.Vector dt4_ai1q
                                                                                                               dt5_ai1r
                                                                                                               dt6_ai1s ->
                                                                                case docUpdate5_abXf
                                                                                of _ [Occ=Dead]
                                                                                { I# y6_agII ->
                                                                                case indexIntArray#
                                                                                       dt6_ai1s
                                                                                       (+#
                                                                                          dt4_ai1q
                                                                                          x_XhvT)
                                                                                of wild13_ai1B
                                                                                { __DEFAULT ->
                                                                                tagToEnum#
                                                                                  @ Bool
                                                                                  (==#
                                                                                     wild13_ai1B
                                                                                     y6_agII)
                                                                                }
                                                                                }
                                                                                }
                                                                            }
                                                                            }
                                                                        }
                                                                        })
                                                                       (:
                                                                          @ (Branch Bool Int)
                                                                          (lvl82_rIrX `cast` ...)
                                                                          (:
                                                                             @ (Branch Bool Int)
                                                                             (let {
                                                                                b1_XkvX :: Int

                                                                                b1_XkvX =
                                                                                  Language.Hakaru.Runtime.Prelude.case_1
                                                                                    @ Int
                                                                                    @ Bool
                                                                                    (case i丙11_abXl
                                                                                     of _ [Occ=Dead]
                                                                                     { I# x_XhvY ->
                                                                                     case tagToEnum#
                                                                                            @ Bool
                                                                                            (>=#
                                                                                               x_XhvY
                                                                                               0#)
                                                                                     of _ [Occ=Dead] {
                                                                                       False ->
                                                                                         case lvl108_sB8e
                                                                                         of _ [Occ=Dead]
                                                                                         { I# n#_ahl6 ->
                                                                                         case lvl49_rIrg
                                                                                                n#_ahl6
                                                                                                x_XhvY
                                                                                         of wild10_00 {
                                                                                         }
                                                                                         };
                                                                                       True ->
                                                                                         case lvl108_sB8e
                                                                                         of _ [Occ=Dead]
                                                                                         { I# y5_Xhwh ->
                                                                                         case tagToEnum#
                                                                                                @ Bool
                                                                                                (<#
                                                                                                   x_XhvY
                                                                                                   y5_Xhwh)
                                                                                         of _ [Occ=Dead] {
                                                                                           False ->
                                                                                             case lvl49_rIrg
                                                                                                    y5_Xhwh
                                                                                                    x_XhvY
                                                                                             of wild11_00 {
                                                                                             };
                                                                                           True ->
                                                                                             case doc4_abXe
                                                                                                  `cast` ...
                                                                                             of _ [Occ=Dead]
                                                                                             { Data.Vector.Primitive.Vector dt4_ai1q
                                                                                                                            dt5_ai1r
                                                                                                                            dt6_ai1s ->
                                                                                             case indexIntArray#
                                                                                                    dt6_ai1s
                                                                                                    (+#
                                                                                                       dt4_ai1q
                                                                                                       x_XhvY)
                                                                                             of wild12_ai1B
                                                                                             { __DEFAULT ->
                                                                                             case tagToEnum#
                                                                                                    @ Bool
                                                                                                    (>=#
                                                                                                       wild12_ai1B
                                                                                                       0#)
                                                                                             of _ [Occ=Dead] {
                                                                                               False ->
                                                                                                 case lvl106_sB83
                                                                                                 of _ [Occ=Dead]
                                                                                                 { I# n#_ahl6 ->
                                                                                                 case lvl49_rIrg
                                                                                                        n#_ahl6
                                                                                                        wild12_ai1B
                                                                                                 of wild15_00 {
                                                                                                 }
                                                                                                 };
                                                                                               True ->
                                                                                                 case lvl106_sB83
                                                                                                 of _ [Occ=Dead]
                                                                                                 { I# y6_Xhwv ->
                                                                                                 case tagToEnum#
                                                                                                        @ Bool
                                                                                                        (<#
                                                                                                           wild12_ai1B
                                                                                                           y6_Xhwv)
                                                                                                 of _ [Occ=Dead] {
                                                                                                   False ->
                                                                                                     case lvl49_rIrg
                                                                                                            y6_Xhwv
                                                                                                            wild12_ai1B
                                                                                                     of wild16_00 {
                                                                                                     };
                                                                                                   True ->
                                                                                                     case z2_abXc
                                                                                                          `cast` ...
                                                                                                     of _ [Occ=Dead]
                                                                                                     { Data.Vector.Primitive.Vector dt7_XicO
                                                                                                                                    dt8_XicQ
                                                                                                                                    dt9_XicS ->
                                                                                                     case indexIntArray#
                                                                                                            dt9_XicS
                                                                                                            (+#
                                                                                                               dt7_XicO
                                                                                                               wild12_ai1B)
                                                                                                     of wild17_Xid4
                                                                                                     { __DEFAULT ->
                                                                                                     case tagToEnum#
                                                                                                            @ Bool
                                                                                                            (==#
                                                                                                               sc_sEnS
                                                                                                               wild17_Xid4)
                                                                                                     of _ [Occ=Dead] {
                                                                                                       False ->
                                                                                                         False;
                                                                                                       True ->
                                                                                                         case tagToEnum#
                                                                                                                @ Bool
                                                                                                                (<#
                                                                                                                   x_XhvY
                                                                                                                   ww5_agDQ)
                                                                                                         of _ [Occ=Dead] {
                                                                                                           False ->
                                                                                                             case lvl49_rIrg
                                                                                                                    ww5_agDQ
                                                                                                                    x_XhvY
                                                                                                             of wild20_00 {
                                                                                                             };
                                                                                                           True ->
                                                                                                             case w3_abXd
                                                                                                                  `cast` ...
                                                                                                             of _ [Occ=Dead]
                                                                                                             { Data.Vector.Primitive.Vector dt10_Xidb
                                                                                                                                            dt11_Xidd
                                                                                                                                            dt12_Xidf ->
                                                                                                             case indexIntArray#
                                                                                                                    dt12_Xidf
                                                                                                                    (+#
                                                                                                                       dt10_Xidb
                                                                                                                       x_XhvY)
                                                                                                             of wild21_Xidr
                                                                                                             { __DEFAULT ->
                                                                                                             tagToEnum#
                                                                                                               @ Bool
                                                                                                               (==#
                                                                                                                  sc1_sEol
                                                                                                                  wild21_Xidr)
                                                                                                             }
                                                                                                             }
                                                                                                         }
                                                                                                     }
                                                                                                     }
                                                                                                     }
                                                                                                 }
                                                                                                 }
                                                                                             }
                                                                                             }
                                                                                             }
                                                                                         }
                                                                                         }
                                                                                     }
                                                                                     })
                                                                                    lvl81_rIrW } in
                                                                              let {
                                                                                lvl113_sBa4
                                                                                  :: Maybe Int

                                                                                lvl113_sBa4 =
                                                                                  Just
                                                                                    @ Int
                                                                                    b1_XkvX } in
                                                                              (\ (p_Xkw0 :: Bool) ->
                                                                                 case p_Xkw0
                                                                                 of _ [Occ=Dead] {
                                                                                   False ->
                                                                                     lvl113_sBa4;
                                                                                   True ->
                                                                                     Nothing
                                                                                       @ Int
                                                                                 })
                                                                              `cast` ...)
                                                                             ([]
                                                                                @ (Branch
                                                                                     Bool
                                                                                     Int)))) } in
                                                               letrec {
                                                                 $wgo3_sysc [InlPrag=[0],
                                                                             Occ=LoopBreaker]
                                                                   :: Int#
                                                                      -> Int#
                                                                      -> Int#
                                                                 [LclId,
                                                                  Arity=2,

                                                                 $wgo3_sysc =
                                                                   \ (w4_sys3 :: Int#)
                                                                     (ww6_sys7 [OS=OneShot]
                                                                        :: Int#) ->
                                                                     case tagToEnum#
                                                                            @ Bool
                                                                            (==#
                                                                               w4_sys3 y4_XhqR)
                                                                     of _ [Occ=Dead] {
                                                                       False ->
                                                                         case w2_ahgx
                                                                                (I#
                                                                                   w4_sys3)
                                                                         of _ [Occ=Dead]
                                                                         { I# y5_agKz ->
                                                                         $wgo3_sysc
                                                                           (+# w4_sys3 1#)
                                                                           (+#
                                                                              ww6_sys7 y5_agKz)
                                                                         };
                                                                       True ->
                                                                         case w2_ahgx
                                                                                (I#
                                                                                   w4_sys3)
                                                                         of _ [Occ=Dead]
                                                                         { I# y5_agKz ->
                                                                         +#
                                                                           ww6_sys7 y5_agKz
                                                                         }
                                                                     }; } in
                                                               case $wgo3_sysc 0# 0#
                                                               of ww6_sysb { __DEFAULT ->
                                                               $j_szhZ
                                                                 (int2Double# ww6_sysb)
                                                               };
                                                             True -> $j_szhZ 0.0##
                                                           }
                                                           }
                                                       }
                                                   };
                                                 True -> 1.0##
                                               } } in
                                         case tagToEnum# @ Bool (># 0# y2_ahgy)
                                         of _ [Occ=Dead] {
                                           False ->
                                             let {
                                               w1_ahgx :: Int -> Int

                                               w1_ahgx =
                                                 \ (i丙10_abXj :: Int) ->
                                                   Language.Hakaru.Runtime.Prelude.case_1
                                                     @ Int
                                                     @ Bool
                                                     (case docUpdate5_abXf
                                                      of _ [Occ=Dead] { I# x_agIE ->
                                                      case i丙10_abXj
                                                      of _ [Occ=Dead] { I# x1_ahkY ->
                                                      case tagToEnum#
                                                             @ Bool (>=# x1_ahkY 0#)
                                                      of _ [Occ=Dead] {
                                                        False ->
                                                          case lvl108_sB8e
                                                          of _ [Occ=Dead] { I# n#_ahl6 ->
                                                          case lvl49_rIrg n#_ahl6 x1_ahkY
                                                          of wild7_00 {
                                                          }
                                                          };
                                                        True ->
                                                          case lvl108_sB8e
                                                          of _ [Occ=Dead] { I# y3_ahlc ->
                                                          case tagToEnum#
                                                                 @ Bool
                                                                 (<# x1_ahkY y3_ahlc)
                                                          of _ [Occ=Dead] {
                                                            False ->
                                                              case lvl49_rIrg y3_ahlc x1_ahkY
                                                              of wild8_00 {
                                                              };
                                                            True ->
                                                              case doc4_abXe `cast` ...
                                                              of _ [Occ=Dead]
                                                              { Data.Vector.Primitive.Vector dt_ai1q
                                                                                             dt1_ai1r
                                                                                             dt2_ai1s ->
                                                              case indexIntArray#
                                                                     dt2_ai1s
                                                                     (+# dt_ai1q x1_ahkY)
                                                              of wild9_ai1B { __DEFAULT ->
                                                              tagToEnum#
                                                                @ Bool
                                                                (==# x_agIE wild9_ai1B)
                                                              }
                                                              }
                                                          }
                                                          }
                                                      }
                                                      }
                                                      })
                                                     (:
                                                        @ (Branch Bool Int)
                                                        (let {
                                                           b1_Xkvl :: Int

                                                           b1_Xkvl =
                                                             Language.Hakaru.Runtime.Prelude.case_1
                                                               @ Int
                                                               @ Bool
                                                               (case lvl112_shSc of _ [Occ=Dead] {
                                                                  False -> False;
                                                                  True ->
                                                                    case i丙10_abXj
                                                                    of _ [Occ=Dead]
                                                                    { I# x_ahkY ->
                                                                    case tagToEnum#
                                                                           @ Bool
                                                                           (>=# x_ahkY 0#)
                                                                    of _ [Occ=Dead] {
                                                                      False ->
                                                                        case lvl49_rIrg
                                                                               ww5_agDQ x_ahkY
                                                                        of wild6_00 {
                                                                        };
                                                                      True ->
                                                                        case tagToEnum#
                                                                               @ Bool
                                                                               (<#
                                                                                  x_ahkY ww5_agDQ)
                                                                        of _ [Occ=Dead] {
                                                                          False ->
                                                                            case lvl49_rIrg
                                                                                   ww5_agDQ x_ahkY
                                                                            of wild7_00 {
                                                                            };
                                                                          True ->
                                                                            case w3_abXd `cast` ...
                                                                            of _ [Occ=Dead]
                                                                            { Data.Vector.Primitive.Vector dt_ai1q
                                                                                                           dt1_ai1r
                                                                                                           dt2_ai1s ->
                                                                            case indexIntArray#
                                                                                   dt2_ai1s
                                                                                   (+#
                                                                                      dt_ai1q
                                                                                      x_ahkY)
                                                                            of wild8_ai1B
                                                                            { __DEFAULT ->
                                                                            tagToEnum#
                                                                              @ Bool
                                                                              (==#
                                                                                 sc1_sEol
                                                                                 wild8_ai1B)
                                                                            }
                                                                            }
                                                                        }
                                                                    }
                                                                    }
                                                                })
                                                               lvl81_rIrW } in
                                                         let {
                                                           lvl113_sBap :: Maybe Int

                                                           lvl113_sBap =
                                                             Just @ Int b1_Xkvl } in
                                                         (\ (p_Xkvo :: Bool) ->
                                                            case p_Xkvo of _ [Occ=Dead] {
                                                              False -> Nothing @ Int;
                                                              True -> lvl113_sBap
                                                            })
                                                         `cast` ...)
                                                        lvl78_rIrS) } in
                                             letrec {
                                               $wgo3_sysK [InlPrag=[0], Occ=LoopBreaker]
                                                 :: Int# -> Int# -> Int#

                                               $wgo3_sysK =
                                                 \ (w2_sysB :: Int#)
                                                   (ww6_sysF [OS=OneShot] :: Int#) ->
                                                   case tagToEnum#
                                                          @ Bool (==# w2_sysB y2_ahgy)
                                                   of _ [Occ=Dead] {
                                                     False ->
                                                       case w1_ahgx (I# w2_sysB)
                                                       of _ [Occ=Dead] { I# y3_agKz ->
                                                       $wgo3_sysK
                                                         (+# w2_sysB 1#)
                                                         (+# ww6_sysF y3_agKz)
                                                       };
                                                     True ->
                                                       case w1_ahgx (I# w2_sysB)
                                                       of _ [Occ=Dead] { I# y3_agKz ->
                                                       +# ww6_sysF y3_agKz
                                                       }
                                                   }; } in
                                             case $wgo3_sysK 0# 0# of ww6_sysJ { __DEFAULT ->
                                             $w$j_sysz ww6_sysJ
                                             };
                                           True -> $w$j_sysz 0#
                                         }
                                         } } in
                                   letrec {
                                     $wgo3_sysZ [InlPrag=[0], Occ=LoopBreaker]
                                       :: Int# -> Double# -> Double#

                                     $wgo3_sysZ =
                                       \ (w1_sysQ :: Int#)
                                         (ww4_sysU [OS=OneShot] :: Double#) ->
                                         case tagToEnum#
                                                @ Bool (==# w1_sysQ y1_XhtC)
                                         of _ [Occ=Dead] {
                                           False ->
                                             case $s$ww2_sEon w1_sysQ of ww5_sysO { __DEFAULT ->
                                             $wgo3_sysZ
                                               (+# w1_sysQ 1#)
                                               (*## ww4_sysU ww5_sysO)
                                             };
                                           True ->
                                             case $s$ww2_sEon w1_sysQ of ww5_sysO { __DEFAULT ->
                                             *## ww4_sysU ww5_sysO
                                             }
                                         }; } in
                                   $wgo3_sysZ 0# 1.0##;
                                 True -> 1.0##
                               }
                               } } in
                         letrec {
                           $wgo3_syte [InlPrag=[0], Occ=LoopBreaker]
                             :: Int# -> Double# -> Double#

                           $wgo3_syte =
                             \ (w1_syt5 :: Int#)
                               (ww2_syt9 [OS=OneShot] :: Double#) ->
                               case tagToEnum# @ Bool (==# w1_syt5 y_ahjK)
                               of _ [Occ=Dead] {
                                 False ->
                                   case $s$ww1_sEnU w1_syt5 of ww3_syt3 { __DEFAULT ->
                                   $wgo3_syte
                                     (+# w1_syt5 1#) (*## ww2_syt9 ww3_syt3)
                                   };
                                 True ->
                                   case $s$ww1_sEnU w1_syt5 of ww3_syt3 { __DEFAULT ->
                                   *## ww2_syt9 ww3_syt3
                                   }
                               }; } in
                         case $wgo3_syte 0# 1.0## of ww2_sytd { __DEFAULT ->
                         D# ww2_sytd
                         };
                       True -> labelHP
                     })
                } } in
            (\ (w1_ak1n :: MWC.GenIO)
               (w2_ak1o [OS=OneShot] :: State# RealWorld) ->
               case w_ak1m `cast` ...
               of _ [Occ=Dead]
               { Data.Vector.Primitive.Vector ww1_ak1r ww2_ak1s ww3_ak1t ->
               case w1_ak1n `cast` ...
               of _ [Occ=Dead]
               { Data.Vector.Primitive.Mutable.MVector ww5_ak1x ww6_ak1y
                                                       ww7_ak1z ->
               Language.Hakaru.Runtime.Prelude.$wcategorical
                 ww1_ak1r ww2_ak1s ww3_ak1t ww5_ak1x ww7_ak1z w2_ak1o
               }
               })
            `cast` ...

-- RHS size: {terms: 2, types: 0, coercions: 0}
$trModule2 :: TrName
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 20}]
$trModule2 = TrNameS "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
$trModule1 :: TrName
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 20}]
$trModule1 = TrNameS "Main"#

-- RHS size: {terms: 3, types: 0, coercions: 0}
$trModule :: Module
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 30}]
$trModule = Module $trModule2 $trModule1

-- RHS size: {terms: 2, types: 0, coercions: 0}
$tc'ArrayType1 :: TrName
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 50 20}]
$tc'ArrayType1 = TrNameS "'ArrayType"#

-- RHS size: {terms: 5, types: 0, coercions: 0}
$tc'ArrayType :: TyCon
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 50}]
$tc'ArrayType =
  TyCon
    5995602583604282351##
    3363985000282575627##
    $trModule
    $tc'ArrayType1

-- RHS size: {terms: 2, types: 0, coercions: 0}
$tcArrayType1 :: TrName
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 50 20}]
$tcArrayType1 = TrNameS "ArrayType"#

-- RHS size: {terms: 5, types: 0, coercions: 0}
$tcArrayType :: TyCon
[GblId,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 50}]
$tcArrayType =
  TyCon
    76949701237834671##
    10835262698067980539##
    $trModule
    $tcArrayType1

-- RHS size: {terms: 21, types: 26, coercions: 0}
$wgibbsC_rIs1
  :: Addr#
     -> Addr#
     -> Addr#
     -> Addr#
     -> Addr#
     -> Int#
     -> State# RealWorld
     -> (# State# RealWorld, Ptr (ArrayType Double) #)
[GblId,
 Arity=7,

$wgibbsC_rIs1 =
  \ (ww_sytp :: Addr#)
    (ww1_sytt :: Addr#)
    (ww2_sytx :: Addr#)
    (ww3_sytB :: Addr#)
    (ww4_sytF :: Addr#)
    (ww5_sytJ :: Int#)
    (w_sytm [OS=OneShot] :: State# RealWorld) ->
    case {__pkg_ccall_GC main gibbsC_shim Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Int#
                                 -> State# RealWorld
                                 -> (# State# RealWorld, Addr# #)}_dfFk
           ww_sytp ww1_sytt ww2_sytx ww3_sytB ww4_sytF ww5_sytJ w_sytm
    of _ [Occ=Dead] { (# ds_dfFi [OS=OneShot], ds1_dfFh #) ->
    (# ds_dfFi, Ptr @ (ArrayType Double) ds1_dfFh #)
    }

-- RHS size: {terms: 33, types: 40, coercions: 0}
gibbsC1_rIs2
  :: Ptr (ArrayType Double)
     -> Ptr (ArrayType Double)
     -> Ptr (ArrayType Int)
     -> Ptr (ArrayType Int)
     -> Ptr (ArrayType Int)
     -> Int
     -> State# RealWorld
     -> (# State# RealWorld, Ptr (ArrayType Double) #)
[GblId,
 Arity=7,

gibbsC1_rIs2 =
  \ (w_sytg :: Ptr (ArrayType Double))
    (w1_syth :: Ptr (ArrayType Double))
    (w2_syti :: Ptr (ArrayType Int))
    (w3_sytj :: Ptr (ArrayType Int))
    (w4_sytk :: Ptr (ArrayType Int))
    (w5_sytl :: Int)
    (w6_sytm [OS=OneShot] :: State# RealWorld) ->
    case w_sytg of _ [Occ=Dead] { Ptr ww1_sytp ->
    case w1_syth of _ [Occ=Dead] { Ptr ww3_sytt ->
    case w2_syti of _ [Occ=Dead] { Ptr ww5_sytx ->
    case w3_sytj of _ [Occ=Dead] { Ptr ww7_sytB ->
    case w4_sytk of _ [Occ=Dead] { Ptr ww9_sytF ->
    case w5_sytl of _ [Occ=Dead] { I# ww11_sytJ ->
    $wgibbsC_rIs1
      ww1_sytp ww3_sytt ww5_sytx ww7_sytB ww9_sytF ww11_sytJ w6_sytm
    }
    }
    }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 27}
gibbsC
  :: ArrayStruct Double
     -> ArrayStruct Double
     -> ArrayStruct Int
     -> ArrayStruct Int
     -> ArrayStruct Int
     -> Int
     -> IO (ArrayStruct Double)
[GblId,
 Arity=7,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=6,unsat_ok=False,boring_ok=False)
         Tmpl= (\ (ds_XfGo [Occ=Once!] :: Ptr (ArrayType Double))
                  (ds1_XfGq [Occ=Once!] :: Ptr (ArrayType Double))
                  (ds2_XfGs [Occ=Once!] :: Ptr (ArrayType Int))
                  (ds3_XfGu [Occ=Once!] :: Ptr (ArrayType Int))
                  (ds4_XfGw [Occ=Once!] :: Ptr (ArrayType Int))
                  (ds5_XfGy [Occ=Once!] :: Int)
                  (eta_XuW [Occ=Once, OS=OneShot]
                     :: State# RealWorld) ->
                  case ds_XfGo of _ [Occ=Dead] { Ptr ds7_dfF6 [Occ=Once] ->
                  case ds1_XfGq of _ [Occ=Dead] { Ptr ds9_dfF8 [Occ=Once] ->
                  case ds2_XfGs of _ [Occ=Dead] { Ptr ds11_dfFa [Occ=Once] ->
                  case ds3_XfGu of _ [Occ=Dead] { Ptr ds13_dfFc [Occ=Once] ->
                  case ds4_XfGw of _ [Occ=Dead] { Ptr ds15_dfFe [Occ=Once] ->
                  case ds5_XfGy
                  of _ [Occ=Dead] { I# ds17_dfFg [Occ=Once] ->
                  case {__pkg_ccall_GC main gibbsC_shim Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Int#
                                 -> State# RealWorld
                                 -> (# State# RealWorld, Addr# #)}_dfFk
                         ds7_dfF6 ds9_dfF8 ds11_dfFa ds13_dfFc ds15_dfFe ds17_dfFg eta_XuW
                  of _ [Occ=Dead]
                  { (# ds18_dfFi [Occ=Once, OS=OneShot], ds19_dfFh [Occ=Once] #) ->
                  (# ds18_dfFi, Ptr @ (ArrayType Double) ds19_dfFh #)
                  }
                  }
                  }
                  }
                  }
                  }
                  })
               `cast` ...}]
gibbsC = gibbsC1_rIs2 `cast` ...

-- RHS size: {terms: 16, types: 6, coercions: 14}
$wdiff [InlPrag=[0]]
  :: time-1.6.0.1:Data.Time.Calendar.Days.Day
     -> DiffTime
     -> time-1.6.0.1:Data.Time.Calendar.Days.Day
     -> DiffTime
     -> [Char]
[GblId,
 Arity=4,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [0 0 0 0] 160 0}]
$wdiff =
  \ (ww_sytQ :: time-1.6.0.1:Data.Time.Calendar.Days.Day)
    (ww1_sytR :: DiffTime)
    (ww2_sytV :: time-1.6.0.1:Data.Time.Calendar.Days.Day)
    (ww3_sytW :: DiffTime) ->
    ++
      @ Char
      (Data.Fixed.showFixed
         @ Data.Fixed.E12
         (Data.Fixed.$fHasResolutionE12_$cresolution `cast` ...)
         True
         ((integer-gmp-1.0.0.1:Type.minusInteger
             ((Data.Time.Clock.POSIX.$wutcTimeToPOSIXSeconds ww2_sytV ww3_sytW)
              `cast` ...)
             ((Data.Time.Clock.POSIX.$wutcTimeToPOSIXSeconds ww_sytQ ww1_sytR)
              `cast` ...))
          `cast` ...))
      time-1.6.0.1:Data.Time.Clock.UTC.$fShowNominalDiffTime2

-- RHS size: {terms: 13, types: 8, coercions: 0}
diff [InlPrag=INLINE[0]] :: Time -> Time -> String
[GblId,
 Arity=2,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=2,unsat_ok=True,boring_ok=False)
         Tmpl= \ (w_sytM [Occ=Once!] :: Time)
                 (w1_sytN [Occ=Once!] :: Time) ->
                 case w_sytM
                 of _ [Occ=Dead]
                 { UTCTime ww1_sytQ [Occ=Once] ww2_sytR [Occ=Once] ->
                 case w1_sytN
                 of _ [Occ=Dead]
                 { UTCTime ww4_sytV [Occ=Once] ww5_sytW [Occ=Once] ->
                 $wdiff ww1_sytQ ww2_sytR ww4_sytV ww5_sytW
                 }
                 }}]
diff =
  \ (w_sytM :: Time) (w1_sytN :: Time) ->
    case w_sytM of _ [Occ=Dead] { UTCTime ww1_sytQ ww2_sytR ->
    case w1_sytN of _ [Occ=Dead] { UTCTime ww4_sytV ww5_sytW ->
    $wdiff ww1_sytQ ww2_sytR ww4_sytV ww5_sytW
    }
    }

-- RHS size: {terms: 10, types: 4, coercions: 0}
lvl86_rIs3 :: Char -> Bool

lvl86_rIs3 =
  \ (ds_dfDo [OS=ProbOneShot] :: Char) ->
    case ds_dfDo of _ [Occ=Dead] { C# x_agOr ->
    case x_agOr of _ [Occ=Dead] {
      __DEFAULT -> True;
      's'# -> False
    }
    }

-- RHS size: {terms: 3, types: 1, coercions: 0}
z_rIs4 :: [Char]

z_rIs4 =
  filter
    @ Char
    lvl86_rIs3
    time-1.6.0.1:Data.Time.Clock.UTC.$fShowNominalDiffTime2

-- RHS size: {terms: 19, types: 11, coercions: 0}
time_go [Occ=LoopBreaker] :: [Char] -> [Char]

time_go =
  \ (ds_agOP :: [Char]) ->
    case ds_agOP of _ [Occ=Dead] {
      [] -> z_rIs4;
      : y_agOU ys_agOV ->
        case y_agOU of wild1_agOp { C# x_agOr ->
        case x_agOr of _ [Occ=Dead] {
          __DEFAULT -> : @ Char wild1_agOp (time_go ys_agOV);
          's'# -> time_go ys_agOV
        }
        }
    }

-- RHS size: {terms: 45, types: 62, coercions: 16}
$wtime [InlPrag=[0]]
  :: forall a_adp4.
     IO a_adp4
     -> State# RealWorld
     -> (# State# RealWorld, a_adp4 #)
[GblId,
 Arity=2,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [0 0] 350 30}]
$wtime =
  \ (@ a_adp4)
    (w_syu1 :: IO a_adp4)
    (w1_syu2 [OS=OneShot] :: State# RealWorld) ->
    case time-1.6.0.1:Data.Time.Clock.CTimespec.getCTimespec1 w1_syu2
    of _ [Occ=Dead] { (# ipv_ajZL, ipv1_ajZM #) ->
    case (w_syu1 `cast` ...) ipv_ajZL
    of _ [Occ=Dead] { (# ipv2_XiKE, ipv3_XiKG #) ->
    case time-1.6.0.1:Data.Time.Clock.CTimespec.getCTimespec1 ipv2_XiKE
    of _ [Occ=Dead] { (# ipv4_Xkvp, ipv5_Xkvr #) ->
    case Handle.Text.hPutStr2
           Handle.FD.stdout
           (case Data.Time.Clock.POSIX.$wposixSecondsToUTCTime
                   (Data.Time.Clock.POSIX.getCurrentTime2 ipv5_Xkvr)
            of _ [Occ=Dead] { (# ww1_am1e, ww2_am1f #) ->
            case Data.Time.Clock.POSIX.$wposixSecondsToUTCTime
                   (Data.Time.Clock.POSIX.getCurrentTime2 ipv1_ajZM)
            of _ [Occ=Dead] { (# ww5_Xmdk, ww6_Xmdm #) ->
            time_go
              (Data.Fixed.showFixed
                 @ Data.Fixed.E12
                 (Data.Fixed.$fHasResolutionE12_$cresolution `cast` ...)
                 True
                 ((integer-gmp-1.0.0.1:Type.minusInteger
                     ((Data.Time.Clock.POSIX.$wutcTimeToPOSIXSeconds ww1_am1e ww2_am1f)
                      `cast` ...)
                     ((Data.Time.Clock.POSIX.$wutcTimeToPOSIXSeconds ww5_Xmdk ww6_Xmdm)
                      `cast` ...))
                  `cast` ...))
            }
            })
           True
           ipv4_Xkvp
    of _ [Occ=Dead] { (# ipv6_aigv, ipv7_aigw #) ->
    (# ipv6_aigv, ipv3_XiKG #)
    }
    }
    }
    }

-- RHS size: {terms: 7, types: 8, coercions: 0}
time1 [InlPrag=INLINE[0]]
  :: forall a_adp4.
     String
     -> IO a_adp4
     -> State# RealWorld
     -> (# State# RealWorld, a_adp4 #)
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=3,unsat_ok=True,boring_ok=True)
         Tmpl= \ (@ a_adp4)
                 _ [Occ=Dead]
                 (w1_syu1 [Occ=Once] :: IO a_adp4)
                 (w2_syu2 [Occ=Once, OS=OneShot]
                    :: State# RealWorld) ->
                 $wtime @ a_adp4 w1_syu1 w2_syu2}]
time1 =
  \ (@ a_adp4)
    _ [Occ=Dead]
    (w1_syu1 :: IO a_adp4)
    (w2_syu2 [OS=OneShot] :: State# RealWorld) ->
    $wtime @ a_adp4 w1_syu1 w2_syu2

-- RHS size: {terms: 1, types: 0, coercions: 11}
time :: forall a_aayj. String -> IO a_aayj -> IO a_aayj
[GblId,
 Arity=3,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= time1 `cast` ...}]
time = time1 `cast` ...

-- RHS size: {terms: 3, types: 0, coercions: 0}
lvl87_rIs5 :: [Int]

lvl87_rIs5 = eftInt 0# 5#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl88_rIs6 :: [Char]

lvl88_rIs6 = unpackCString# "C,%d,%d,%d,%d,"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl89_rIs7 :: [Char]

lvl89_rIs7 = unpackCString# "Haskell,%d,%d,%d,%d,"#

-- RHS size: {terms: 15, types: 17, coercions: 3}
$wgo2_rIs8
  :: [Data.Vector.Unboxed.Base.Vector Int]
     -> Int# -> Int#

$wgo2_rIs8 =
  \ (w_syu4 :: [Data.Vector.Unboxed.Base.Vector Int])
    (ww_syu8 :: Int#) ->
    case w_syu4 of _ [Occ=Dead] {
      [] -> ww_syu8;
      : y_agOU ys_agOV ->
        case y_agOU `cast` ...
        of _ [Occ=Dead]
        { Data.Vector.Primitive.Vector dt_ahZ1 dt1_ahZ2 dt2_ahZ3 ->
        $wgo2_rIs8 ys_agOV (+# ww_syu8 dt1_ahZ2)
        }
    }

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl90_rIs9 :: [Char]

lvl90_rIs9 =
  unpackCString#
    "Pattern match failure in do expression at NBHakaru.hs:107:5-15"#

-- RHS size: {terms: 7, types: 4, coercions: 0}
lvl91_rIsa :: Exception.IOException

lvl91_rIsa =
  Exception.IOError
    (Nothing @ Handle.Types.Handle)
    Exception.UserError
    ([] @ Char)
    lvl90_rIs9
    (Nothing @ CInt)
    (Nothing @ FilePath)

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl92_rIsb :: SomeException

lvl92_rIsb =
  Exception.$fExceptionIOException_$ctoException lvl91_rIsa

-- RHS size: {terms: 1,270, types: 1,426, coercions: 158}
main2
  :: Int
     -> Int
     -> Int
     -> Int
     -> State# RealWorld
     -> (# State# RealWorld, () #)

main2 =
  \ (numDocs_abMS :: Int)
    (k_abMT :: Int)
    (vocabSize_abMU :: Int)
    (trial_abMV :: Int)
    (eta_XvH [OS=OneShot] :: State# RealWorld) ->
    case System.Random.MWC.createSystemRandom2
           @ (MWC.Gen (Control.Monad.Primitive.PrimState IO))
           ((returnIO1 @ MWC.GenIO) `cast` ...)
           eta_XvH
    of _ [Occ=Dead] { (# ipv_aif4, ipv1_aif5 #) ->
    let {
      doc_sm5C [Dmd=<L,U(U,U,U)>] :: Data.Vector.Unboxed.Base.Vector Int

      doc_sm5C =
        case runRW#
               @ 'PtrRepLifted
               @ (Data.Vector.Unboxed.Base.Vector Int)
               (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                  let {
                    eta1_ahvg [Dmd=<S,U>] :: [Data.Vector.Unboxed.Base.Vector Int]

                    eta1_ahvg =
                      map
                        @ Int
                        @ (Data.Vector.Unboxed.Base.Vector Int)
                        ($sreplicate numDocs_abMS)
                        lvl87_rIs5 } in
                  case $wgo2_rIs8 eta1_ahvg 0# of ww_syuc { __DEFAULT ->
                  case tagToEnum# @ Bool (<# ww_syuc 0#)
                  of _ [Occ=Dead] {
                    False ->
                      case divInt# 9223372036854775807# 8#
                      of ww4_agN3 { __DEFAULT ->
                      case tagToEnum# @ Bool (># ww_syuc ww4_agN3)
                      of _ [Occ=Dead] {
                        False ->
                          case newByteArray#
                                 @ (Control.Monad.Primitive.PrimState
                                      (ST RealWorld))
                                 (*# ww_syuc 8#)
                                 (s1_ah7U `cast` ...)
                          of _ [Occ=Dead] { (# ipv2_ajsv, ipv3_ajsw #) ->
                          letrec {
                            $s$wfoldlM'_loop_sEkF [Occ=LoopBreaker]
                              :: State# RealWorld
                                 -> [Data.Vector.Unboxed.Base.Vector Int]
                                 -> Int#
                                 -> (# State# RealWorld, Int #)

                            $s$wfoldlM'_loop_sEkF =
                              \ (sc_sEkE [OS=OneShot] :: State# RealWorld)
                                (sc1_sEkC :: [Data.Vector.Unboxed.Base.Vector Int])
                                (sc2_sEkB :: Int#) ->
                                case sc1_sEkC of _ [Occ=Dead] {
                                  [] -> (# sc_sEkE, I# sc2_sEkB #);
                                  : v1_ahw2 vs_ahw3 ->
                                    case v1_ahw2 `cast` ...
                                    of _ [Occ=Dead]
                                    { Data.Vector.Primitive.Vector dt_ahZ1 dt1_ahZ2 dt2_ahZ3 ->
                                    case copyByteArray#
                                           @ (Control.Monad.Primitive.PrimState
                                                (ST RealWorld))
                                           dt2_ahZ3
                                           (*# dt_ahZ1 8#)
                                           ipv3_ajsw
                                           (*# sc2_sEkB 8#)
                                           (*# dt1_ahZ2 8#)
                                           (sc_sEkE `cast` ...)
                                    of s'#_ahZT [OS=OneShot] { __DEFAULT ->
                                    $s$wfoldlM'_loop_sEkF
                                      (s'#_ahZT `cast` ...) vs_ahw3 (+# sc2_sEkB dt1_ahZ2)
                                    }
                                    }
                                }; } in
                          case $s$wfoldlM'_loop_sEkF (ipv2_ajsv `cast` ...) eta1_ahvg 0#
                          of _ [Occ=Dead] { (# ipv6_ah4v, ipv7_ah4w #) ->
                          case ipv7_ah4w of _ [Occ=Dead] { I# dt4_ajtM ->
                          case unsafeFreezeByteArray#
                                 @ (Control.Monad.Primitive.PrimState
                                      (ST RealWorld))
                                 ipv3_ajsw
                                 (ipv6_ah4v `cast` ...)
                          of _ [Occ=Dead] { (# ipv4_ajqY, ipv5_ajqZ #) ->
                          (# ipv4_ajqY `cast` ...,
                             (Data.Vector.Primitive.Vector @ Int 0# dt4_ajtM ipv5_ajqZ)
                             `cast` ... #)
                          }
                          }
                          }
                          };
                        True -> case lvl16_rIqF ww_syuc of wild2_00 { }
                      }
                      };
                    True -> case lvl22_rIqL ww_syuc of wild1_00 { }
                  }
                  })
        of _ [Occ=Dead] { (# ipv2_ah84, ipv3_ah85 #) ->
        ipv3_ah85
        } } in
    case ((((generateDataset
               k_abMT vocabSize_abMU numDocs_abMS doc_sm5C)
            `cast` ...)
             ipv1_aif5)
          `cast` ...)
           ipv_aif4
    of _ [Occ=Dead] { (# ipv2_XiL0, ipv3_XiL2 #) ->
    case ipv3_XiL2 of _ [Occ=Dead] {
      Nothing ->
        raiseIO#
          @ SomeException @ () lvl92_rIsb ipv2_XiL0;
      Just ds_dfDT ->
        case ds_dfDT of _ [Occ=Dead] { (z1_abMY, w_abMZ) ->
        let {
          v_XcMS [Dmd=<L,U(U,U,U)>] :: SV.Vector Int

          v_XcMS =
            case doc_sm5C `cast` ...
            of _ [Occ=Dead]
            { Data.Vector.Primitive.Vector ipv4_spO3 ipv5_spO4 ipv6_spO5 ->
            case runRW#
                   @ 'PtrRepLifted
                   @ (SV.Vector Int)
                   (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                      case tagToEnum# @ Bool (<# ipv5_spO4 0#)
                      of _ [Occ=Dead] {
                        False ->
                          case tagToEnum#
                                 @ Bool (># ipv5_spO4 1152921504606846975#)
                          of _ [Occ=Dead] {
                            False ->
                              let {
                                x_ajOR [Dmd=<S,U>] :: Int#

                                x_ajOR = *# ipv5_spO4 8# } in
                              case tagToEnum# @ Bool (<# x_ajOR 0#)
                              of _ [Occ=Dead] {
                                False ->
                                  case newPinnedByteArray#
                                         @ RealWorld x_ajOR s1_ah7U
                                  of _ [Occ=Dead] { (# ipv7_ajOY, ipv8_ajOZ #) ->
                                  let {
                                    ipv9_sqO0 :: Addr#

                                    ipv9_sqO0 =
                                      byteArrayContents# (ipv8_ajOZ `cast` ...) } in
                                  let {
                                    ipv10_sqO1 :: ForeignPtrContents

                                    ipv10_sqO1 = PlainPtr ipv8_ajOZ } in
                                  letrec {
                                    $s$wfoldlM'_loop_sEkN [Occ=LoopBreaker]
                                      :: State# RealWorld
                                         -> Int#
                                         -> Int#
                                         -> (# State# RealWorld, Int #)

                                    $s$wfoldlM'_loop_sEkN =
                                      \ (sc_sEkM [OS=OneShot] :: State# RealWorld)
                                        (sc1_sEkL :: Int#)
                                        (sc2_sEkK :: Int#) ->
                                        case tagToEnum#
                                               @ Bool (>=# sc1_sEkL ipv5_spO4)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case indexIntArray#
                                                   ipv6_spO5 (+# ipv4_spO3 sc1_sEkL)
                                            of wild6_ai1B { __DEFAULT ->
                                            case writeIntOffAddr#
                                                   @ RealWorld
                                                   (plusAddr#
                                                      ipv9_sqO0 (*# sc2_sEkK 8#))
                                                   0#
                                                   wild6_ai1B
                                                   sc_sEkM
                                            of s2_atiP [OS=OneShot] { __DEFAULT ->
                                            case touch#
                                                   @ 'PtrRepLifted
                                                   @ ForeignPtrContents
                                                   ipv10_sqO1
                                                   s2_atiP
                                            of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                                            $s$wfoldlM'_loop_sEkN
                                              s'_ahW8
                                              (+# sc1_sEkL 1#)
                                              (+# sc2_sEkK 1#)
                                            }
                                            }
                                            };
                                          True -> (# sc_sEkM, I# sc2_sEkK #)
                                        }; } in
                                  case $s$wfoldlM'_loop_sEkN ipv7_ajOY 0# 0#
                                  of _ [Occ=Dead] { (# ipv11_ah4v, ipv12_ah4w #) ->
                                  case ipv12_ah4w of _ [Occ=Dead] { I# dt4_ajNI ->
                                  (# ipv11_ah4v,
                                     Data.Vector.Storable.Vector
                                       @ Int dt4_ajNI ipv9_sqO0 ipv10_sqO1 #)
                                  }
                                  }
                                  };
                                True ->
                                  case mallocPlainForeignPtrBytes2 of wild5_00 { }
                              };
                            True -> case lvl69_rIrH ipv5_spO4 of wild4_00 { }
                          };
                        True -> case lvl70_rIrI ipv5_spO4 of wild3_00 { }
                      })
            of _ [Occ=Dead] { (# ipv7_ah84, ipv8_ah85 #) ->
            ipv8_ah85
            }
            } } in
        let {
          v1_XcN9 [Dmd=<L,U(U,U,U)>] :: SV.Vector Int

          v1_XcN9 =
            case z1_abMY `cast` ...
            of _ [Occ=Dead]
            { Data.Vector.Primitive.Vector ipv4_spPE ipv5_spPF ipv6_spPG ->
            case runRW#
                   @ 'PtrRepLifted
                   @ (SV.Vector Int)
                   (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                      case tagToEnum# @ Bool (<# ipv5_spPF 0#)
                      of _ [Occ=Dead] {
                        False ->
                          case tagToEnum#
                                 @ Bool (># ipv5_spPF 1152921504606846975#)
                          of _ [Occ=Dead] {
                            False ->
                              let {
                                x_ajOR [Dmd=<S,U>] :: Int#

                                x_ajOR = *# ipv5_spPF 8# } in
                              case tagToEnum# @ Bool (<# x_ajOR 0#)
                              of _ [Occ=Dead] {
                                False ->
                                  case newPinnedByteArray#
                                         @ RealWorld x_ajOR s1_ah7U
                                  of _ [Occ=Dead] { (# ipv7_ajOY, ipv8_ajOZ #) ->
                                  let {
                                    ipv9_sqOI :: Addr#

                                    ipv9_sqOI =
                                      byteArrayContents# (ipv8_ajOZ `cast` ...) } in
                                  let {
                                    ipv10_sqOJ :: ForeignPtrContents

                                    ipv10_sqOJ = PlainPtr ipv8_ajOZ } in
                                  letrec {
                                    $s$wfoldlM'_loop_sEkU [Occ=LoopBreaker]
                                      :: State# RealWorld
                                         -> Int#
                                         -> Int#
                                         -> (# State# RealWorld, Int #)

                                    $s$wfoldlM'_loop_sEkU =
                                      \ (sc_sEkT [OS=OneShot] :: State# RealWorld)
                                        (sc1_sEkS :: Int#)
                                        (sc2_sEkR :: Int#) ->
                                        case tagToEnum#
                                               @ Bool (>=# sc1_sEkS ipv5_spPF)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case indexIntArray#
                                                   ipv6_spPG (+# ipv4_spPE sc1_sEkS)
                                            of wild6_ai1B { __DEFAULT ->
                                            case writeIntOffAddr#
                                                   @ RealWorld
                                                   (plusAddr#
                                                      ipv9_sqOI (*# sc2_sEkR 8#))
                                                   0#
                                                   wild6_ai1B
                                                   sc_sEkT
                                            of s2_atiP [OS=OneShot] { __DEFAULT ->
                                            case touch#
                                                   @ 'PtrRepLifted
                                                   @ ForeignPtrContents
                                                   ipv10_sqOJ
                                                   s2_atiP
                                            of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                                            $s$wfoldlM'_loop_sEkU
                                              s'_ahW8
                                              (+# sc1_sEkS 1#)
                                              (+# sc2_sEkR 1#)
                                            }
                                            }
                                            };
                                          True -> (# sc_sEkT, I# sc2_sEkR #)
                                        }; } in
                                  case $s$wfoldlM'_loop_sEkU ipv7_ajOY 0# 0#
                                  of _ [Occ=Dead] { (# ipv11_ah4v, ipv12_ah4w #) ->
                                  case ipv12_ah4w of _ [Occ=Dead] { I# dt4_ajNI ->
                                  (# ipv11_ah4v,
                                     Data.Vector.Storable.Vector
                                       @ Int dt4_ajNI ipv9_sqOI ipv10_sqOJ #)
                                  }
                                  }
                                  };
                                True ->
                                  case mallocPlainForeignPtrBytes2 of wild5_00 { }
                              };
                            True -> case lvl69_rIrH ipv5_spPF of wild4_00 { }
                          };
                        True -> case lvl70_rIrI ipv5_spPF of wild3_00 { }
                      })
            of _ [Occ=Dead] { (# ipv7_ah84, ipv8_ah85 #) ->
            ipv8_ah85
            }
            } } in
        let {
          v2_XcNf [Dmd=<L,U(U,U,U)>] :: SV.Vector Int

          v2_XcNf =
            case w_abMZ `cast` ...
            of _ [Occ=Dead]
            { Data.Vector.Primitive.Vector ipv4_spRb ipv5_spRc ipv6_spRd ->
            case runRW#
                   @ 'PtrRepLifted
                   @ (SV.Vector Int)
                   (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                      case tagToEnum# @ Bool (<# ipv5_spRc 0#)
                      of _ [Occ=Dead] {
                        False ->
                          case tagToEnum#
                                 @ Bool (># ipv5_spRc 1152921504606846975#)
                          of _ [Occ=Dead] {
                            False ->
                              let {
                                x_ajOR [Dmd=<S,U>] :: Int#

                                x_ajOR = *# ipv5_spRc 8# } in
                              case tagToEnum# @ Bool (<# x_ajOR 0#)
                              of _ [Occ=Dead] {
                                False ->
                                  case newPinnedByteArray#
                                         @ RealWorld x_ajOR s1_ah7U
                                  of _ [Occ=Dead] { (# ipv7_ajOY, ipv8_ajOZ #) ->
                                  let {
                                    ipv9_sqPm :: Addr#

                                    ipv9_sqPm =
                                      byteArrayContents# (ipv8_ajOZ `cast` ...) } in
                                  let {
                                    ipv10_sqPn :: ForeignPtrContents

                                    ipv10_sqPn = PlainPtr ipv8_ajOZ } in
                                  letrec {
                                    $s$wfoldlM'_loop_sEl1 [Occ=LoopBreaker]
                                      :: State# RealWorld
                                         -> Int#
                                         -> Int#
                                         -> (# State# RealWorld, Int #)

                                    $s$wfoldlM'_loop_sEl1 =
                                      \ (sc_sEl0 [OS=OneShot] :: State# RealWorld)
                                        (sc1_sEkZ :: Int#)
                                        (sc2_sEkY :: Int#) ->
                                        case tagToEnum#
                                               @ Bool (>=# sc1_sEkZ ipv5_spRc)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case indexIntArray#
                                                   ipv6_spRd (+# ipv4_spRb sc1_sEkZ)
                                            of wild6_ai1B { __DEFAULT ->
                                            case writeIntOffAddr#
                                                   @ RealWorld
                                                   (plusAddr#
                                                      ipv9_sqPm (*# sc2_sEkY 8#))
                                                   0#
                                                   wild6_ai1B
                                                   sc_sEl0
                                            of s2_atiP [OS=OneShot] { __DEFAULT ->
                                            case touch#
                                                   @ 'PtrRepLifted
                                                   @ ForeignPtrContents
                                                   ipv10_sqPn
                                                   s2_atiP
                                            of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                                            $s$wfoldlM'_loop_sEl1
                                              s'_ahW8
                                              (+# sc1_sEkZ 1#)
                                              (+# sc2_sEkY 1#)
                                            }
                                            }
                                            };
                                          True -> (# sc_sEl0, I# sc2_sEkY #)
                                        }; } in
                                  case $s$wfoldlM'_loop_sEl1 ipv7_ajOY 0# 0#
                                  of _ [Occ=Dead] { (# ipv11_ah4v, ipv12_ah4w #) ->
                                  case ipv12_ah4w of _ [Occ=Dead] { I# dt4_ajNI ->
                                  (# ipv11_ah4v,
                                     Data.Vector.Storable.Vector
                                       @ Int dt4_ajNI ipv9_sqPm ipv10_sqPn #)
                                  }
                                  }
                                  };
                                True ->
                                  case mallocPlainForeignPtrBytes2 of wild5_00 { }
                              };
                            True -> case lvl69_rIrH ipv5_spRc of wild4_00 { }
                          };
                        True -> case lvl70_rIrI ipv5_spRc of wild3_00 { }
                      })
            of _ [Occ=Dead] { (# ipv7_ah84, ipv8_ah85 #) ->
            ipv8_ah85
            }
            } } in
        case $wtime
               @ (ArrayStruct Double)
               ((\ (s_aigs [OS=OneShot] :: State# RealWorld) ->
                   case Text.Printf.$fPrintfTypeIO1
                          @ ()
                          ($sprintf3 `cast` ...)
                          lvl88_rIs6
                          (:
                             @ (ModifierParser, FieldFormatter)
                             (Text.Printf.$wparseIntFormat @ Int,
                              Text.Printf.$fPrintfArgInt_$sformatInt trial_abMV)
                             (:
                                @ (ModifierParser, FieldFormatter)
                                (Text.Printf.$wparseIntFormat @ Int,
                                 Text.Printf.$fPrintfArgInt_$sformatInt vocabSize_abMU)
                                (:
                                   @ (ModifierParser, FieldFormatter)
                                   (Text.Printf.$wparseIntFormat @ Int,
                                    Text.Printf.$fPrintfArgInt_$sformatInt k_abMT)
                                   (:
                                      @ (ModifierParser, FieldFormatter)
                                      (Text.Printf.$wparseIntFormat @ Int,
                                       Text.Printf.$fPrintfArgInt_$sformatInt numDocs_abMS)
                                      ([] @ Text.Printf.UPrintf)))))
                          s_aigs
                   of _ [Occ=Dead] { (# ipv4_aigv, ipv5_aigw #) ->
                   case vocabSize_abMU of _ [Occ=Dead] { I# ww1_sypf ->
                   case $wvocabPrior ww1_sypf ipv1_aif5 ipv4_aigv
                   of _ [Occ=Dead] { (# ipv6_XiLp, ipv7_XiLr #) ->
                   case k_abMT of _ [Occ=Dead] { I# ww3_syo5 ->
                   case $wlabelPrior ww3_syo5 ipv1_aif5 ipv6_XiLp
                   of _ [Occ=Dead] { (# ipv8_XiLo, ipv9_XiLq #) ->
                   case $fStorableArrayType7
                   of _ [Occ=Dead] { I# size_ak3d ->
                   case newAlignedPinnedByteArray#
                          @ RealWorld size_ak3d 8# ipv8_XiLo
                   of _ [Occ=Dead] { (# ipv10_ak3l, ipv11_ak3m #) ->
                   case unsafeFreezeByteArray#
                          @ RealWorld ipv11_ak3m ipv10_ak3l
                   of _ [Occ=Dead] { (# ipv12_ak3r, ipv13_ak3s #) ->
                   case ipv7_XiLr
                   of _ [Occ=Dead]
                   { Data.Vector.Vector ipv14_spUm ipv15_spUn ipv16_spUo ->
                   case runRW#
                          @ 'PtrRepLifted
                          @ (SV.Vector Double)
                          (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                             case tagToEnum# @ Bool (<# ipv15_spUn 0#)
                             of _ [Occ=Dead] {
                               False ->
                                 case tagToEnum#
                                        @ Bool (># ipv15_spUn 1152921504606846975#)
                                 of _ [Occ=Dead] {
                                   False ->
                                     let {
                                       x_ajOR [Dmd=<S,U>] :: Int#

                                       x_ajOR = *# ipv15_spUn 8# } in
                                     case tagToEnum# @ Bool (<# x_ajOR 0#)
                                     of _ [Occ=Dead] {
                                       False ->
                                         case newPinnedByteArray#
                                                @ RealWorld x_ajOR s1_ah7U
                                         of _ [Occ=Dead] { (# ipv17_ajOY, ipv18_ajOZ #) ->
                                         let {
                                           ipv19_sqQH :: Addr#

                                           ipv19_sqQH =
                                             byteArrayContents#
                                               (ipv18_ajOZ `cast` ...) } in
                                         let {
                                           ipv20_sqQI :: ForeignPtrContents

                                           ipv20_sqQI = PlainPtr ipv18_ajOZ } in
                                         letrec {
                                           $s$wfoldlM'_loop_sEl8 [Occ=LoopBreaker]
                                             :: State# RealWorld
                                                -> Int#
                                                -> Int#
                                                -> (# State# RealWorld, Int #)

                                           $s$wfoldlM'_loop_sEl8 =
                                             \ (sc_sEl7 [OS=OneShot]
                                                  :: State# RealWorld)
                                               (sc1_sEl6 :: Int#)
                                               (sc2_sEl5 :: Int#) ->
                                               case tagToEnum#
                                                      @ Bool (>=# sc1_sEl6 ipv15_spUn)
                                               of _ [Occ=Dead] {
                                                 False ->
                                                   case indexArray#
                                                          @ Double
                                                          ipv16_spUo
                                                          (+# ipv14_spUm sc1_sEl6)
                                                   of _ [Occ=Dead] { (# ipv21_ai43 #) ->
                                                   case ipv21_ai43
                                                   of _ [Occ=Dead] { D# x1_at2v ->
                                                   case writeDoubleOffAddr#
                                                          @ RealWorld
                                                          (plusAddr#
                                                             ipv19_sqQH (*# sc2_sEl5 8#))
                                                          0#
                                                          x1_at2v
                                                          sc_sEl7
                                                   of s2_at2x [OS=OneShot] { __DEFAULT ->
                                                   case touch#
                                                          @ 'PtrRepLifted
                                                          @ ForeignPtrContents
                                                          ipv20_sqQI
                                                          s2_at2x
                                                   of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                                                   $s$wfoldlM'_loop_sEl8
                                                     s'_ahW8
                                                     (+# sc1_sEl6 1#)
                                                     (+# sc2_sEl5 1#)
                                                   }
                                                   }
                                                   }
                                                   };
                                                 True -> (# sc_sEl7, I# sc2_sEl5 #)
                                               }; } in
                                         case $s$wfoldlM'_loop_sEl8 ipv17_ajOY 0# 0#
                                         of _ [Occ=Dead] { (# ipv21_ah4v, ipv22_ah4w #) ->
                                         case ipv22_ah4w of _ [Occ=Dead] { I# dt4_ajNI ->
                                         (# ipv21_ah4v,
                                            Data.Vector.Storable.Vector
                                              @ Double dt4_ajNI ipv19_sqQH ipv20_sqQI #)
                                         }
                                         }
                                         };
                                       True ->
                                         case mallocPlainForeignPtrBytes2
                                         of wild6_00 {
                                         }
                                     };
                                   True -> case lvl52_rIrm ipv15_spUn of wild5_00 { }
                                 };
                               True -> case lvl53_rIrn ipv15_spUn of wild4_00 { }
                             })
                   of _ [Occ=Dead] { (# ipv17_ah84, ipv18_ah85 #) ->
                   case ipv18_ah85
                   of ww4_syve
                   { Data.Vector.Storable.Vector ww5_syvf ww6_syvg ww7_syvh ->
                   case $slength2 ww4_syve
                   of _ [Occ=Dead] { I# x_ajy0 ->
                   let {
                     ptr_ak3q [Dmd=<S,U>] :: Addr#

                     ptr_ak3q = byteArrayContents# ipv13_ak3s } in
                   case writeIntOffAddr#
                          @ RealWorld ptr_ak3q 0# x_ajy0 ipv12_ak3r
                   of s2_ajy2 [OS=OneShot] { __DEFAULT ->
                   case $fStorableArrayType2
                   of _ [Occ=Dead] { I# d_ajyi ->
                   case writeAddrOffAddr#
                          @ RealWorld
                          (plusAddr# ptr_ak3q d_ajyi)
                          0#
                          ww6_syvg
                          s2_ajy2
                   of s1_ajyo [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepLifted
                          @ ForeignPtrContents
                          ww7_syvh
                          s1_ajyo
                   of s'_ahW8 [OS=OneShot] { __DEFAULT ->
                   case newAlignedPinnedByteArray#
                          @ RealWorld size_ak3d 8# s'_ahW8
                   of _ [Occ=Dead] { (# ipv19_XkAk, ipv20_XkAm #) ->
                   case unsafeFreezeByteArray#
                          @ RealWorld ipv20_XkAm ipv19_XkAk
                   of _ [Occ=Dead] { (# ipv21_XkAu, ipv22_XkAw #) ->
                   case ipv9_XiLq
                   of _ [Occ=Dead]
                   { Data.Vector.Vector ipv23_spSN ipv24_spSO ipv25_spSP ->
                   case runRW#
                          @ 'PtrRepLifted
                          @ (SV.Vector Double)
                          (\ (s3_Xhjo [OS=OneShot] :: State# RealWorld) ->
                             case tagToEnum# @ Bool (<# ipv24_spSO 0#)
                             of _ [Occ=Dead] {
                               False ->
                                 case tagToEnum#
                                        @ Bool (># ipv24_spSO 1152921504606846975#)
                                 of _ [Occ=Dead] {
                                   False ->
                                     let {
                                       x1_ajOR [Dmd=<S,U>] :: Int#

                                       x1_ajOR = *# ipv24_spSO 8# } in
                                     case tagToEnum# @ Bool (<# x1_ajOR 0#)
                                     of _ [Occ=Dead] {
                                       False ->
                                         case newPinnedByteArray#
                                                @ RealWorld x1_ajOR s3_Xhjo
                                         of _ [Occ=Dead] { (# ipv26_ajOY, ipv27_ajOZ #) ->
                                         let {
                                           ipv28_sqQ3 :: Addr#

                                           ipv28_sqQ3 =
                                             byteArrayContents#
                                               (ipv27_ajOZ `cast` ...) } in
                                         let {
                                           ipv29_sqQ4 :: ForeignPtrContents

                                           ipv29_sqQ4 = PlainPtr ipv27_ajOZ } in
                                         letrec {
                                           $s$wfoldlM'_loop_sElf [Occ=LoopBreaker]
                                             :: State# RealWorld
                                                -> Int#
                                                -> Int#
                                                -> (# State# RealWorld, Int #)

                                           $s$wfoldlM'_loop_sElf =
                                             \ (sc_sEle [OS=OneShot]
                                                  :: State# RealWorld)
                                               (sc1_sEld :: Int#)
                                               (sc2_sElc :: Int#) ->
                                               case tagToEnum#
                                                      @ Bool (>=# sc1_sEld ipv24_spSO)
                                               of _ [Occ=Dead] {
                                                 False ->
                                                   case indexArray#
                                                          @ Double
                                                          ipv25_spSP
                                                          (+# ipv23_spSN sc1_sEld)
                                                   of _ [Occ=Dead] { (# ipv30_ai43 #) ->
                                                   case ipv30_ai43
                                                   of _ [Occ=Dead] { D# x2_at2v ->
                                                   case writeDoubleOffAddr#
                                                          @ RealWorld
                                                          (plusAddr#
                                                             ipv28_sqQ3 (*# sc2_sElc 8#))
                                                          0#
                                                          x2_at2v
                                                          sc_sEle
                                                   of s4_at2x [OS=OneShot] { __DEFAULT ->
                                                   case touch#
                                                          @ 'PtrRepLifted
                                                          @ ForeignPtrContents
                                                          ipv29_sqQ4
                                                          s4_at2x
                                                   of s'1_Xi7A [OS=OneShot] { __DEFAULT ->
                                                   $s$wfoldlM'_loop_sElf
                                                     s'1_Xi7A
                                                     (+# sc1_sEld 1#)
                                                     (+# sc2_sElc 1#)
                                                   }
                                                   }
                                                   }
                                                   };
                                                 True -> (# sc_sEle, I# sc2_sElc #)
                                               }; } in
                                         case $s$wfoldlM'_loop_sElf ipv26_ajOY 0# 0#
                                         of _ [Occ=Dead] { (# ipv30_ah4v, ipv31_ah4w #) ->
                                         case ipv31_ah4w of _ [Occ=Dead] { I# dt4_ajNI ->
                                         (# ipv30_ah4v,
                                            Data.Vector.Storable.Vector
                                              @ Double dt4_ajNI ipv28_sqQ3 ipv29_sqQ4 #)
                                         }
                                         }
                                         };
                                       True ->
                                         case mallocPlainForeignPtrBytes2
                                         of wild8_00 {
                                         }
                                     };
                                   True -> case lvl52_rIrm ipv24_spSO of wild7_00 { }
                                 };
                               True -> case lvl53_rIrn ipv24_spSO of wild6_00 { }
                             })
                   of _ [Occ=Dead] { (# ipv26_XhjA, ipv27_XhjC #) ->
                   case ipv27_XhjC
                   of ww8_syvx
                   { Data.Vector.Storable.Vector ww9_syvy ww10_syvz ww11_syvA ->
                   case $slength2 ww8_syvx
                   of _ [Occ=Dead] { I# x1_Xk59 ->
                   let {
                     ptr1_Xl7C [Dmd=<S,U>] :: Addr#

                     ptr1_Xl7C = byteArrayContents# ipv22_XkAw } in
                   case writeIntOffAddr#
                          @ RealWorld ptr1_Xl7C 0# x1_Xk59 ipv21_XkAu
                   of s3_Xk5d [OS=OneShot] { __DEFAULT ->
                   case writeAddrOffAddr#
                          @ RealWorld
                          (plusAddr# ptr1_Xl7C d_ajyi)
                          0#
                          ww10_syvz
                          s3_Xk5d
                   of s4_Xk5I [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepLifted
                          @ ForeignPtrContents
                          ww11_syvA
                          s4_Xk5I
                   of s'1_Xitu [OS=OneShot] { __DEFAULT ->
                   case newAlignedPinnedByteArray#
                          @ RealWorld size_ak3d 8# s'1_Xitu
                   of _ [Occ=Dead] { (# ipv28_Xl7R, ipv29_XkAo #) ->
                   case unsafeFreezeByteArray#
                          @ RealWorld ipv29_XkAo ipv28_Xl7R
                   of _ [Occ=Dead] { (# ipv30_Xl85, ipv31_XkAy #) ->
                   case $slength v1_XcN9
                   of _ [Occ=Dead] { I# x2_Xk5b ->
                   let {
                     ptr2_Xl7G [Dmd=<S,U>] :: Addr#

                     ptr2_Xl7G = byteArrayContents# ipv31_XkAy } in
                   case writeIntOffAddr#
                          @ RealWorld ptr2_Xl7G 0# x2_Xk5b ipv30_Xl85
                   of s5_Xk5f [OS=OneShot] { __DEFAULT ->
                   case v1_XcN9
                   of _ [Occ=Dead]
                   { Data.Vector.Storable.Vector dt_XhKx dt1_XhKA dt2_XhcR ->
                   case writeAddrOffAddr#
                          @ RealWorld
                          (plusAddr# ptr2_Xl7G d_ajyi)
                          0#
                          dt1_XhKA
                          s5_Xk5f
                   of s6_Xk5K [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepLifted
                          @ ForeignPtrContents
                          dt2_XhcR
                          s6_Xk5K
                   of s'2_Xitw [OS=OneShot] { __DEFAULT ->
                   case newAlignedPinnedByteArray#
                          @ RealWorld size_ak3d 8# s'2_Xitw
                   of _ [Occ=Dead] { (# ipv32_Xl8g, ipv33_Xl7Q #) ->
                   case unsafeFreezeByteArray#
                          @ RealWorld ipv33_Xl7Q ipv32_Xl8g
                   of _ [Occ=Dead] { (# ipv34_Xl8u, ipv35_Xl84 #) ->
                   case $slength v2_XcNf
                   of _ [Occ=Dead] { I# x3_XkDf ->
                   let {
                     ptr3_XlFF [Dmd=<S,U>] :: Addr#

                     ptr3_XlFF = byteArrayContents# ipv35_Xl84 } in
                   case writeIntOffAddr#
                          @ RealWorld ptr3_XlFF 0# x3_XkDf ipv34_Xl8u
                   of s7_XkDl [OS=OneShot] { __DEFAULT ->
                   case v2_XcNf
                   of _ [Occ=Dead]
                   { Data.Vector.Storable.Vector dt4_XhKW dt5_XhKw dt6_XhKz ->
                   case writeAddrOffAddr#
                          @ RealWorld
                          (plusAddr# ptr3_XlFF d_ajyi)
                          0#
                          dt5_XhKw
                          s7_XkDl
                   of s8_XkDY [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepLifted
                          @ ForeignPtrContents
                          dt6_XhKz
                          s8_XkDY
                   of s'3_Xj1M [OS=OneShot] { __DEFAULT ->
                   case newAlignedPinnedByteArray#
                          @ RealWorld size_ak3d 8# s'3_Xj1M
                   of _ [Occ=Dead] { (# ipv36_XkAl, ipv37_XkAn #) ->
                   case unsafeFreezeByteArray#
                          @ RealWorld ipv37_XkAn ipv36_XkAl
                   of _ [Occ=Dead] { (# ipv38_XkAv, ipv39_XkAx #) ->
                   case $slength v_XcMS of _ [Occ=Dead] { I# x4_Xk5a ->
                   let {
                     ptr4_Xl7E [Dmd=<S,U>] :: Addr#

                     ptr4_Xl7E = byteArrayContents# ipv39_XkAx } in
                   case writeIntOffAddr#
                          @ RealWorld ptr4_Xl7E 0# x4_Xk5a ipv38_XkAv
                   of s9_Xk5e [OS=OneShot] { __DEFAULT ->
                   case v_XcMS
                   of _ [Occ=Dead]
                   { Data.Vector.Storable.Vector dt7_XhcM dt8_XhcO dt9_XhcQ ->
                   case writeAddrOffAddr#
                          @ RealWorld
                          (plusAddr# ptr4_Xl7E d_ajyi)
                          0#
                          dt8_XhcO
                          s9_Xk5e
                   of s10_Xk5J [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepLifted
                          @ ForeignPtrContents
                          dt9_XhcQ
                          s10_Xk5J
                   of s'4_Xitv [OS=OneShot] { __DEFAULT ->
                   case {__pkg_ccall_GC main gibbsC_shim Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Addr#
                                 -> Int#
                                 -> State# RealWorld
                                 -> (# State# RealWorld, Addr# #)}_dfFk
                          ptr_ak3q ptr1_Xl7C ptr2_Xl7G ptr3_XlFF ptr4_Xl7E 1# s'4_Xitv
                   of _ [Occ=Dead] { (# ds18_dfFi [OS=OneShot], ds19_dfFh #) ->
                   case touch#
                          @ 'PtrRepUnlifted
                          @ ByteArray#
                          ipv39_XkAx
                          ds18_dfFi
                   of s11_ak3F [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepUnlifted
                          @ ByteArray#
                          ipv35_Xl84
                          s11_ak3F
                   of s12_XkBr [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepUnlifted
                          @ ByteArray#
                          ipv31_XkAy
                          s12_XkBr
                   of s13_XkBP [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepUnlifted
                          @ ByteArray#
                          ipv22_XkAw
                          s13_XkBP
                   of s14_XkCg [OS=OneShot] { __DEFAULT ->
                   case touch#
                          @ 'PtrRepUnlifted
                          @ ByteArray#
                          ipv13_ak3s
                          s14_XkCg
                   of s15_XkCH [OS=OneShot] { __DEFAULT ->
                   (# s15_XkCH, Ptr @ (ArrayType Double) ds19_dfFh #)
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   }
                   })
                `cast` ...)
               ipv2_XiL0
        of _ [Occ=Dead] { (# ipv4_XiLk, ipv5_XiLm #) ->
        case $wtime
               @ (Maybe Int)
               ((\ (s_aigs [OS=OneShot] :: State# RealWorld) ->
                   case Text.Printf.$fPrintfTypeIO1
                          @ ()
                          ($sprintf3 `cast` ...)
                          lvl89_rIs7
                          (:
                             @ (ModifierParser, FieldFormatter)
                             (Text.Printf.$wparseIntFormat @ Int,
                              Text.Printf.$fPrintfArgInt_$sformatInt trial_abMV)
                             (:
                                @ (ModifierParser, FieldFormatter)
                                (Text.Printf.$wparseIntFormat @ Int,
                                 Text.Printf.$fPrintfArgInt_$sformatInt vocabSize_abMU)
                                (:
                                   @ (ModifierParser, FieldFormatter)
                                   (Text.Printf.$wparseIntFormat @ Int,
                                    Text.Printf.$fPrintfArgInt_$sformatInt k_abMT)
                                   (:
                                      @ (ModifierParser, FieldFormatter)
                                      (Text.Printf.$wparseIntFormat @ Int,
                                       Text.Printf.$fPrintfArgInt_$sformatInt numDocs_abMS)
                                      ([] @ Text.Printf.UPrintf)))))
                          s_aigs
                   of _ [Occ=Dead] { (# ipv6_aigv, ipv7_aigw #) ->
                   case vocabSize_abMU of _ [Occ=Dead] { I# ww1_sypf ->
                   case $wvocabPrior ww1_sypf ipv1_aif5 ipv6_aigv
                   of _ [Occ=Dead] { (# ipv8_XiLs, ipv9_XiLu #) ->
                   case k_abMT of _ [Occ=Dead] { I# ww3_syo5 ->
                   case $wlabelPrior ww3_syo5 ipv1_aif5 ipv8_XiLs
                   of _ [Occ=Dead] { (# ipv10_XjhR, ipv11_XiLr #) ->
                   ((((gibbs
                         (case ipv9_XiLu
                          of _ [Occ=Dead]
                          { Data.Vector.Vector ipv12_spWH ipv13_spWI ipv14_spWJ ->
                          case runRW#
                                 @ 'PtrRepLifted
                                 @ (Data.Vector.Unboxed.Base.Vector Double)
                                 (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                                    case tagToEnum# @ Bool (<# ipv13_spWI 0#)
                                    of _ [Occ=Dead] {
                                      False ->
                                        case divInt# 9223372036854775807# 8#
                                        of ww4_agN3 { __DEFAULT ->
                                        case tagToEnum#
                                               @ Bool (># ipv13_spWI ww4_agN3)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case newByteArray#
                                                   @ (Control.Monad.Primitive.PrimState
                                                        (ST RealWorld))
                                                   (*# ipv13_spWI 8#)
                                                   (s1_ah7U `cast` ...)
                                            of _ [Occ=Dead] { (# ipv15_ai7t, ipv16_ai7u #) ->
                                            letrec {
                                              $s$wfoldlM'_loop_sEln [Occ=LoopBreaker]
                                                :: State# RealWorld
                                                   -> Int#
                                                   -> Int#
                                                   -> (# State# RealWorld, Int #)

                                              $s$wfoldlM'_loop_sEln =
                                                \ (sc_sElm [OS=OneShot]
                                                     :: State# RealWorld)
                                                  (sc1_sElk :: Int#)
                                                  (sc2_sElj :: Int#) ->
                                                  case tagToEnum#
                                                         @ Bool (>=# sc1_sElk ipv13_spWI)
                                                  of _ [Occ=Dead] {
                                                    False ->
                                                      case indexArray#
                                                             @ Double
                                                             ipv14_spWJ
                                                             (+# ipv12_spWH sc1_sElk)
                                                      of _ [Occ=Dead] { (# ipv17_ai43 #) ->
                                                      case ipv17_ai43
                                                      of _ [Occ=Dead] { D# x#_asBl ->
                                                      case writeDoubleArray#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  (ST RealWorld))
                                                             ipv16_ai7u
                                                             sc2_sElj
                                                             x#_asBl
                                                             (sc_sElm `cast` ...)
                                                      of s'#_asBn [OS=OneShot] { __DEFAULT ->
                                                      $s$wfoldlM'_loop_sEln
                                                        (s'#_asBn `cast` ...)
                                                        (+# sc1_sElk 1#)
                                                        (+# sc2_sElj 1#)
                                                      }
                                                      }
                                                      };
                                                    True -> (# sc_sElm, I# sc2_sElj #)
                                                  }; } in
                                            case $s$wfoldlM'_loop_sEln (ipv15_ai7t `cast` ...) 0# 0#
                                            of _ [Occ=Dead] { (# ipv17_ah4v, ipv18_ah4w #) ->
                                            case ipv18_ah4w
                                            of _ [Occ=Dead] { I# dt4_ai91 ->
                                            case unsafeFreezeByteArray#
                                                   @ (Control.Monad.Primitive.PrimState
                                                        (ST RealWorld))
                                                   ipv16_ai7u
                                                   (ipv17_ah4v `cast` ...)
                                            of _ [Occ=Dead] { (# ipv19_ai4Z, ipv20_ai50 #) ->
                                            (# ipv19_ai4Z `cast` ...,
                                               (Data.Vector.Primitive.Vector
                                                  @ Double 0# dt4_ai91 ipv20_ai50)
                                               `cast` ... #)
                                            }
                                            }
                                            }
                                            };
                                          True -> case lvl25_rIqP ipv13_spWI of wild4_00 { }
                                        }
                                        };
                                      True -> case lvl26_rIqQ ipv13_spWI of wild3_00 { }
                                    })
                          of _ [Occ=Dead] { (# ipv15_ah84, ipv16_ah85 #) ->
                          ipv16_ah85
                          }
                          })
                         (case ipv11_XiLr
                          of _ [Occ=Dead]
                          { Data.Vector.Vector ipv12_spY7 ipv13_spY8 ipv14_spY9 ->
                          case runRW#
                                 @ 'PtrRepLifted
                                 @ (Data.Vector.Unboxed.Base.Vector Double)
                                 (\ (s1_ah7U [OS=OneShot] :: State# RealWorld) ->
                                    case tagToEnum# @ Bool (<# ipv13_spY8 0#)
                                    of _ [Occ=Dead] {
                                      False ->
                                        case divInt# 9223372036854775807# 8#
                                        of ww4_agN3 { __DEFAULT ->
                                        case tagToEnum#
                                               @ Bool (># ipv13_spY8 ww4_agN3)
                                        of _ [Occ=Dead] {
                                          False ->
                                            case newByteArray#
                                                   @ (Control.Monad.Primitive.PrimState
                                                        (ST RealWorld))
                                                   (*# ipv13_spY8 8#)
                                                   (s1_ah7U `cast` ...)
                                            of _ [Occ=Dead] { (# ipv15_ai7t, ipv16_ai7u #) ->
                                            letrec {
                                              $s$wfoldlM'_loop_sElw [Occ=LoopBreaker]
                                                :: State# RealWorld
                                                   -> Int#
                                                   -> Int#
                                                   -> (# State# RealWorld, Int #)

                                              $s$wfoldlM'_loop_sElw =
                                                \ (sc_sElv [OS=OneShot]
                                                     :: State# RealWorld)
                                                  (sc1_sElt :: Int#)
                                                  (sc2_sEls :: Int#) ->
                                                  case tagToEnum#
                                                         @ Bool (>=# sc1_sElt ipv13_spY8)
                                                  of _ [Occ=Dead] {
                                                    False ->
                                                      case indexArray#
                                                             @ Double
                                                             ipv14_spY9
                                                             (+# ipv12_spY7 sc1_sElt)
                                                      of _ [Occ=Dead] { (# ipv17_ai43 #) ->
                                                      case ipv17_ai43
                                                      of _ [Occ=Dead] { D# x#_asBl ->
                                                      case writeDoubleArray#
                                                             @ (Control.Monad.Primitive.PrimState
                                                                  (ST RealWorld))
                                                             ipv16_ai7u
                                                             sc2_sEls
                                                             x#_asBl
                                                             (sc_sElv `cast` ...)
                                                      of s'#_asBn [OS=OneShot] { __DEFAULT ->
                                                      $s$wfoldlM'_loop_sElw
                                                        (s'#_asBn `cast` ...)
                                                        (+# sc1_sElt 1#)
                                                        (+# sc2_sEls 1#)
                                                      }
                                                      }
                                                      };
                                                    True -> (# sc_sElv, I# sc2_sEls #)
                                                  }; } in
                                            case $s$wfoldlM'_loop_sElw (ipv15_ai7t `cast` ...) 0# 0#
                                            of _ [Occ=Dead] { (# ipv17_ah4v, ipv18_ah4w #) ->
                                            case ipv18_ah4w
                                            of _ [Occ=Dead] { I# dt4_ai91 ->
                                            case unsafeFreezeByteArray#
                                                   @ (Control.Monad.Primitive.PrimState
                                                        (ST RealWorld))
                                                   ipv16_ai7u
                                                   (ipv17_ah4v `cast` ...)
                                            of _ [Occ=Dead] { (# ipv19_ai4Z, ipv20_ai50 #) ->
                                            (# ipv19_ai4Z `cast` ...,
                                               (Data.Vector.Primitive.Vector
                                                  @ Double 0# dt4_ai91 ipv20_ai50)
                                               `cast` ... #)
                                            }
                                            }
                                            }
                                            };
                                          True -> case lvl25_rIqP ipv13_spY8 of wild4_00 { }
                                        }
                                        };
                                      True -> case lvl26_rIqQ ipv13_spY8 of wild3_00 { }
                                    })
                          of _ [Occ=Dead] { (# ipv15_ah84, ipv16_ah85 #) ->
                          ipv16_ah85
                          }
                          })
                         z1_abMY
                         w_abMZ
                         doc_sm5C
                         b_rIrT)
                      `cast` ...)
                       ipv1_aif5)
                    `cast` ...)
                     ipv10_XjhR
                   }
                   }
                   }
                   }
                   })
                `cast` ...)
               ipv4_XiLk
        of _ [Occ=Dead] { (# ipv6_XiLn, ipv7_XiLp #) ->
        (# ipv6_XiLn, () #)
        }
        }
        }
    }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 11}
runner :: Int -> Int -> Int -> Int -> IO ()
[GblId,
 Arity=5,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= main2 `cast` ...}]
runner = main2 `cast` ...

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl93_rIsc :: [Char]

lvl93_rIsc = unpackCString# "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl94_rIsd :: [Char]

lvl94_rIsd = unpackCString# "Main"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl95_rIse :: [Char]

lvl95_rIse = unpackCString# "NBHakaru.hs"#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl96_rIsf :: Int

lvl96_rIsf = I# 149#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl97_rIsg :: Int

lvl97_rIsg = I# 10#

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl98_rIsh :: Int

lvl98_rIsh = I# 61#

-- RHS size: {terms: 8, types: 0, coercions: 0}
lvl99_rIsi :: Types.SrcLoc

lvl99_rIsi =
  Types.SrcLoc
    lvl93_rIsc
    lvl94_rIsd
    lvl95_rIse
    lvl96_rIsf
    lvl97_rIsg
    lvl96_rIsf
    lvl98_rIsh

-- RHS size: {terms: 4, types: 0, coercions: 0}
lvl100_rIsj :: Types.CallStack

lvl100_rIsj =
  Types.PushCallStack
    lvl_rIqm lvl99_rIsi Types.EmptyCallStack

-- RHS size: {terms: 2, types: 0, coercions: 0}
lvl101_rIsk :: [Char]

lvl101_rIsk =
  unpackCString#
    "NBHakaru <num docs> <k> <vocabSize> <trial>"#

-- RHS size: {terms: 5, types: 7, coercions: 4}
$wfail [InlPrag=[0]]
  :: State# RealWorld
     -> (# State# RealWorld, () #)

$wfail =
  \ _ [Occ=Dead, OS=OneShot] ->
    case error
           @ 'PtrRepLifted
           @ (IO ())
           (lvl100_rIsj `cast` ...)
           lvl101_rIsk
    of wild_00 {
    }

-- RHS size: {terms: 43, types: 41, coercions: 0}
main1
  :: State# RealWorld
     -> (# State# RealWorld, () #)
[GblId,
 Arity=1,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [0] 250 0}]
main1 =
  \ (s_aif1 [OS=OneShot] :: State# RealWorld) ->
    case System.Environment.getArgs1 s_aif1
    of _ [Occ=Dead] { (# ipv_aif4, ipv1_aif5 #) ->
    case map @ [Char] @ Int $sread ipv1_aif5 of _ [Occ=Dead] {
      [] -> $wfail ipv_aif4;
      : numDocs_abVP ds_dfEn ->
        case ds_dfEn of _ [Occ=Dead] {
          [] -> $wfail ipv_aif4;
          : k_abVQ ds2_dfEo ->
            case ds2_dfEo of _ [Occ=Dead] {
              [] -> $wfail ipv_aif4;
              : vocabSize_abVR ds3_dfEp ->
                case ds3_dfEp of _ [Occ=Dead] {
                  [] -> $wfail ipv_aif4;
                  : trial_abVS ds4_dfEq ->
                    case ds4_dfEq of _ [Occ=Dead] {
                      [] ->
                        main2 numDocs_abVP k_abVQ vocabSize_abVR trial_abVS ipv_aif4;
                      : ipv2_sgT4 ipv3_sgT5 -> $wfail ipv_aif4
                    }
                }
            }
        }
    }
    }

-- RHS size: {terms: 1, types: 0, coercions: 3}
main :: IO ()
[GblId,
 Arity=1,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= main1 `cast` ...}]
main = main1 `cast` ...

-- RHS size: {terms: 2, types: 1, coercions: 3}
main3
  :: State# RealWorld
     -> (# State# RealWorld, () #)
[GblId,
 Arity=1,

 Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 60}]
main3 = runMainIO1 @ () (main1 `cast` ...)

-- RHS size: {terms: 1, types: 0, coercions: 3}
:main :: IO ()
[GblId,
 Arity=1,

 Unf=Unf{Src=InlineStable, TopLvl=True, Value=True, ConLike=True,
         WorkFree=True, Expandable=True,
         Guidance=ALWAYS_IF(arity=0,unsat_ok=True,boring_ok=True)
         Tmpl= main3 `cast` ...}]
:main = main3 `cast` ...

------ Local rules for imported ids --------
"SPEC/Main unstream @ Vector @ Int" [ALWAYS]
    forall ($dVector_sh2U
              :: G.Vector Data.Vector.Unboxed.Base.Vector Int).
      G.unstream @ Data.Vector.Unboxed.Base.Vector @ Int $dVector_sh2U
      = $sreplicate_$sunstream
"SPEC/Main replicate @ Vector @ Int" [ALWAYS]
    forall ($dVector_sgUS
              :: G.Vector Data.Vector.Unboxed.Base.Vector Int).
      G.replicate @ Data.Vector.Unboxed.Base.Vector @ Int $dVector_sgUS
      = $sreplicate
"SPEC/Main read @ Int" [ALWAYS]
    forall ($dRead_shbg :: Read Int).
      read @ Int $dRead_shbg
      = $sread
"SPEC/Main length @ (MayBoxVec Double) @ Double" [ALWAYS]
    forall ($dVector_shh2 :: G.Vector (MayBoxVec Double) Double).
      G.length @ (MayBoxVec Double) @ Double $dVector_shh2
      = $s!_$slength2
"SPEC/Main length @ (MayBoxVec Int) @ Int" [ALWAYS]
    forall ($dVector_shh4 :: G.Vector (MayBoxVec Int) Int).
      G.length @ (MayBoxVec Int) @ Int $dVector_shh4
      = $s!_$slength1
"SPEC/Main length @ (MayBoxVec (Vector Double)) @ (Vector Double)" [ALWAYS]
    forall ($dVector_shlJ
              :: G.Vector
                   (MayBoxVec (Data.Vector.Unboxed.Base.Vector Double))
                   (Data.Vector.Unboxed.Base.Vector Double)).
      G.length @ (MayBoxVec (Data.Vector.Unboxed.Base.Vector Double))
               @ (Data.Vector.Unboxed.Base.Vector Double)
               $dVector_shlJ
      = $s!_$slength
"SPEC/Main length @ Vector @ Double" [ALWAYS]
    forall ($dVector_Xi2m :: G.Vector SV.Vector Double).
      G.length @ SV.Vector @ Double $dVector_Xi2m
      = $slength2
"SPEC/Main length @ Vector @ Int" [ALWAYS]
    forall ($dVector_Xi2o :: G.Vector SV.Vector Int).
      G.length @ SV.Vector @ Int $dVector_Xi2o
      = $slength

Linking NBHakaru ...

