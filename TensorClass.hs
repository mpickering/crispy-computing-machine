{-# LANGUAGE OverloadedLists, UndecidableInstances  #-}
{-# OPTIONS_GHC  -fno-specialise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-simpl -ddump-ds -ddump-ds-preopt -dsuppress-coercions #-}
-- | Dual numbers and various operations on them, arithmetic and related
-- to tensors (vectors, matrices and others). This is a part of
-- the high-level API of the horde-ad library, defined using the mid-level
-- (and safely impure) API in "HordeAd.Core.DualClass". The other part
-- of the high-level API is in "HordeAd.Core.Engine".
module TensorClass
  ( IndexOf, ShapeInt, Tensor(..), tbuild'
  ) where


import Prelude

import           GHC.TypeLits (KnownNat, Nat, type (+))
import GHC.TypeNats

import SizedIndex
import Data.Proxy
import Debug.Trace
import Data.Typeable


class ( RealFloat r
      , Integral (IntOf r) )
      => Tensor r where
  type TensorOf (n :: Nat) r = result | result -> n r
  type IntOf r

instance Tensor Double where
  type TensorOf n Double = Proxy n
  type IntOf Double = Int


tbuild1' x y = undefined
{-# NOINLINE tbuild1' #-}

takeShape' :: forall m n a b . a -> b
takeShape' x = undefined
{-# NOINLINE takeShape' #-}

takeShape'' :: forall m n i. KnownNat m
           => Shape (m + n) i -> Shape m i
takeShape'' (Shape ix) = Shape $ undefined --takeSized ix
{-# NOINLINE takeShape'' #-}

type IndexOf n r = Index n (IntOf r)

tbuild' :: forall r m n. (Tensor r, KnownNat (m + n), KnownNat m, KnownNat n)
       => ShapeInt (m + n) -> (IndexOf m r -> TensorOf n r)
       -> TensorOf (m + n) r
tbuild' sh0 f0 =
  let
    buildSh :: (KnownNat (m1 + n), KnownNat m1)

            => ShapeInt m1 -> (IndexOf m1 r -> TensorOf n r)
                    -> TensorOf (m1 + n) r
    buildSh (Shape Z) f = f (Index Z)
    buildSh ((:$) @ix k sh) f =
--    buildSh (Shape ((:::) @ix k sh)) f =
          let rep = typeRep (Proxy @ix)
--          in traceShow rep (withKnownNat (addSNat (twiddleSnat (natSing @ix)) (natSing @n)) (tbuild1' k (\i -> buildSh sh (\ix -> f (i :. ix)))))
          in traceShow rep ((tbuild1' k (\i -> buildSh sh (\ix -> f (i :. ix)))))
  in
    buildSh (takeShape'' @m @n sh0) f0

{-
addSNat :: SNat n -> SNat m -> SNat (n + m)
addSNat (UnsafeSNat m) (UnsafeSNat n) = (UnsafeSNat (n + m))

twiddleSnat :: forall n1 m1 . (1 + n1 ~ m1) => SNat m1 -> SNat n1
twiddleSnat (UnsafeSNat m) = UnsafeSNat (m - 1)
-}




