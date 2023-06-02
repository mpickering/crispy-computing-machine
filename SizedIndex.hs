{-# LANGUAGE DerivingStrategies, ViewPatterns, AllowAmbiguousTypes, RankNTypes #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
-- | Sized indexes and shapes for tensors.
module SizedIndex  where

import Prelude

import GHC.TypeLits (KnownNat, type (+))


import           Data.Proxy (Proxy (Proxy))
import           Data.Type.Equality ((:~:) (Refl))
import           GHC.Exts (IsList (..))
import           GHC.TypeLits

infixr 3 :::
data SizedList (n :: Nat) i where
  Z :: SizedList 0 i
  (:::) :: KnownNat n
        => i -> SizedList n i -> SizedList (1 + n) i

type ShapeInt n = Shape n Int

-- * Tensor indexes as fully encapsulated sized lists, with operations

-- | An index in an n-dimensional array represented as a sized list.
-- The slowest-moving index is at the head position;
-- thus the index 'i :. j :. Z' represents 'a[i][j]' in traditional C notation.
newtype Index n i = Index (SizedList n i)

pattern ZI :: forall n i. () => n ~ 0 => Index n i
pattern ZI = Index Z

infixr 3 :.
pattern (:.) :: forall n1 i. KnownNat n1 => forall n. (KnownNat n, (1 + n) ~ n1)
             => i -> Index n i -> Index n1 i
pattern i :. sh <- (unconsIndex -> Just (UnconsIndexRes sh i))
  where i :. (Index sh) = Index (i ::: sh)
{-# COMPLETE ZI, (:.) #-}

data UnconsIndexRes i n1 =
  forall n. n1 ~ (1 + n) => UnconsIndexRes (Index n i) i
unconsIndex :: Index n1 i -> Maybe (UnconsIndexRes i n1)
unconsIndex (Index sh) = case sh of
  i ::: sh' -> Just (UnconsIndexRes (Index sh') i)
  Z -> Nothing

data Shape n i = Shape (SizedList n i)

pattern ZS :: forall n i. () => n ~ 0 => Shape n i
pattern ZS = Shape Z

infixr 3 :$
pattern (:$) :: forall n1 i. KnownNat n1 => forall n. (KnownNat n, (1 + n) ~ n1)
             => i -> Shape n i -> Shape n1 i
pattern i :$ sh <- (unconsShape -> Just (UnconsShapeRes sh i))
  where i :$ (Shape sh) = Shape (i ::: sh)
{-# COMPLETE ZS, (:$) #-}

data UnconsShapeRes i n1 =
  forall n. n1 ~ (1 + n) => UnconsShapeRes (Shape n i) i
unconsShape :: Shape n1 i -> Maybe (UnconsShapeRes i n1)
unconsShape (Shape sh) = case sh of
  i ::: sh' -> Just (UnconsShapeRes (Shape sh') i)
  Z -> Nothing


