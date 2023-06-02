{-# LANGUAGE DerivingStrategies #-}
-- | @GHC.Nat@-indexed lists.
module HordeAd.Internal.SizedList
  ( SizedList(..)
  , listToSized, sizedListToList, takeSized
  ) where

import Prelude

import           Data.Array.Internal (valueOf)
import           Data.Proxy (Proxy (Proxy))
import           Data.Type.Equality ((:~:) (Refl))
import           GHC.Exts (IsList (..))
import           GHC.TypeLits
  (KnownNat, Nat, OrderingI (..), cmpNat, sameNat, type (+), type (-))

infixr 3 :::
data SizedList (n :: Nat) i where
  Z :: SizedList 0 i
  (:::) :: KnownNat n
        => i -> SizedList n i -> SizedList (1 + n) i

deriving instance Eq i => Eq (SizedList n i)

deriving instance Ord i => Ord (SizedList n i)

-- This is pretty controversion and only lawful when OverloadedLists
-- is enabled. However, it's much more readable when tracing and debugging.
instance Show i => Show (SizedList n i) where
  showsPrec d l = showsPrec d (sizedListToList l)

deriving stock instance Functor (SizedList n)

instance Foldable (SizedList n) where
  foldr f z l = foldr f z (sizedListToList l)

instance KnownNat n => IsList (SizedList n i) where
  type Item (SizedList n i) = i
  fromList = listToSized
  toList = sizedListToList

takeSized :: forall len n i. KnownNat len
          => SizedList (len + n) i -> SizedList len i
takeSized ix = listToSized $ take (valueOf @len) $ sizedListToList ix
-- Look Ma, no unsafeCoerce! This compiles only with GHC >= 9.2,
-- but the rest of our code caught up and fails with GHC 9.0 as well.
listToSized :: forall n i. KnownNat n => [i] -> SizedList n i
listToSized []
  | Just Refl <- sameNat (Proxy @n) (Proxy @0) = Z
  | otherwise = error $ "listToSized: list too short; missing "
                        ++ show (valueOf @n :: Int)
listToSized (i : is)
  -- What we really need here to make the types check out is a <= check.
  | EQI <- cmpNat (Proxy @1) (Proxy @n) =
      let sh = listToSized @(n - 1) is
      in i ::: sh
  | LTI <- cmpNat (Proxy @1) (Proxy @n) =
      let sh = listToSized @(n - 1) is
      in i ::: sh
  | otherwise =
      error $ "listToSized: list too long; spurious " ++ show (length (i : is))

sizedListToList :: SizedList n i -> [i]
sizedListToList Z = []
sizedListToList (i ::: is) = i : sizedListToList is
