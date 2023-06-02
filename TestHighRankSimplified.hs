{-# OPTIONS_GHC -ddump-to-file -ddump-simpl -ddump-spec -ddump-ds -dsuppress-coercions -dverbose-core2core -ddump-rule-rewrites #-}
{-# LANGUAGE DataKinds, TypeFamilies  #-}
{-# LANGUAGE OverloadedLists, UndecidableInstances, AllowAmbiguousTypes #-}
module TestHighRankSimplified(spec, spec3) where

import Prelude
import Data.Typeable
import Data.Proxy
import TensorClass

spec f = tbuild' @Double @33 @0 f

spec3 = typeRep (Proxy @4)




