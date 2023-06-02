{-# OPTIONS_GHC -ddump-to-file -ddump-simpl -ddump-rule-rewrites -fno-specialise -ddump-rule-rewrites #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import qualified TestHighRankSimplified as T
import Data.Proxy
import Data.Typeable
import Control.Monad
import GHC.TypeLits

main = print T.spec3 >> unless (show (T.spec3) == "4") (error "Bad runtime result")
