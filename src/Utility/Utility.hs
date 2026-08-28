{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Utility.Utility (nat)
where

import GHC.TypeNats (KnownNat, natVal)
import Data.Singletons.Base.TH

nat :: forall n. (KnownNat n) => Int
nat = fromIntegral (natVal (Proxy @n))

