-------------------------------------------------------------------------------
-- |
-- Module      :  ValidLiterals
-- Copyright   :  (C) 2015 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  portable
--
-- __WARNING: For implementors of new 'Validate' instances only!__
--
-- This module exposes functions whose use is easily screwed up. Users of
-- validated literals should use the interface exported by "ValidLiterals".
-- This module only exists for implementers of new 'Validate' instances.
-------------------------------------------------------------------------------
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module ValidLiterals.Class where

import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- | Class for validated, compile-time, partial conversions from type 'a' to
-- 'b'.
class Validate a b where
    -- | Converts 'a' values into validated 'b' values
    fromLiteral :: a -> Maybe b

    -- | __WARNING: Don't use! For implementors of 'Validate' instances only!__
    --
    -- Produces the actual Typed Template Haskell splice for the validated
    -- value to splice into the source text. Since you may want to implement a
    -- 'Validate' instance for types that cannot have 'Lift' instances, this
    -- function receives __both__ the original 'a' value and the resulting 'b'
    -- as input.
    --
    -- This allows it to either:
    --
    --      1. Splice the resulting 'b' into the source using it's Lift
    --      instance, or a custom splice.
    --      2. Splice the initial 'a' into the source using it's Lift instance,
    --      and perform the conversion again at runtime, and coercing via,
    --      e.g., 'fromJust'. Since 'fromLiteral' is pure (right?!) this should
    --      be perfectly safe.
    --
    -- Clearly, splicing the result directly is much safer (it avoids any
    -- shenanigans with 'unsafePerformIO') and more efficient (no conversion at
    -- runtime). However, this is just not always possible, since, for example,
    -- @ByteString@ does not have a 'Lift' instance and we may still want to
    -- have validated newtypes of @ByteString@.
    --
    -- __Default implementation:__ The default implementation (using
    -- DefaultSignatures) uses 'b''s 'Lift' instance to do the efficient thing
    -- of directly splicing the resulting 'b' into the source.
    spliceValid :: a -> b -> Q (TExp b)
    default spliceValid :: Lift b => a -> b -> Q (TExp b)
    spliceValid _ v = [|| v ||]

-- | Default implementation for 'spliceValid', uses the 'Lift' instance for 'a'
-- and 'fromJust' to redo the conversion at runtime and (unsafely) coerce from
-- 'Maybe b' to 'b'. . Since 'fromLiteral' is pure (right?!) this should be
-- perfectly safe.
hackySpliceValid :: (Lift a, Validate a b) => a -> b -> Q (TExp b)
hackySpliceValid v _ = [|| fromJust (fromLiteral v) ||]
