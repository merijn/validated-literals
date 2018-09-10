-------------------------------------------------------------------------------
-- |
-- Module      :  ValidLiterals
-- Copyright   :  (C) 2015 Merijn Verstraaten
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Merijn Verstraaten <merijn@inconsistent.nl>
-- Stability   :  experimental
-- Portability :  portable
--
-- To disallow invalid input it is common to define (new)types with hidden
-- data constructors. Forcing the user to go through a smart-constructor that
-- enforces invariants and returns @Maybe ResultType@, preventing the
-- construction of data with invalid values.
--
-- However, it is __also__ common to want to include literal values of such
-- types in source text. Things of textual literals for HTML, HTTP, etc.
-- In such cases smart-constructors force us to handle potential conversion
-- failures at runtime, or abusing functions like @fromJust@ to break away all
-- the safety smart-constructors provide. All this despite the fact that we
-- can statically know at compile time that the conversion will always succeed
-- or always fails.
--
-- This package provides a typeclasses for using TH to validate the
-- correctness of provided literals at compile. This lets you define, e.g.,
-- @newtype Even = Even Integer@ and write:
--
-- @
-- x :: Even
-- x = $$(valid 38)
-- @
--
-- This will check, at compile time, that the provided 'Integer' is, in fact,
-- even and unwrap it from 'Maybe', avoiding the runtime check.
-------------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module ValidLiterals
    ( Validate(..)
    , valid
    , validInteger
    , validRational
    , validString
    , validList
    , Lift(..)
    ) where

import Language.Haskell.TH.Syntax

-- | Class for validated, compile-time, partial conversions from type 'a' to
-- 'b'.
class Lift b => Validate a b where
    -- | Converts 'a' values into validated 'b' values
    fromLiteral :: a -> Maybe b

-- | The core function of ValidLiterals, use this together with Typed Template
-- Haskell splices to insert validated literals into your code. For example, if
-- we assume @newtype ASCII = ASCII Char@ where @ASCII@ should only contain
-- ASCII characters, we would write:
--
-- @
-- {-\# LANGUAGE TemplateHaskell #-}
--
-- import ValidLiterals
--
-- x :: ASCII
-- x = $$(valid \'c\')
-- @
valid :: Validate a b => a -> Q (TExp b)
valid input = case fromLiteral input of
    Nothing -> fail "Invalid input used for type-safe validated literal!"
    Just result -> [|| result ||]

-- | Integer literals lead to obnoxious defaulting complaints by GHC, by
-- using this function you can force the defaulting to 'Integer', silencing
-- these warnings.
--
-- Since 'Integral' literals use @fromInteger :: Num a => Integer -> a@ this
-- function cannot cost you any precision.
validInteger :: Validate Integer b => Integer -> Q (TExp b)
validInteger = valid

-- | Same as 'validInteger', but for 'Fractional' values.
--
-- Since 'Fractional' literals use
-- @fromRational :: Fractional a => Rational -> a@ this function cannot cost
-- you any precision.
validRational :: Validate Rational b => Rational -> Q (TExp b)
validRational = valid

-- | Same as 'validInteger', but for when enabling @OverloadedStrings@ makes
-- 'String' literals polymorphic.
validString :: Validate String b => String -> Q (TExp b)
validString = valid

-- | Same as 'validInteger', but for when enabling @OverloadedLists@ makes list
-- literals polymorphic.
validList :: Validate [a] b => [a] -> Q (TExp b)
validList = valid
