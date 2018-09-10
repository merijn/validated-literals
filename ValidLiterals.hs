-------------------------------------------------------------------------------
-- |
-- Module      :  ValidLiterals
-- Copyright   :  (C) 2015 Merijn Verstraaten, Nikita Churaev
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
-- @newtype Even = Even Int@ and write:
--
-- @
-- x :: Even
-- x = $$(valid 38)
-- @
--
-- This will check, at compile time, that the provided 'Integer' is, in fact,
-- even and unwrap it from 'Either', avoiding the runtime check.
--
-- Complete usage example:
--
-- @
-- {-\# LANGUAGE TemplateHaskell #-}
--
-- module Main where
--
-- import ValidLiterals
-- import Language.Haskell.TH.Syntax (Lift (..))
--
-- newtype Even = Even Int deriving (Eq, Ord, Show)
--
-- instance Lift Even where
--    lift (Even int) = [| Even int |]
--
-- instance FromLiteral Integer Even where
--     fromLiteral integerValue =
--         case fromLiteral integerValue of
--             Right intValue ->
--                 if even intValue
--                     then Right (Even intValue)
--                     else Left "Number must be even"
--             Left reason ->
--                 Left reason
--
-- instance DefaultLiteralType Integer Even
--
-- main :: IO ()
-- main = do
--     print ($$(valid 38) :: Even) -- OK.
--     print ($$(valid 35) :: Even) -- Compile-time error.
-- @
-------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module ValidLiterals (
    valid,
    valid',
    FromLiteral (..),
    DefaultLiteralType,
    fallbackSpliceValid,
    LiteralAndValue,
    lvValue,
    lvLiteral
) where

import Numeric.Natural (Natural)
import Data.Int (Int64, Int32, Int16, Int8)
import Data.Word (Word64, Word32, Word16, Word8)
import Language.Haskell.TH.Syntax (Q, TExp, Lift)

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
-- x = $$(valid 'c')
-- @
--
-- See the module description for a complete usage example.
valid :: DefaultLiteralType l a => l -> Q (TExp a)
valid = valid'

-- | Same as 'valid', but does not force the default literal type. Be wary of
-- potential ambiguities. For example @$$(valid' 3)@ is ambiguous, since the
-- literal @3@ has the following type: @Num l => l@, so the compiler can't know
-- which @l@ to use.
valid' :: FromLiteral l a => l -> Q (TExp a)
valid' lit =
    case fromLiteral lit of
        Right val -> spliceValid (LiteralAndValue lit val)
        Left err -> fail err

-- | Class for validated, compile-time conversions from type 'l' to 'a'.
--
-- See the module description for a complete usage example.
class FromLiteral l a where
    -- | Converts an @'l'@ value to an @'a'@ value. Returns @('Right' value)@ on
    -- success and @('Left' errorMessage)@ on failure.
    fromLiteral :: l -> Either String a

    -- | Produces the actual Typed Template Haskell splice for the validated
    -- value to splice into the source text. Since you may want to implement a
    -- 'FromLiteral' instance for types that cannot have 'Lift' instances, this
    -- function receives __both__ the original 'l' value and the resulting 'a'
    -- as input.
    --
    -- This allows it to either:
    --
    --      1. Splice the resulting 'a' into the source using it's Lift
    --      instance, or a custom splice.
    --      2. Splice the initial 'l' into the source using it's Lift instance,
    --      and perform the conversion again at runtime. Since 'fromLiteral' is
    --      pure (right?!) this should be perfectly safe.
    --
    -- Clearly, splicing the result directly is much safer (it avoids any
    -- shenanigans with 'unsafePerformIO') and more efficient (no conversion at
    -- runtime). However, this is just not always possible, since, for example,
    -- 'ByteString' does not have a 'Lift' instance and we may still want to
    -- have validated newtypes of 'ByteString'.
    --
    -- __Default implementation:__ The default implementation (using
    -- DefaultSignatures) uses 'a''s 'Lift' instance to do the efficient thing
    -- of directly splicing the resulting 'a' into the source. If 'a' cannot
    -- have a 'Lift' instance but 'l' has one, you can use 'fallbackSpliceValid'
    -- to easily implement 'spliceValid' using the 'l''s instance.
    spliceValid :: LiteralAndValue l a -> Q (TExp a)
    default spliceValid :: Lift a => LiteralAndValue l a -> Q (TExp a)
    spliceValid (LiteralAndValue _ val) = [|| val ||]

-- | Specifies the default literal type for 'a'. See the description of
-- 'valid'' for the rationale.
class FromLiteral l a => DefaultLiteralType l a | a -> l

-- | Fallback implementation of 'spliceValid' for cases where it's impossible
-- to lift the resulting value. Uses the 'Lift' instance of the literal type 'l'
-- to redo the conversion at runtime.
fallbackSpliceValid
    :: (Lift l, FromLiteral l a) => LiteralAndValue l a -> Q (TExp a)
fallbackSpliceValid (LiteralAndValue lit _) =
    [|| case fromLiteral lit of
            Right val -> val
            Left err -> error err ||]

-- | Holds a value and the literal it was obtained from by 'fromLiteral'.
data LiteralAndValue l a = LiteralAndValue {
    lvLiteral :: l, -- ^ Gets the original literal.
    lvValue   :: a  -- ^ Gets the value obtained from the literal.
  }

instance FromLiteral Integer Natural where
  fromLiteral integer =
    if integer >= 0
      then Right (fromInteger integer)
      else Left ("Natural is negative: " ++ show integer)

instance FromLiteral Integer Int where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Int64 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Int32 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Int16 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Int8 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Word where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Word64 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Word32 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Word16 where
  fromLiteral = boundedIntegralFromLiteral

instance FromLiteral Integer Word8 where
  fromLiteral = boundedIntegralFromLiteral

instance DefaultLiteralType Integer Natural

instance DefaultLiteralType Integer Int

instance DefaultLiteralType Integer Int64

instance DefaultLiteralType Integer Int32

instance DefaultLiteralType Integer Int16

instance DefaultLiteralType Integer Int8

instance DefaultLiteralType Integer Word

instance DefaultLiteralType Integer Word64

instance DefaultLiteralType Integer Word32

instance DefaultLiteralType Integer Word16

instance DefaultLiteralType Integer Word8

boundedIntegralFromLiteral
  :: forall a . (Integral a, Bounded a) => Integer -> Either String a
boundedIntegralFromLiteral lit =
  if lit >= integerMin && lit <= integerMax
    then Right (fromInteger lit)
    else Left ("Integer out of range: " ++ show lit ++
               " (minimum: " ++ show integerMin ++
               ", maximum: " ++ show integerMax ++ ")")
  where
    integerMin = toInteger (minBound :: a)
    integerMax = toInteger (maxBound :: a)
