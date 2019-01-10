-------------------------------------------------------------------------------
-- |
-- Module      :  ValidLiterals
-- Copyright   :  (C) 2015-2019 Merijn Verstraaten
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
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module ValidLiterals
    ( Validate(..)
    , ValidationFailure(..)
    , valid
    , validInteger
    , validRational
    , validString
    , validList
    -- * Re-export from "Language.Haskell.TH.Syntax"
    , Lift(..)
    ) where

import Control.Exception (Exception(displayException), throwIO)
import Data.Maybe (maybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import Language.Haskell.TH.Syntax

-- | 'Exception' type for failed conversions. Useful for testing and more
-- gracefully handling compile time failures.
data ValidationFailure = ValidationFailure String deriving (Show, Typeable)

instance Exception ValidationFailure where
    displayException (ValidationFailure s) = "Validation failure: " ++ s

-- | Class for validated, compile-time, partial conversions from type 'a' to
-- 'b'.
class Validate a b where
    -- | Converts 'a' values into validated 'b' values, 'Left' values are
    -- reported in the compilation error.
    fromLiteralWithError :: a -> Either String b
    fromLiteralWithError = maybe (Left errMsg) Right . fromLiteral
      where
        errMsg = "An error occured during compile-time validation!"

    -- | Converts 'a' values into validated 'b' values, 'Nothing' values
    -- produce a generic error message. Use 'fromLiteralWithError' for custom
    -- error messages.
    fromLiteral :: a -> Maybe b
    fromLiteral = either (const Nothing) Just . fromLiteralWithError

    {-# MINIMAL fromLiteralWithError | fromLiteral #-}

    -- | Creates a Typed TH splice for the resulting 'b' values, useful for
    -- avoiding the need for orphan 'Lift' instances and allowing complex
    -- splices for types that can't be directly lifted. See the 'ByteString'
    -- example module for an example.
    liftResult :: Proxy a -> b -> Q (TExp b)
    default liftResult :: Lift b => Proxy a -> b -> Q (TExp b)
    liftResult _ val = [|| val ||]

-- | The core function of ValidLiterals, use this together with Typed Template
-- Haskell splices to insert validated literals into your code. For example, if
-- we assume @newtype ASCII = ASCII Char@ where @ASCII@ should only contain
-- ASCII characters, we would write:
--
-- Polymorphic literals, such as numbers (or strings when @OverloadedStrings@
-- is enabled) can result in ambiguous type errors with this function. Enabing
-- the @ExtendedDefaultRules@ extension will allow inputs to 'valid' to be
-- defaulted to 'Integer' or 'Double' allowing code to compile. A more robust
-- solution is to use the various explicitly defaulted functions in this
-- module, such as 'validInteger'.
--
-- @
-- {-\# LANGUAGE TemplateHaskell #-}
--
-- import ValidLiterals
--
-- x :: ASCII
-- x = $$(valid \'c\')
-- @
valid :: forall a b . Validate a b => a -> Q (TExp b)
valid input = case fromLiteralWithError input of
    Right result -> liftResult (Proxy :: Proxy a) result
    Left err -> do
        reportError $ unlines
            [ "Invalid input used for type-safe validated literal!", err ]
        runIO $ throwIO (ValidationFailure err)

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
