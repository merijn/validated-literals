{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module ValidLiterals where

import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

class Validate a b where
    fromLiteral :: a -> Maybe b

    spliceValid :: a -> b -> Q (TExp b)
    default spliceValid :: Lift b => a -> b -> Q (TExp b)
    spliceValid _ v = [|| v ||]

valid :: Validate a b => a -> Q (TExp b)
valid input = case fromLiteral input of
    Nothing -> fail "Invalid input used for type-safe validated literal!"
    Just result -> spliceValid input result

validInteger :: Validate Integer b => Integer -> Q (TExp b)
validInteger = valid

validRational :: Validate Rational b => Rational -> Q (TExp b)
validRational = valid

validString :: Validate String b => String -> Q (TExp b)
validString = valid

validList :: Validate [a] b => [a] -> Q (TExp b)
validList = valid

hackySpliceValid :: (Lift a, Validate a b) => a -> b -> Q (TExp b)
hackySpliceValid v _ = [|| fromJust (fromLiteral v) ||]
