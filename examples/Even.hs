{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Even (Even, mkEven) where

import ValidLiterals
import Language.Haskell.TH.Syntax (Lift (..))

newtype Even = Even Int deriving (Eq, Ord, Show)

mkEven :: Int -> Maybe Even
mkEven int = if even int then Just (Even int) else Nothing

instance Lift Even where
   lift (Even int) = [| Even int |]

instance FromLiteral Integer Even where
    fromLiteral integerValue =
        case fromLiteral integerValue of
            Right intValue ->
                if even intValue
                    then Right (Even intValue)
                    else Left "Number must be even"
            Left reason ->
                Left reason

instance DefaultLiteralType Integer Even
