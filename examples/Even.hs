{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Even where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import ValidLiterals

newtype Even = Even Integer deriving (Show, Generic, Lift)

instance NFData Even

instance Integral a => Validate a Even where
    fromLiteral i
        | even i = Just . Even $ fromIntegral i
        | otherwise = Nothing
