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
    fromLiteralWithError i
      | even i = Right . Even $ integer
      | otherwise = Left $ show integer ++ " is not even!"
      where
        integer = fromIntegral i
