{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Even where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import ValidLiterals.Class

newtype Even = Even Integer deriving (Show, Generic)

instance NFData Even

instance Integral a => Validate a Even where
    fromLiteral i
        | even i = Just . Even $ fromIntegral i
        | otherwise = Nothing

    spliceValid _ (Even i) = [|| Even i ||]
