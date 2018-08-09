{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Even where

import ValidLiterals
import ValidLiterals.Class

newtype Even = Even Integer

instance Integral a => Validate a Even where
    fromLiteral i
        | even i = Just . Even $ fromIntegral i
        | otherwise = Nothing

    spliceValid _ (Even i) = [|| Even i ||]
