{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module ByteString (ByteString) where

import Data.ByteString.Char8 (ByteString, pack)
import Data.Char

import ValidLiterals

instance Validate String ByteString where
    fromLiteral s
        | all ((<=255) . ord) s = Just $ pack s
        | otherwise = Nothing

    spliceValid = hackySpliceValid
