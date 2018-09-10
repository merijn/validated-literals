{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module ByteString (ByteString) where

import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString as BS
import Data.Char
import Language.Haskell.TH.Syntax

import ValidLiterals

instance Lift ByteString where
    lift v = AppE <$> [| BS.pack |] <*> lift (BS.unpack v)

instance Validate String ByteString where
    fromLiteral s
        | all ((<=255) . ord) s = Just $ pack s
        | otherwise = Nothing
