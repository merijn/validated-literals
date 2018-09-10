{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ByteString (ByteString) where

import Data.Char
import Data.List
import Data.ByteString.Char8 (ByteString, pack)
import ValidLiterals

instance FromLiteral String ByteString where
    fromLiteral bs =
        case findIndex (\c -> ord c > 255) bs of
            Just index ->
                Left ("Byte at the index " ++ show index ++ " is out of range")
            Nothing ->
                Right (pack bs)

    spliceValid = fallbackSpliceValid

instance DefaultLiteralType String ByteString
