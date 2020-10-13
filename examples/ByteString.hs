{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module ByteString (ByteString) where

import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString as BS
import Data.Char
import Language.Haskell.TH.Syntax

import ValidLiterals

instance Validate String ByteString where
    fromLiteralWithError s = case nonAsciiVals of
        [] -> Right $ pack s
        _ -> Left $ "Found non-ASCII values: " ++ show nonAsciiVals
      where
        nonAsciiVals = filter (not . (<=255) . ord) s

    -- As of template-haskell-2.16.0.0 we no longer need to rely on the hack of
    -- unsafeTExpCoerce!
#if MIN_VERSION_template_haskell(2,16,0)
    liftResult _ v = [|| BS.pack $$(liftTyped (BS.unpack v)) ||]
#else
    liftResult _ v = unsafeTExpCoerce $
        AppE <$> [| BS.pack |] <*> lift (BS.unpack v)
#endif
