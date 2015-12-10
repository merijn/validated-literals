{-# LANGUAGE TemplateHaskell #-}

module Examples where

import ValidLiterals
import Even
import ByteString

x :: Even
x = $$(valid 38)

b :: ByteString
b = $$(valid "HTTP/1.1 GET")
