{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.DeepSeq (NFData, force)
import Control.Exception
    (Exception(..), SomeException(..), bracket_, evaluate, try)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Language.Haskell.TH (Q, TExp, runQ)
import System.IO (IOMode(WriteMode), stderr, hFlush, withFile)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)
import Test.Tasty.Travis

import ValidLiterals
import Even
import ByteString

failingEven :: Q (TExp Even)
failingEven = validInteger 39

failingByteString :: Q (TExp ByteString)
failingByteString = valid "λ"

evenVal :: Even
evenVal = $$(validInteger 38)

bytestringVal :: ByteString
bytestringVal = $$(valid "HTTP/1.1 GET")

checkExceptions :: NFData a => String -> a -> TestTree
checkExceptions name expr = testCase name $ do
    result <- try . evaluate . force $ expr
    case result of
        Right _ -> return ()
        Left (SomeException e) -> assertFailure (displayException e)

withRedirectedStderr :: IO a -> IO a
withRedirectedStderr act = withFile "/dev/null" WriteMode $ \nullHnd -> do
    hFlush stderr
    oldStderr <- hDuplicate stderr
    bracket_ (hDuplicateTo nullHnd stderr) (hDuplicateTo oldStderr stderr) act

checkTHFails :: String -> Q a -> TestTree
checkTHFails name thExpr = testCase name $ do
    result <- try . withRedirectedStderr $ runQ thExpr
    case result of
        Right _ -> assertFailure "TH didn't fail!"
        Left e | Just ValidationFailure{} <- fromException e -> return ()
               | otherwise -> assertFailure "Unexpected TH failure!"

allTests :: TestTree
allTests = testGroup "Tests"
  [ checkExceptions "Even" evenVal
  , checkExceptions "ByteString" bytestringVal
  , checkTHFails "Failing Even" $ failingEven
  , checkTHFails "Failing ByteString" $ failingByteString
  ]

main :: IO ()
main = travisTestReporter travisConfig [] allTests
  where
    travisConfig = defaultConfig
      { travisFoldGroup = FoldMoreThan 1
      , travisSummaryWhen = SummaryAlways
      }
