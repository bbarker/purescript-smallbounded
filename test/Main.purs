module Test.Main where

import Prelude

-- import Data.Array                        ((!!), length)
import Data.XPath                        (root, at, xx, (//), (/?))
-- import Debug.Trace                       (traceM)
import Effect                            (Effect)
import Effect.Aff                        (Aff)
import Effect.Class                      (liftEffect)
import Effect.Console                    (logShow)
-- import Foreign                           (isUndefined, isNull, unsafeToForeign)
import Test.Unit                         (suite, test)
import Test.Unit.Main                    (runTest)
import Test.Unit.Assert                  as Assert


nsd :: String
nsd = "x"

rec :: String
rec = "record"

ident :: String
ident = "identifier"

testRootRecId :: String
testRootRecId = root // rec // ident
expectRootRecId :: String
expectRootRecId = root <> rec <> "/" <> ident

testRootRecIdNSD :: String
testRootRecIdNSD = root /? rec /? ident
expectRootRecIdNSD :: String
expectRootRecIdNSD = "/" <> nsd <> ":" <> rec <> "/" <> nsd <> ":" <> ident

expectIdentWithNS :: String
expectIdentWithNS = nsd <> ":" <> ident
testIdentWithNS :: String
testIdentWithNS = xx ident

dummyAttribName :: String
dummyAttribName = "dummyAttrib"

main :: Effect Unit
main = runTest do
  suite "path append tests" do
    test "without dummy prefix" do
      tlog $ expectRootRecId <> " =? " <> testRootRecId
      Assert.equal expectRootRecId testRootRecId
    test "with dummy prefix" do
      tlog $ expectRootRecIdNSD <> " =? " <> testRootRecIdNSD
      Assert.equal expectRootRecIdNSD testRootRecIdNSD
      tlog $ expectIdentWithNS <> " =? " <> testIdentWithNS
      Assert.equal expectIdentWithNS testIdentWithNS
  suite "simple attribute tests" do
    test "prepend @ on string" do
      Assert.equal ("@" <> dummyAttribName) (at dummyAttribName)

tlog :: forall a. Show a => a -> Aff Unit
tlog = liftEffect <<< logShow
