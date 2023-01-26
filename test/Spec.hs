import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString (ByteString)
import Parse
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  defaultMain $ testGroup "Unit tests" $ map oneTest cases

oneTest :: (TestName, ByteString, ByteString) -> TestTree
oneTest (description, input, expected) =
  testCase description $
    parseOnly Parse.parse input @?= Right expected

cases :: [(TestName, ByteString, ByteString)]
cases =
  [ ( "hello world formatted",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \",
      "module X exposing (x)\n\
      \\n\
      \\n\
      \x =\n\
      \    0\n\
      \"
    )
  ]
