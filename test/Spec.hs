import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $
  testCase "2+2=4" $ do
    2 + 2 @?= 4
