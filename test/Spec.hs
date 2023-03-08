module Main (main) where

import Test.Tasty (testGroup, defaultMain, TestTree)
import qualified Trumat
import Data.Word (Word8)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import qualified Ast
import qualified Array8
import Array8 (Array8)
import Data.Text (Text)
import Test.Tasty.HUnit ((@?=), testCase)
import qualified Data.ByteString as ByteString
import Prelude
    (String
    , Int
    , (+)
    , return
    , reverse
    , map
    , IO, ($), Maybe(Just, Nothing), Either(Left, Right))

main :: IO ()
main = defaultMain $ testGroup "unit tests" (map oneTest cases)


oneTest :: (String, Text, Text) -> TestTree
oneTest (name, unformatted, formatted) =
  testCase name $
    do
    buffer <- Array8.malloc 1000

    fillResult <- fillBuffer buffer unformatted
    fillResult @?= Right ()

    ast <- Ast.malloc

    formatResult <- Trumat.format buffer ast
    formatResult @?= Right ()

    got <- readBuffer buffer
    got @?= Right formatted


fillBuffer :: Array8 -> Text -> IO (Either String ())
fillBuffer buffer text =
    fillBufferHelp buffer (ByteString.unpack $ encodeUtf8 text)


fillBufferHelp :: Array8 -> [Word8] -> IO (Either String ())
fillBufferHelp buffer words =
    case words of
        [] ->
            return $ Right ()

        top : remainder ->
            do
            result <- Array8.push top buffer
            case result of
                Nothing ->
                    return $ Left "the buffer is too short"

                Just () ->
                    fillBufferHelp buffer remainder


readBuffer :: Array8 -> IO (Either String Text)
readBuffer buffer =
    readBufferHelp buffer 0 []

readBufferHelp :: Array8 -> Int -> [Word8] -> IO (Either String Text)
readBufferHelp buffer index accumulated =
    do
    maybeWord <- Array8.get index buffer
    case maybeWord of
        Nothing ->
            case decodeUtf8' $ ByteString.pack $ reverse accumulated of
                Left _ ->
                    return $ Left "invalid UTF8"

                Right text ->
                    return $ Right text

        Just word ->
            readBufferHelp buffer (index + 1) (word : accumulated)
    

cases :: [(String, Text, Text)]
cases =
    [ ( "hello world formatted"
      , "module X exposing (x)\n\
        \\n\
        \\n\
        \x =\n\
        \    0\n\
        \"
      , "module X exposing (x)\n\
        \\n\
        \\n\
        \x =\n\
        \    0\n\
        \")
    ]

