module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Text (Text)
import Trumat
import Array8 (malloc, Array8, get, set, makeIndex)
import Ast (malloc)
import Data.ByteString (ByteString, snoc, length, uncons)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

main :: IO ()
main =
    defaultMain $
    testGroup "Tests" $
    Prelude.map oneTest cases

oneTest :: (String, Text, Text) -> TestTree
oneTest (name, unformatted, expected) =
    testCase name $
        do
        ast <- Ast.malloc
        buffer <- Array8.malloc 1000
        fillResult <- fillBuffer buffer unformatted
        fillResult @?= Right ()
        formatResult <- trumat ast buffer
        formatResult @?= Right ()
        got <- readBuffer buffer
        got @?= Right expected

readBuffer :: Array8 -> IO (Either String Text)
readBuffer buffer =
    readBufferHelp buffer ""

fillBuffer :: Array8 -> Text -> IO (Either String ())
fillBuffer buffer text =
    fillBufferHelp buffer (encodeUtf8 text) 0


fillBufferHelp :: Array8 -> ByteString -> Int -> IO (Either String ())
fillBufferHelp buffer text counter =
            case Data.ByteString.uncons text of
                Nothing ->
                    return $ Right ()

                Just (first , "") ->
                    do
                    maybeIndex <- Array8.makeIndex buffer counter
                    case maybeIndex of
                        Nothing ->
                            return $ Left "buffer too small"

                        Just index ->
                            do
                            Array8.set first index
                            return $ Right ()

                Just (first , remainder) ->
                    do
                    maybeIndex <- Array8.makeAppend buffer counter
                    putStrLn $ "COUNTER IS: " <> show counter
                    case maybeIndex of
                        Nothing ->
                            return $ Left "buffer too small"

                        Just index ->
                            do
                            Array8.append first index
                            fillBufferHelp buffer remainder (counter + 1)
                    
            
readBufferHelp :: Array8 -> ByteString -> IO (Either String Text)
readBufferHelp buffer accum =
    do
    maybeIndex <- Array8.makeIndex buffer (Data.ByteString.length accum)
    case maybeIndex of
        Nothing ->
          return
            (case decodeUtf8' accum of
                Left err ->
                    Left $ show err

                Right text ->
                    Right text)

        Just index ->
            do
            byte <- Array8.get index
            readBufferHelp buffer (Data.ByteString.snoc accum byte)


cases :: [ (String, Text, Text) ]
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
        \"
      )
    ]
