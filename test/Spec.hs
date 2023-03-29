import qualified Ast
import Data.Word (Word8)
import Array (Array)
import qualified Array
import qualified Data.Char
import Format (format)
import qualified Capacity


main :: IO ()
main =
    mapM_ oneTest testCases


testCases :: [(String, String, String)]
testCases =
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


fillInBuf :: Array Word8 -> String -> IO (Either String ())
fillInBuf buf str =
    case str of
        "" ->
            return $ Right ()


        top : remainder ->
            if Data.Char.ord top > 127 then
                return $ Left "non-ascii char in test string"

            else
            do
            appendResult <- Array.append (fromIntegral (Data.Char.ord top)) buf
            case appendResult of
                Nothing ->
                    return $ Left "test input buffer too short"

                Just () ->
                    fillInBuf buf remainder




oneTest :: (String, String, String) -> IO ()
oneTest (description, unformatted, formatted) =
    do
    putStrLn description
    inBuf <- Array.new Capacity.k20
    fillResult <- fillInBuf inBuf unformatted
    ast <- Ast.new
    outBuf <- Array.new Capacity.k20
    case fillResult of
        Left _ ->
            putStrLn "FAILURE: could not fill buffer"

        Right () ->
            do
            formatResult <- format inBuf ast outBuf
            case formatResult of
                Left err ->
                    putStrLn ("FAILURE: could not format the code: " ++ err)


                Right () ->
                    do
                    got <- readOutBuf outBuf
                    if got == formatted then
                        putStrLn "SUCCESS"

                    else
                        putStrLn $
                            mconcat
                            [ "FAILURE: expecting "
                            , formatted
                            , " but got "
                            , got
                            ]
                        

readOutBuf :: Array Word8 -> IO String
readOutBuf array =
    readOutBufHelp array 0 ""


readOutBufHelp :: Array Word8 -> Int -> String -> IO String
readOutBufHelp array index accum =
    do
    getResult <- Array.get index array
    case getResult of
        Nothing ->
            return $ reverse accum

        Just byte ->
          readOutBufHelp
            array
            (index + 1)
            ((Data.Char.chr (fromIntegral byte)) : accum)
            
