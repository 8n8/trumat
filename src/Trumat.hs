module Trumat (trumat) where

import Data.Set (Set, fromList, member)
import Data.Text (Text, intercalate, pack, singleton, unpack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    lookAhead,
    many,
    parse,
    some,
    takeWhile1P,
    takeWhileP,
    try,
  )
import Text.Megaparsec.Char (char, space)
import Prelude
  ( Char,
    Either (..),
    Int,
    Maybe (..),
    Show,
    String,
    elem,
    fail,
    mconcat,
    null,
    repeat,
    return,
    take,
    ($),
    (&&),
    (+),
    (-),
    (/=),
    (<>),
    (==),
    (>>),
    (||),
  )

preamble :: Text
preamble =
  "module X exposing (x)\n\
  \\n\
  \\n\
  \x =\n\
  \    "

type Parser =
  Parsec Void Text

trumat :: Text -> Either String Text
trumat unformatted =
  case parse parser "" unformatted of
    Left err ->
      Left $ errorBundlePretty err
    Right formatted ->
      Right formatted

parser :: Parser Text
parser =
  do
    _ <- chunk preamble
    expression <- parseExpression 4
    _ <- space
    _ <- eof
    return $ preamble <> expression <> "\n"

parseExpression :: Int -> Parser Text
parseExpression indent =
  choice
    [ parseList indent '(' ')',
      parseList indent '[' ']',
      parseCaseOf indent,
      parseIfThenElse indent,
      parseVerbatim
    ]

space1 :: Parser ()
space1 =
  do
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    return ()

parseIfThenElseType :: Parser ContainerType
parseIfThenElseType =
  do
    _ <- chunk "if"
    _ <- space
    expressionLininess

expressionLininess :: Parser ContainerType
expressionLininess =
  choice
    [ listLininess '[' ']',
      listLininess '(' ')',
      verbatimLininess
    ]

verbatimLininess :: Parser ContainerType
verbatimLininess =
  return SingleLine

listLininess :: Char -> Char -> Parser ContainerType
listLininess start end =
  do
    _ <- char start
    listLinynessHelp 1 start end

listLinynessHelp :: Int -> Char -> Char -> Parser ContainerType
listLinynessHelp nesting start end =
  if nesting == 0
    then return SingleLine
    else do
      _ <- takeWhileP Nothing (\ch -> ch /= start && ch /= end && ch /= '\n')
      choice
        [ do
            _ <- char start
            listLinynessHelp (nesting + 1) start end,
          do
            _ <- char '\n'
            return MultiLine,
          do
            _ <- char end
            listLinynessHelp (nesting - 1) start end
        ]

parseIfThenElse :: Int -> Parser Text
parseIfThenElse indent =
  do
    ifThenElseType <- lookAhead parseIfThenElseType
    _ <- chunk "if"
    _ <- space1
    if_ <- parseExpression (indent + 4)
    _ <- space1
    _ <- chunk "then"
    _ <- space1
    then_ <- parseExpression (indent + 4)
    _ <- space1
    _ <- chunk "else"
    _ <- space1
    else_ <- parseExpression (indent + 4)
    return $
      mconcat
        [ "if",
          case ifThenElseType of
            SingleLine ->
              " "
            MultiLine ->
              "\n" <> pack (take (indent + 4) (repeat ' ')),
          if_,
          case ifThenElseType of
            SingleLine ->
              " "
            MultiLine ->
              "\n" <> pack (take indent (repeat ' ')),
          "then\n" <> pack (take (indent + 4) (repeat ' ')),
          then_,
          "\n\n" <> pack (take indent (repeat ' ')),
          "else\n" <> pack (take (indent + 4) (repeat ' ')),
          else_
        ]

parseCaseOfType :: Parser ContainerType
parseCaseOfType =
  do
    _ <- choice [chunk "case ", chunk "case\n"]
    parseCaseOfTypeHelp

parseOf :: Parser ()
parseOf =
  do
    _ <- choice [chunk " of", chunk "\nof"]
    _ <- lookAhead $ choice [char ' ', char '\n']
    return ()

parseCaseOfTypeHelp :: Parser ContainerType
parseCaseOfTypeHelp =
  choice
    [ parseOf >> return SingleLine,
      do
        _ <- takeWhileP Nothing (\ch -> ch /= 'o' && ch /= '\n' && ch /= ' ')
        choice
          [ do
              _ <- char '\n'
              return MultiLine,
            parseCaseOfTypeHelp
          ]
    ]

parseCaseOf :: Int -> Parser Text
parseCaseOf indent =
  do
    caseOfType <- lookAhead parseCaseOfType
    _ <- chunk "case"
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    caseOf <- parseExpression (indent + 4)
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    _ <- chunk "of"
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    branches <- some (parseCaseOfBranch (indent + 4))
    return $
      mconcat
        [ "case",
          case caseOfType of
            MultiLine ->
              "\n" <> pack (take (indent + 4) (repeat ' '))
            SingleLine ->
              " ",
          caseOf,
          case caseOfType of
            MultiLine ->
              "\n" <> pack (take indent (repeat ' ') <> "of\n")
            SingleLine ->
              " of\n",
          intercalate "\n\n" branches
        ]

parseCaseOfBranch :: Int -> Parser Text
parseCaseOfBranch indent =
  do
    left <- parseExpression indent
    _ <- space
    _ <- chunk "->"
    _ <- space
    right <- parseExpression (indent + 4)
    _ <- space
    return $
      mconcat
        [ pack $ take indent $ repeat ' ',
          left,
          " ->\n",
          pack $ take (indent + 4) $ repeat ' ',
          right
        ]

keywords :: Set Text
keywords =
  fromList ["case", "of"]

parseVerbatim :: Parser Text
parseVerbatim =
  do
    word <-
      takeWhile1P
        (Just "verbatim character")
        (\ch -> ch `elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" :: String))
    if member word keywords
      then fail $ "expecting a verbatim, but got: " <> unpack word
      else return word

parseListItem :: Int -> Parser () -> Char -> Parser Text
parseListItem indent spaceParser end =
  do
    expression <- parseExpression indent
    _ <- spaceParser
    _ <- choice [char ',', lookAhead (try (char end))]
    _ <- spaceParser
    return expression

data ContainerType
  = SingleLine
  | MultiLine
  deriving (Show)

parseListType :: Char -> Char -> Parser ContainerType
parseListType start end =
  do
    _ <- char start
    parseListTypeHelp 1 start end

parseListTypeHelp :: Int -> Char -> Char -> Parser ContainerType
parseListTypeHelp nesting start end =
  if nesting == 0
    then return SingleLine
    else do
      _ <- takeWhileP Nothing (\ch -> ch /= '\n' && ch /= start && ch /= end)
      choice
        [ do
            _ <- char '\n'
            return MultiLine,
          do
            _ <- char start
            parseListTypeHelp (nesting + 1) start end,
          do
            _ <- char end
            parseListTypeHelp (nesting - 1) start end
        ]

parseList :: Int -> Char -> Char -> Parser Text
parseList indent start end =
  do
    listType <- lookAhead (parseListType start end)
    case listType of
      SingleLine ->
        parseSingleLineList indent start end
      MultiLine ->
        parseMultiLineList indent start end

parseMultiLineList :: Int -> Char -> Char -> Parser Text
parseMultiLineList indent start end =
  do
    _ <- char start
    _ <- space
    items <- many (parseListItem (indent + 2) space end)
    _ <- char end
    if null items
      then return $ pack [start, end]
      else
        return $
          mconcat
            [ pack [start, ' '],
              intercalate (",\n" <> (pack $ take indent $ repeat ' ')) items,
              "\n" <> (pack $ take indent $ repeat ' ') <> singleton end
            ]

parseSpaces :: Parser ()
parseSpaces =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return ()

parseSingleLineList :: Int -> Char -> Char -> Parser Text
parseSingleLineList indent start end =
  do
    _ <- char start
    _ <- parseSpaces
    items <- many (parseListItem indent parseSpaces end)
    _ <- char end
    if null items
      then return $ pack [start, end]
      else
        return $
          mconcat
            [ pack [start, ' '],
              intercalate ", " items,
              pack [' ', end]
            ]
