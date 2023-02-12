module Trumat (trumat) where

import Data.Set (Set, fromList, member)
import Data.Text (Text, intercalate, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
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
  ( Either (..),
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
    return $ preamble <> expression <> "\n"

parseExpression :: Int -> Parser Text
parseExpression indent =
  choice [parseList indent, parseCaseOf indent, parseVerbatim]

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

parseListItem :: Int -> Parser () -> Parser Text
parseListItem indent spaceParser =
  do
    expression <- parseExpression indent
    _ <- spaceParser
    _ <- choice [char ',', lookAhead (try (char ']'))]
    _ <- spaceParser
    return expression

data ContainerType
  = SingleLine
  | MultiLine
  deriving (Show)

parseListType :: Parser ContainerType
parseListType =
  do
    _ <- char '['
    parseListTypeHelp 1

parseListTypeHelp :: Int -> Parser ContainerType
parseListTypeHelp nesting =
  if nesting == 0
    then return SingleLine
    else do
      _ <- takeWhileP Nothing (\ch -> ch /= '\n' && ch /= '[' && ch /= ']')
      choice
        [ do
            _ <- char '\n'
            return MultiLine,
          do
            _ <- char '['
            parseListTypeHelp (nesting + 1),
          do
            _ <- char ']'
            parseListTypeHelp (nesting - 1)
        ]

parseList :: Int -> Parser Text
parseList indent =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineList indent
      MultiLine ->
        parseMultiLineList indent

parseMultiLineList :: Int -> Parser Text
parseMultiLineList indent =
  do
    _ <- char '['
    _ <- space
    items <- many (parseListItem (indent + 2) space)
    _ <- char ']'
    if null items
      then return "[]"
      else
        return $
          mconcat
            [ "[ ",
              intercalate (",\n" <> (pack $ take indent $ repeat ' ')) items,
              "\n" <> (pack $ take indent $ repeat ' ') <> "]"
            ]

parseSpaces :: Parser ()
parseSpaces =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return ()

parseSingleLineList :: Int -> Parser Text
parseSingleLineList indent =
  do
    _ <- char '['
    _ <- parseSpaces
    items <- many (parseListItem indent parseSpaces)
    _ <- char ']'
    if null items
      then return "[]"
      else
        return $
          mconcat
            [ "[ ",
              intercalate ", " items,
              " ]"
            ]
