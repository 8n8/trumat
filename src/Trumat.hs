module Trumat (trumat) where

import Data.Set (Set, fromList, member)
import Data.Text (Text, intercalate, isInfixOf, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    choice,
    chunk,
    eof,
    errorBundlePretty,
    getSourcePos,
    lookAhead,
    many,
    notFollowedBy,
    parse,
    some,
    sourceColumn,
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
    fmap,
    head,
    length,
    mconcat,
    not,
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
    (>),
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
    [ parseCaseOf indent,
      parseTuple indent,
      parseList indent,
      parseRecord indent,
      parseIfThenElse indent,
      parseLetIn indent,
      try $ parseFunctionCall indent,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral
    ]

parseRecord :: Int -> Parser Text
parseRecord indent =
  do
    _ <- char '{'
    _ <- parseSpaces
    items <- many (parseRecordItem indent)
    _ <- char '}'
    if null items
      then return "{}"
      else
        return $
          mconcat
            [ "{ ",
              intercalate ", " items,
              " }"
            ]

parseRecordItem :: Int -> Parser Text
parseRecordItem indent =
  do
    name <- parseName
    _ <- space
    _ <- char '='
    _ <- space
    right <- parseExpression indent
    _ <- space
    _ <- choice [char ',', lookAhead (try (char '}'))]
    _ <- space
    return $
      mconcat
        [ name,
          " = ",
          right
        ]

parseTripleStringLiteral :: Parser Text
parseTripleStringLiteral =
  do
    _ <- chunk "\"\"\""
    contents <- many parseTripleStringLiteralChar
    _ <- chunk "\"\"\""
    return $ mconcat ["\"\"\"", mconcat contents, "\"\"\""]

parseTripleStringLiteralChar :: Parser Text
parseTripleStringLiteralChar =
  choice
    [ takeWhile1P Nothing (\ch -> ch /= '"' && ch /= '\\'),
      chunk "\\\"",
      chunk "\\\\",
      try $ do
        _ <- char '"'
        _ <- notFollowedBy (char '"')
        return "\""
    ]

parseSimpleStringLiteral :: Parser Text
parseSimpleStringLiteral =
  do
    _ <- char '"'
    contents <- many parseSimpleStringLiteralChar
    _ <- char '"'
    return $ mconcat ["\"", mconcat contents, "\""]

parseSimpleStringLiteralChar :: Parser Text
parseSimpleStringLiteralChar =
  choice
    [ takeWhile1P Nothing (\ch -> ch /= '"' && ch /= '\\'),
      chunk "\\\"",
      chunk "\\\\"
    ]

parsePattern :: Int -> Parser Text
parsePattern indent =
  choice
    [ parseTuple indent,
      parseList indent,
      parseVerbatim
    ]

parseFunctionCall :: Int -> Parser Text
parseFunctionCall indent =
  do
    lininess <- try $ lookAhead functionCallLininess
    f <- parseName
    items <- some $
      try $
        do
          _ <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
          parseExpression indent
    let spaces =
          case lininess of
            MultiLine ->
              pack $ '\n' : (take (indent + 4) $ repeat ' ')
            SingleLine ->
              " "
    return $ intercalate spaces (f : items)

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

functionCallLininess :: Parser ContainerType
functionCallLininess =
  do
    startColumn <- fmap sourceColumn getSourcePos
    _ <- parseName
    items <-
      some $
        try $
          do
            spaces <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
            argColumn <- fmap sourceColumn getSourcePos
            if argColumn > startColumn
              then do
                _ <- parseExpression 8
                return spaces
              else fail "argument is indented less than function"
    let combined = mconcat items
    if isInfixOf "\n" combined
      then return MultiLine
      else return SingleLine

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

parseLetIn :: Int -> Parser Text
parseLetIn indent =
  do
    _ <- chunk "let"
    _ <- space1
    let_ <- some $
      try $
        do
          items <- parseLetBind (indent + 4)
          _ <- space1
          return items
    _ <- chunk "in"
    _ <- space1
    in_ <- parseExpression indent
    let inSpaces = "\n" <> (pack $ take indent $ repeat ' ')
    return $
      mconcat
        [ "let\n",
          intercalate "\n\n" let_,
          inSpaces,
          "in",
          inSpaces,
          in_
        ]

parseLetBind :: Int -> Parser Text
parseLetBind indent =
  do
    left <- parseExpression indent
    _ <- space
    _ <- char '='
    _ <- space
    right <- parseExpression indent
    let leftSpaces = pack $ take indent $ repeat ' '
    let rightSpaces = pack $ take (indent + 4) $ repeat ' '
    return $
      mconcat
        [ leftSpaces,
          left,
          " =\n",
          rightSpaces,
          right
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
    left <- parsePattern indent
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
  fromList ["case", "of", "let", "in", "if", "then", "else", "->"]

parseName :: Parser Text
parseName =
  do
    word <-
      takeWhile1P
        (Just "name character")
        (\ch -> ch `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz." :: String))
    if member word keywords
      then fail $ "expecting a name but got: " <> unpack word
      else return word

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

parseOpen :: Parser ()
parseOpen =
  do
    _ <- choice [char '(', char '[', char '{']
    return ()

parseClose :: Parser ()
parseClose =
  do
    _ <- choice [char ')', char ']', char '}']
    return ()

parseListType :: Parser ContainerType
parseListType =
  do
    _ <- parseOpen
    parseListTypeHelp 1

parseListTypeHelp :: Int -> Parser ContainerType
parseListTypeHelp nesting =
  if nesting == 0
    then return SingleLine
    else do
      _ <-
        takeWhileP
          Nothing
          (\ch -> not (ch `elem` ("\n([{}])" :: String)))
      choice
        [ do
            _ <- char '\n'
            return MultiLine,
          do
            _ <- parseOpen
            parseListTypeHelp (nesting + 1),
          do
            _ <- parseClose
            parseListTypeHelp (nesting - 1)
        ]

parseTuple :: Int -> Parser Text
parseTuple indent =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineTuple indent
      MultiLine ->
        parseMultiLineTuple indent

parseMultiLineTuple :: Int -> Parser Text
parseMultiLineTuple indent =
  do
    _ <- char '('
    _ <- space
    items <- many (parseListItem (indent + 2) space ')')
    _ <- char ')'
    if null items
      then return "()"
      else
        if length items == 1
          then return $ head items
          else
            return $
              mconcat
                [ "( ",
                  intercalate ("\n" <> (pack $ take indent $ repeat ' ') <> ", ") items,
                  "\n" <> (pack $ take indent $ repeat ' ') <> ")"
                ]

parseSingleLineTuple :: Int -> Parser Text
parseSingleLineTuple indent =
  do
    _ <- char '('
    _ <- parseSpaces
    items <- many (parseListItem indent parseSpaces ')')
    _ <- char ')'
    if null items
      then return "()"
      else
        if length items == 1
          then return $ head items
          else
            return $
              mconcat
                [ "( ",
                  intercalate ", " items,
                  " )"
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
    items <- many (parseListItem (indent + 2) space ']')
    _ <- char ']'
    if null items
      then return "[]"
      else
        return $
          mconcat
            [ "[ ",
              intercalate ("\n" <> (pack $ take indent $ repeat ' ') <> ", ") items,
              "\n" <> (pack $ take indent $ repeat ' ') <> "]"
            ]

parseSingleLineList :: Int -> Parser Text
parseSingleLineList indent =
  do
    _ <- char '['
    _ <- parseSpaces
    items <- many (parseListItem indent parseSpaces ']')
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

parseSpaces :: Parser ()
parseSpaces =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return ()
