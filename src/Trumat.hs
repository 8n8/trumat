module Trumat (trumat) where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, intercalate, isInfixOf, pack, unpack)
import qualified Data.Text as Text
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
    sourceLine,
    takeWhile1P,
    takeWhileP,
    token,
    try,
    unPos,
  )
import Text.Megaparsec.Char (char, space)
import Prelude
  ( Char,
    Either (..),
    Eq,
    Int,
    Maybe (..),
    Show,
    String,
    elem,
    fail,
    filter,
    fmap,
    head,
    length,
    map,
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
    (.),
    (/=),
    (<=),
    (<>),
    (==),
    (>),
    (>>),
    (||),
  )

type Parser =
  Parsec Void Text

trumat :: Text -> Either String Text
trumat unformatted =
  case parse parser "" unformatted of
    Left err ->
      Left $ errorBundlePretty err
    Right formatted ->
      Right formatted

parseExposing :: Parser Text
parseExposing =
  do
    listType <- lookAhead parseListType
    docs <- choice [lookAhead $ try parseExportDocs, return []]
    case listType of
      SingleLine ->
        parseSingleLineExports docs
      MultiLine ->
        parseMultiLineExports docs

parseExportDocs :: Parser [[Text]]
parseExportDocs =
  do
    _ <- consumeExportList
    _ <- space
    _ <- chunk "{-|"
    results <- many $ try parseExportDocsRow
    _ <- endDocComment
    return results

consumeExportList :: Parser ()
consumeExportList =
  do
    _ <- char '('
    consumeExportListHelp 1

consumeExportListHelp :: Int -> Parser ()
consumeExportListHelp nesting =
  if nesting == 0
    then return ()
    else do
      _ <- takeWhileP Nothing (\ch -> ch /= ')' && ch /= '(')
      choice
        [ do
            _ <- char '('
            consumeExportListHelp (nesting + 1),
          do
            _ <- char ')'
            consumeExportListHelp (nesting - 1)
        ]

parseExportDocsRow :: Parser [Text]
parseExportDocsRow =
  do
    _ <- takeWhileP Nothing (\ch -> ch /= '@')
    _ <- chunk "@docs"
    _ <- takeWhile1P Nothing (\ch -> ch == ' ')
    some parseOneExportDoc

parseOneExportDoc :: Parser Text
parseOneExportDoc =
  do
    name <- parseName
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- choice [char ',', lookAhead (char '\n')]
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return name

parseExport :: Parser Text
parseExport =
  do
    name <- parseName
    all_ <-
      choice
        [ do
            _ <- char '('
            _ <- takeWhileP Nothing (\ch -> ch /= ')')
            _ <- char ')'
            return "(..)",
          return ""
        ]
    _ <-
      choice
        [ try $ do
            _ <- space
            _ <- char ','
            _ <- space
            return (),
          return ()
        ]
    return $ name <> all_

parseSingleLineExports :: [[Text]] -> Parser Text
parseSingleLineExports docs =
  do
    _ <- char '('
    _ <- parseSpaces
    items <- some parseExport
    _ <- char ')'
    return $ formatSingleLineExports docs items

formatExportRow :: [Text] -> [Text] -> Text
formatExportRow items docRow =
  let documented = filter (\doc -> elem doc items) docRow
   in intercalate ", " documented

formatSingleLineExports :: [[Text]] -> [Text] -> Text
formatSingleLineExports docs items =
  let rows = (map (formatExportRow items) docs) <> [undocumented]
      undocumented = intercalate ", " (getUndocumented docs items)
   in case rows of
        [] ->
          "()"
        [single] ->
          " (" <> single <> ")"
        multiple ->
          mconcat
            [ "\n    ( ",
              intercalate "\n    , " multiple,
              "\n    )"
            ]

getUndocumented :: [[Text]] -> [Text] -> [Text]
getUndocumented docs items =
  let docSet = Set.fromList $ mconcat docs
   in filter (\item -> not (Set.member item docSet)) items

parseMultiLineExports :: [[Text]] -> Parser Text
parseMultiLineExports docs =
  do
    _ <- char '('
    _ <- space
    items <- some parseExport
    _ <- space
    _ <- char ')'
    return $
      mconcat
        [ "\n    ( ",
          constructMultilineExports docs items,
          "\n    )"
        ]

constructMultilineExports :: [[Text]] -> [Text] -> Text
constructMultilineExports _ exports =
  intercalate "\n    , " exports

endDocComment :: Parser Text
endDocComment =
  do
    part <- takeWhileP Nothing (\ch -> ch /= '-')
    _ <- chunk "-}"
    return part

parseModuleDocs :: Parser Text
parseModuleDocs =
  do
    _ <- chunk "{-|"
    doc <- endDocComment
    return $ mconcat ["{-|", doc, "-}"]

parseModuleDeclaration :: Parser Text
parseModuleDeclaration =
  do
    _ <- chunk "module "
    name <- parseName
    _ <- chunk " exposing"
    _ <- space
    exports <- parseExposing
    _ <- space
    moduleDocs <- choice [parseModuleDocs, return ""]
    return $
      mconcat
        [ "module ",
          name,
          " exposing",
          exports,
          if moduleDocs == ""
            then ""
            else "\n\n" <> moduleDocs
        ]

typeAliasDeclaration :: Parser Text
typeAliasDeclaration =
  do
    documentation <- choice [try $ parseDocumentation, return ""]
    _ <- space
    _ <- "type"
    _ <- space
    _ <- "alias"
    _ <- space
    name <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- space
    _ <- char '='
    _ <- space
    type_ <- parseType 4
    _ <- space
    return $
      mconcat
        [ documentation,
          if documentation == ""
            then ""
            else "\n",
          "type alias ",
          name,
          if parameters == ""
            then ""
            else " ",
          parameters,
          " =\n    ",
          type_
        ]

customTypeDeclaration :: Parser Text
customTypeDeclaration =
  do
    _ <- "type"
    _ <- space
    name <- parseName
    _ <- space
    _ <- char '='
    _ <- space
    branches <- some parseBranch
    return $
      mconcat
        [ "type ",
          name,
          "\n    = ",
          intercalate "\n    | " branches
        ]

parseBranch :: Parser Text
parseBranch =
  do
    _ <- space
    column <- fmap sourceColumn getSourcePos
    if unPos column == 0
      then fail "column == 0"
      else do
        startColumn <- fmap (unPos . sourceColumn) getSourcePos
        branchName <- parseName
        _ <- space
        parameters <- parseParameters startColumn
        _ <- choice [char '|', return ' ']
        return $
          mconcat
            [ branchName,
              if parameters == ""
                then ""
                else " ",
              parameters
            ]

parseDocumentation :: Parser Text
parseDocumentation =
  do
    _ <- chunk "{-"
    _ <- choice [char '|', return ' ']
    contents <-
      many $
        do
          _ <- notFollowedBy (char '}')
          takeWhile1P Nothing (\ch -> ch /= '-')
    _ <- chunk "-}"
    return $
      mconcat
        [ "{-|",
          mconcat contents,
          "-}"
        ]

topLevelBind :: Parser Text
topLevelBind =
  do
    documentation <- choice [try $ parseDocumentation, return ""]
    _ <- space
    signature <- choice [try $ parseTypeSignature, return ""]
    _ <- space
    name <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- char '='
    _ <- space
    expression <- parseExpression DoesntNeedBrackets 4
    _ <- space
    return $
      mconcat
        [ documentation,
          if documentation == ""
            then ""
            else "\n",
          if signature == ""
            then ""
            else signature <> "\n",
          name,
          if parameters == ""
            then ""
            else " ",
          parameters,
          " =\n    ",
          expression
        ]

parseParameters :: Int -> Parser Text
parseParameters startColumn =
  do
    parameters <-
      many $
        do
          parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
          if parameterColumn <= startColumn
            then fail "invalid indentation"
            else do
              parameter <- parsePattern 0
              _ <- space
              return parameter
    return $ intercalate " " parameters

parseTypeSignature :: Parser Text
parseTypeSignature =
  do
    startColumn <- fmap sourceColumn getSourcePos
    name <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    types <- some $
      do
        column <- fmap sourceColumn getSourcePos
        if column <= startColumn
          then fail "invalid indentation"
          else do
            type_ <- parseType 0
            _ <- space
            _ <- choice [chunk "->", return ""]
            _ <- space
            return type_
    return $
      mconcat
        [ name,
          " : ",
          intercalate " -> " types
        ]

parseType :: Int -> Parser Text
parseType indent =
  choice
    [ parseTypeWithParameters,
      try $ parseEmptyRecord,
      parseRecordType indent,
      parseTupleType indent
    ]

parseTupleType :: Int -> Parser Text
parseTupleType indent =
  do
    _ <- char '('
    _ <- parseSpaces
    items <- many (parseTupleTypeItem indent)
    _ <- char ')'
    if null items
      then return "()"
      else
        return $
          mconcat
            [ "( ",
              intercalate ", " items,
              " )"
            ]

parseTupleTypeItem :: Int -> Parser Text
parseTupleTypeItem indent =
  do
    _ <- space
    type_ <- parseType indent
    _ <- space
    _ <- choice [char ',', lookAhead (try (char ')'))]
    return type_

parseRecordType :: Int -> Parser Text
parseRecordType indent =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineRecordType indent
      MultiLine ->
        parseMultiLineRecordType indent

parseMultiLineRecordType :: Int -> Parser Text
parseMultiLineRecordType indent =
  do
    _ <- char '{'
    _ <- space
    items <- many (parseRecordTypeItem (indent + 2))
    _ <- char '}'
    if null items
      then return "{}"
      else
        return $
          mconcat
            [ "{ ",
              intercalate ("\n" <> (pack $ take indent $ repeat ' ') <> ", ") items,
              "\n" <> (pack $ take indent $ repeat ' ') <> "}"
            ]

parseRecordTypeItemLininess :: Parser ContainerType
parseRecordTypeItemLininess =
  do
    _ <- parseName
    spaces1 <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    _ <- char ':'
    spaces2 <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    right <- parseType 0
    if "\n" `isInfixOf` (spaces1 <> spaces2 <> right)
      then return MultiLine
      else return SingleLine

parseSingleLineRecordType :: Int -> Parser Text
parseSingleLineRecordType indent =
  do
    _ <- char '{'
    _ <- parseSpaces
    items <- many (parseRecordTypeItem indent)
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

parseRecordTypeItem :: Int -> Parser Text
parseRecordTypeItem indent =
  do
    recordLininess <- lookAhead parseRecordTypeItemLininess
    name <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    right <- parseType indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- space
    _ <- choice [char ',', lookAhead (try (char '}'))]
    _ <- space
    return $
      mconcat
        [ name,
          " :",
          case recordLininess of
            SingleLine ->
              " "
            MultiLine ->
              "\n" <> pack (take (indent + 2) (repeat ' ')),
          right,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> pack (take (indent - 2) (repeat ' ')) <> commentAfter
        ]

parseTypeWithParameters :: Parser Text
parseTypeWithParameters =
  do
    startColumn <- fmap (unPos . sourceColumn) getSourcePos
    name <- parseName
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    parameters <- parseParameters startColumn
    if parameters == ""
      then return name
      else return $ name <> " " <> parameters

parseImport :: Parser Text
parseImport =
  do
    _ <- chunk "import"
    _ <- space
    name <- parseName
    _ <- space
    as_ <-
      choice
        [ do
            _ <- chunk "as"
            _ <- space
            parseName,
          return ""
        ]
    _ <- space
    exposing_ <-
      choice
        [ do
            _ <- chunk "exposing"
            _ <- space
            parseExposing,
          return ""
        ]
    return $
      mconcat
        [ "import ",
          name,
          if as_ == ""
            then ""
            else " as " <> as_,
          if exposing_ == ""
            then ""
            else " exposing" <> exposing_
        ]

parser :: Parser Text
parser =
  do
    moduleDeclaration <- parseModuleDeclaration
    _ <- space
    imports <-
      fmap (intercalate "\n" . List.sort) $
        many $
          do
            import_ <- parseImport
            _ <- space
            return import_
    _ <- space
    topLevelBinds <-
      some $
        choice
          [ try customTypeDeclaration,
            try typeAliasDeclaration,
            topLevelBind
          ]
    _ <- eof
    return $
      mconcat
        [ moduleDeclaration,
          if imports == ""
            then ""
            else "\n\n",
          imports,
          "\n\n\n",
          intercalate "\n\n\n" topLevelBinds,
          "\n"
        ]

parseExpression :: Context -> Int -> Parser Text
parseExpression context indent =
  choice
    [ parseCaseOf indent,
      parseTuple context indent,
      parseList indent,
      try $ parseRecord indent,
      parseRecordUpdate indent,
      parseIfThenElse indent,
      parseLetIn indent,
      try $ parseInfixed indent,
      try $ parseFunctionCall indent,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseAnonymousFunction indent
    ]

parseAnonymousFunction :: Int -> Parser Text
parseAnonymousFunction indent =
  do
    _ <- char '\\'
    pattern <- parsePattern indent
    _ <- space
    _ <- chunk "->"
    _ <- space
    body <- parseExpression DoesntNeedBrackets indent
    return $
      mconcat
        [ "\\",
          pattern,
          " -> ",
          body
        ]

parseRecord :: Int -> Parser Text
parseRecord indent =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineRecord indent
      MultiLine ->
        parseMultiLineRecord indent

parseMultiLineRecord :: Int -> Parser Text
parseMultiLineRecord indent =
  do
    _ <- char '{'
    _ <- space
    items <- many (parseRecordItem (indent + 2))
    _ <- char '}'
    if null items
      then return "{}"
      else
        return $
          mconcat
            [ "{ ",
              intercalate ("\n" <> (pack $ take indent $ repeat ' ') <> ", ") items,
              "\n" <> (pack $ take indent $ repeat ' ') <> "}"
            ]

parseRecordUpdate :: Int -> Parser Text
parseRecordUpdate indent =
  do
    lininess <- lookAhead parseListType
    case lininess of
      MultiLine ->
        parseMultiLineRecordUpdate indent
      SingleLine ->
        parseSingleLineRecordUpdate indent

parseSingleLineRecordUpdate :: Int -> Parser Text
parseSingleLineRecordUpdate indent =
  do
    _ <- char '{'
    _ <- parseSpaces
    recordName <- parseName
    _ <- parseSpaces
    _ <- char '|'
    _ <- parseSpaces
    items <- many (parseRecordItem indent)
    _ <- char '}'
    return $
      mconcat
        [ "{ ",
          recordName,
          " | ",
          intercalate ", " items,
          " }"
        ]

parseMultiLineRecordUpdate :: Int -> Parser Text
parseMultiLineRecordUpdate indent =
  do
    _ <- char '{'
    _ <- space
    recordName <- parseName
    _ <- space
    _ <- char '|'
    _ <- space
    items <- many (parseRecordItem indent)
    _ <- char '}'
    let spaces = "\n" <> pack (take (indent + 4) (repeat ' '))
    return $
      mconcat
        [ "{ ",
          recordName,
          spaces,
          "| ",
          intercalate (spaces <> ", ") items,
          "\n" <> pack (take indent (repeat ' ')),
          "}"
        ]

parseSingleLineRecord :: Int -> Parser Text
parseSingleLineRecord indent =
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
    recordLininess <- lookAhead parseRecordItemLininess
    name <- parseName
    _ <- space
    _ <- char '='
    _ <- space
    right <- parseExpression DoesntNeedBrackets indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- space
    _ <- choice [char ',', lookAhead (try (char '}'))]
    _ <- space
    return $
      mconcat
        [ name,
          " =",
          case recordLininess of
            SingleLine ->
              " "
            MultiLine ->
              "\n" <> pack (take (indent + 2) (repeat ' ')),
          right,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> pack (take (indent - 2) (repeat ' ')) <> commentAfter
        ]

parseRecordItemLininess :: Parser ContainerType
parseRecordItemLininess =
  do
    _ <- parseName
    spaces1 <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    _ <- char '='
    spaces2 <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    right <- parseExpression DoesntNeedBrackets 0
    if "\n" `isInfixOf` (spaces1 <> spaces2 <> right)
      then return MultiLine
      else return SingleLine

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
      chunk "\\u",
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
      chunk "\\u",
      chunk "\\n",
      chunk "\\\\"
    ]

parsePattern :: Int -> Parser Text
parsePattern indent =
  choice
    [ parseTuple NeedsBrackets indent,
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
          _ <- space
          column <- fmap (unPos . sourceColumn) getSourcePos
          if column - 1 > indent - 4
            then parseExpression NeedsBrackets indent
            else fail "argument is too far left"
    let spaces =
          case lininess of
            MultiLine ->
              pack $ '\n' : (take (indent + 4) $ repeat ' ')
            SingleLine ->
              " "
    return $ intercalate spaces (f : items)

parseInfix :: Parser Text
parseInfix =
  do
    column <- fmap (unPos . sourceColumn) getSourcePos
    if column == 0
      then fail "can't have an infix at column zero"
      else choice $ map chunk infixes

infixes :: [Text]
infixes =
  ["<|", "+", "|>"]

parseInfixedExpression :: Int -> Parser Text
parseInfixedExpression indent =
  choice
    [ parseTuple NeedsBrackets indent,
      parseList indent,
      try $ parseRecord indent,
      parseRecordUpdate indent,
      try $ parseFunctionCall indent,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseAnonymousFunction indent
    ]

parseInfixLininess :: Int -> Parser ContainerType
parseInfixLininess indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- parseInfixedExpression indent
    _ <- some $
      try $
        do
          _ <- space
          _ <- parseInfix
          _ <- space
          _ <- parseInfixedExpression indent
          return ()
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $
      if endRow > startRow
        then MultiLine
        else SingleLine

parseInfixed :: Int -> Parser Text
parseInfixed indent =
  do
    lininess <- lookAhead $ parseInfixLininess indent
    case lininess of
      MultiLine ->
        parseMultiLineInfixed indent
      SingleLine ->
        parseSingleLineInfixed indent

parseMultiLineInfixed :: Int -> Parser Text
parseMultiLineInfixed indent =
  do
    firstExpression <- parseInfixedExpression indent
    _ <- space

    items <- some $
      do
        infix_ <- parseInfix
        _ <- space
        expression <- parseInfixedExpression indent
        _ <- space
        return $ infix_ <> " " <> expression
    let between = "\n" <> pack (take (indent + 4) (repeat ' '))
    return $
      mconcat
        [ firstExpression,
          between,
          intercalate between items
        ]

parseSingleLineInfixed :: Int -> Parser Text
parseSingleLineInfixed indent =
  do
    firstExpression <- parseInfixedExpression indent
    _ <- takeWhileP Nothing (\ch -> ch == ' ')

    items <- some $
      do
        infix_ <- parseInfix
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        expression <- parseInfixedExpression indent
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        return $ infix_ <> " " <> expression
    return $
      mconcat
        [ firstExpression,
          " ",
          intercalate " " items
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

functionCallLininess :: Parser ContainerType
functionCallLininess =
  do
    _ <- parseName
    items <-
      some $
        try $
          do
            spaces <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
            argColumn <- fmap sourceColumn getSourcePos
            if unPos argColumn > 0
              then do
                _ <- parseExpression NeedsBrackets 8
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
    in_ <- parseExpression DoesntNeedBrackets indent
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
    left <- parseExpression DoesntNeedBrackets indent
    _ <- space
    _ <- char '='
    _ <- space
    right <- parseExpression DoesntNeedBrackets indent
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
    if_ <- parseExpression DoesntNeedBrackets (indent + 4)
    _ <- space1
    _ <- chunk "then"
    _ <- space1
    then_ <- parseExpression DoesntNeedBrackets (indent + 4)
    _ <- space1
    _ <- chunk "else"
    _ <- space1
    else_ <- parseExpression DoesntNeedBrackets (indent + 4)
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
    caseOf <- parseExpression DoesntNeedBrackets (indent + 4)
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
    right <- parseExpression DoesntNeedBrackets (indent + 4)
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
  Set.fromList $
    ["case", "of", "let", "in", "if", "then", "else", "->", "type"]

parseFirstNameChar :: Parser Char
parseFirstNameChar =
  token getFirstNameChar Set.empty

getFirstNameChar :: Char -> Maybe Char
getFirstNameChar ch =
  if ch `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz._" :: String)
    then Just ch
    else Nothing

parseName :: Parser Text
parseName =
  do
    firstChar <- parseFirstNameChar
    remainder <-
      takeWhileP
        (Just "name character")
        (\ch -> ch `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789" :: String))
    let word = Text.cons firstChar remainder
    if Set.member word keywords
      then fail $ "expecting a name tail but got: " <> unpack word
      else return word

parseVerbatim :: Parser Text
parseVerbatim =
  do
    word <-
      takeWhile1P
        (Just "verbatim character")
        (\ch -> ch `elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz." :: String))
    if Set.member word keywords
      then fail $ "expecting a verbatim, but got: " <> unpack word
      else return word

parseComment :: Parser Text
parseComment =
  do
    _ <- chunk "--"
    comment <- takeWhileP Nothing (\ch -> ch /= '\n')
    _ <- char '\n'
    return ("--" <> comment)

commentSpaceParser :: Int -> Parser Text
commentSpaceParser indent =
  do
    comments <-
      many $
        choice
          [ parseComment,
            do
              _ <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
              return ""
          ]
    return $
      intercalate
        ("\n" <> pack (take indent (repeat ' ')))
        (filter (\s -> s /= "") comments)

parseSameLineComment :: Parser Text
parseSameLineComment =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- chunk "--"
    comment <- takeWhileP Nothing (\ch -> ch /= '\n')
    _ <- char '\n'
    return ("--" <> comment)

parseMultiListItem :: Int -> Char -> Parser Text
parseMultiListItem indent end =
  do
    commentBefore <- commentSpaceParser indent
    expression <- parseExpression DoesntNeedBrackets indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- choice [char ',', lookAhead (try (char end))]
    return $
      mconcat
        [ if commentBefore == ""
            then ""
            else pack (take indent (repeat ' ')) <> commentBefore,
          expression,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> pack (take (indent - 2) (repeat ' ')) <> commentAfter
        ]

parseListItem :: Int -> Parser () -> Char -> Parser Text
parseListItem indent spaceParser end =
  do
    _ <- spaceParser
    expression <- parseExpression DoesntNeedBrackets indent
    _ <- spaceParser
    _ <- choice [char ',', lookAhead (try (char end))]
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

parseTuple :: Context -> Int -> Parser Text
parseTuple context indent =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineTuple context indent
      MultiLine ->
        parseMultiLineTuple indent

parseMultiLineTuple :: Int -> Parser Text
parseMultiLineTuple indent =
  do
    _ <- char '('
    _ <- space
    items <- many (parseMultiListItem (indent + 2) ')')
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

data Context
  = NeedsBrackets
  | DoesntNeedBrackets
  deriving (Eq)

parseSingleLineTuple :: Context -> Int -> Parser Text
parseSingleLineTuple context indent =
  do
    _ <- char '('
    _ <- parseSpaces
    items <- many (parseListItem indent parseSpaces ')')
    _ <- char ')'
    if null items
      then return "()"
      else
        if length items == 1
          then case context of
            NeedsBrackets ->
              return $ mconcat ["(", head items, ")"]
            DoesntNeedBrackets ->
              return $ head items
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
    comment <- commentSpaceParser (indent + 2)
    items <- many (parseMultiListItem (indent + 2) ']')
    _ <- char ']'
    if null items
      then
        return $
          if comment == ""
            then "[]"
            else "[" <> comment <> "\n" <> pack (take indent (repeat ' ')) <> "]"
      else
        return $
          mconcat
            [ "[ ",
              if comment == ""
                then ""
                else comment <> "\n" <> (pack $ take indent $ repeat ' ') <> "  ",
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

parseEmptyRecord :: Parser Text
parseEmptyRecord =
  do
    _ <- char '{'
    _ <- space
    _ <- char '}'
    return "{}"

parseSpaces :: Parser ()
parseSpaces =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return ()
