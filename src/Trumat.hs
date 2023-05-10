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
    takeP,
    takeWhile1P,
    takeWhileP,
    token,
    try,
    unPos,
  )
import Text.Megaparsec.Char (char, space)
import Prelude
  ( Bool,
    Char,
    Either (..),
    Eq,
    Int,
    Maybe (..),
    Show,
    String,
    div,
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
    (*),
    (+),
    (-),
    (.),
    (/=),
    (<),
    (<=),
    (<>),
    (==),
    (>),
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
    name <- choice [parseName, parseInfixInBrackets]
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
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    contents <-
      many $
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '-'),
            try $ do
              _ <- char '-'
              _ <- notFollowedBy (char '}')
              return ""
          ]
    _ <- chunk "-}"
    return $ Text.strip $ mconcat contents

parseModuleDocs :: Parser Text
parseModuleDocs =
  do
    _ <- chunk "{-|"
    docRows <- many $ try parseExportDocsRow
    remainder <- endDocComment
    return $
      mconcat
        [ "{-|",
          if remainder == "" && null docRows
            then "\n\n\n"
            else
              if null docRows
                then " " <> remainder <> "\n"
                else
                  mconcat
                    [ "\n\n",
                      intercalate "\n" (map (\row -> "@docs " <> intercalate ", " row) docRows),
                      "\n\n",
                      if remainder == ""
                        then ""
                        else remainder <> "\n\n"
                    ],
          "-}"
        ]

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
    _ <- space
    title <- choice [parseSectionComment, return ""]
    return $
      mconcat
        [ "module ",
          name,
          " exposing",
          exports,
          if moduleDocs == ""
            then ""
            else "\n\n" <> moduleDocs,
          if title == ""
            then ""
            else "\n" <> title
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
    type_ <- parseAliasedType 4
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
    documentation <- choice [try $ parseDocumentation, return ""]
    _ <- space
    _ <- "type"
    _ <- space
    name <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- space
    _ <- char '='
    _ <- space
    branches <- some parseBranch
    return $
      mconcat
        [ documentation,
          if documentation == ""
            then ""
            else "\n",
          "type ",
          name,
          if parameters == ""
            then ""
            else " ",
          parameters,
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
        if startColumn == 1
          then fail "column is too low"
          else do
            branchName <- parseName
            _ <- space
            parameters <- parseTypeParameters startColumn
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
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '-'),
            try $ do
              _ <- char '-'
              _ <- notFollowedBy (char '}')
              return "-"
          ]
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
    documentation <- try $ choice [try $ parseDocumentation, return ""]
    _ <- space
    signature <- choice [try $ parseTypeSignature, return ""]
    _ <- space
    name <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- char '='
    commentBeforeExpression <- commentSpaceParser 4
    expression <- parseExpression 0 2 DoesntNeedBrackets 4
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
          commentBeforeExpression,
          if commentBeforeExpression == ""
            then ""
            else "\n    ",
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
              parameter <- parsePattern startColumn 0
              _ <- space
              return parameter
    return $ intercalate " " parameters

parseTypeSignature :: Parser Text
parseTypeSignature =
  do
    startColumn <- fmap (unPos . sourceColumn) getSourcePos
    name <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    types <- some $
      do
        column <- fmap (unPos . sourceColumn) getSourcePos
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
      try parseFunctionType,
      parseTupleType indent
    ]

parseAliasedType :: Int -> Parser Text
parseAliasedType indent =
  choice
    [ try $ parseBareFunctionType 1 indent,
      parseTypeWithParameters,
      try $ parseEmptyRecord,
      parseRecordType indent,
      parseTupleType indent
    ]

parseBareFunctionType :: Int -> Int -> Parser Text
parseBareFunctionType minColumn indent =
  do
    types <- some $
      do
        column <- fmap (unPos . sourceColumn) getSourcePos
        if column <= minColumn
          then fail "too far left"
          else do
            type_ <- parseType indent
            _ <- space
            _ <- choice [chunk "->", return ""]
            _ <- space
            return type_
    return $ intercalate " -> " types

parseFunctionType :: Parser Text
parseFunctionType =
  do
    _ <- char '('
    types <- some $
      do
        type_ <- parseType 0
        _ <- space
        _ <- choice [chunk "->", return ""]
        _ <- space
        return type_
    _ <- char ')'
    return $ "(" <> intercalate " -> " types <> ")"

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
    parameters <- parseTypeParameters startColumn
    if parameters == ""
      then return name
      else return $ name <> " " <> parameters

parseTypeParameters :: Int -> Parser Text
parseTypeParameters startColumn =
  do
    parameters <-
      many $
        do
          parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
          if parameterColumn <= startColumn
            then fail "invalid indentation"
            else do
              parameter <- parseTypeParameter 0
              _ <- space
              return parameter
    return $ intercalate " " parameters

parseImport :: Parser Text
parseImport =
  do
    _ <- chunk "import"
    _ <- space1
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
            import_ <- try parseImport
            _ <- space
            return import_
    _ <- space
    topLevelBinds <-
      some $
        choice
          [ try typeAliasDeclaration,
            try customTypeDeclaration,
            parseSectionComment,
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

parseSectionComment :: Parser Text
parseSectionComment =
  do
    comment <- parseComment
    _ <- space
    return $ "\n" <> comment

notFollowedByInfix :: Parser Text -> Parser Text
notFollowedByInfix p =
  do
    item <- p
    _ <- lookAhead $
      do
        _ <- space
        notFollowedBy parseInfix
    return item

parseExpression :: Int -> Int -> Context -> Int -> Parser Text
parseExpression nesting minColumn context indent =
  choice
    [ try $ parseCaseOf indent,
      parseList nesting indent,
      try $ parseIfThenElse minColumn indent,
      try $ parseLetIn minColumn indent,
      try $ notFollowedByInfix $ parseRecord nesting indent,
      try $ notFollowedByInfix $ parseRecordUpdate indent,
      try $ notFollowedByInfix (parseFunctionCall minColumn indent),
      try $ parseInfixed minColumn indent,
      try parseInfixInBrackets,
      try $ parseTuple nesting context indent,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseCharLiteral,
      parseAnonymousFunction minColumn indent
    ]

parseAnonymousFunction :: Int -> Int -> Parser Text
parseAnonymousFunction minColumn indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '\\'
    pattern <- parseParameters minColumn
    _ <- space
    _ <- chunk "->"
    _ <- space
    body <- parseExpression 0 minColumn DoesntNeedBrackets (indent + 4)
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $
      mconcat
        [ "\\",
          pattern,
          " ->",
          if endRow > startRow
            then "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
            else " ",
          body
        ]

parseRecord :: Int -> Int -> Parser Text
parseRecord nesting indent =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineRecord indent
      MultiLine ->
        parseMultiLineRecord nesting indent

parseMultiLineRecord :: Int -> Int -> Parser Text
parseMultiLineRecord nesting indent =
  do
    _ <- char '{'
    _ <- space
    items <- many (parseRecordItem nesting indent)
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
    items <- many (parseRecordItem 0 indent)
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
    items <- many (parseRecordItem 0 indent)
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
    items <- many (parseRecordItem 0 indent)
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

parseRecordItem :: Int -> Int -> Parser Text
parseRecordItem nesting indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    name <- parseName
    _ <- space
    _ <- char '='
    _ <- space
    right <- parseExpression 0 1 DoesntNeedBrackets (indent + 4)
    endRow <- fmap (unPos . sourceLine) getSourcePos
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- space
    _ <- choice [char ',', lookAhead (try (char '}'))]
    _ <- space
    return $
      mconcat
        [ name,
          " =",
          if endRow > startRow
            then "\n" <> pack (take (indent + 4) (repeat ' '))
            else " ",
          right,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> pack (take indent (repeat ' ')) <> commentAfter
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
      chunk "\\u",
      chunk "\\n",
      try $ do
        _ <- char '"'
        _ <- notFollowedBy (char '"')
        return "\"",
      try $ do
        _ <- chunk "\"\""
        _ <- notFollowedBy (char '"')
        return "\"\""
    ]

parseSimpleStringLiteral :: Parser Text
parseSimpleStringLiteral =
  do
    _ <- char '"'
    contents <- many parseSimpleStringLiteralChar
    _ <- char '"'
    return $ mconcat ["\"", mconcat contents, "\""]

parseCharLiteral :: Parser Text
parseCharLiteral =
  do
    _ <- char '\''
    contents <-
      choice
        [ fmap Text.singleton $ token charLiteralMatcher Set.empty,
          chunk "\\n",
          chunk "\\t",
          chunk "\\'",
          chunk "\\\\",
          do
            _ <- chunk "\\u{"
            codePoint <- takeP Nothing 4
            _ <- char '}'
            return $ "\\u{" <> codePoint <> "}"
        ]
    _ <- char '\''
    return $ "'" <> contents <> "'"

charLiteralMatcher :: Char -> Maybe Char
charLiteralMatcher ch =
  if ch == '\'' || ch == '\\'
    then Nothing
    else Just ch

parseSimpleStringLiteralChar :: Parser Text
parseSimpleStringLiteralChar =
  choice
    [ takeWhile1P Nothing (\ch -> ch /= '"' && ch /= '\\'),
      chunk "\\\"",
      chunk "\\u",
      chunk "\\n",
      chunk "\\t",
      chunk "\\\\"
    ]

parsePattern :: Int -> Int -> Parser Text
parsePattern minColumn indent =
  choice
    [ try $ parsePatternNoAlias minColumn indent,
      parseAliasedPattern minColumn indent
    ]

parseTypeParameter :: Int -> Parser Text
parseTypeParameter indent =
  choice
    [ try parseFunctionType,
      try $ parseTuple 0 NeedsBrackets indent,
      parseList 0 indent,
      parseRecordType indent,
      try parseFunctionCallPattern,
      parseVerbatim,
      parseSimpleStringLiteral
    ]

parseAliasedPattern :: Int -> Int -> Parser Text
parseAliasedPattern minColumn indent =
  do
    _ <- char '('
    pattern <- parsePatternNoAlias minColumn indent
    _ <- space
    _ <- chunk "as"
    _ <- space
    name <- parseName
    _ <- space
    _ <- char ')'
    return $
      mconcat
        [ "(",
          pattern,
          " as ",
          name,
          ")"
        ]

parseRecordPattern :: Parser Text
parseRecordPattern =
  do
    _ <- char '{'
    _ <- space
    items <- many parseRecordPatternItem
    _ <- char '}'
    if null items
      then return "{}"
      else return $ mconcat ["{ ", intercalate ", " items, " }"]

parseRecordPatternItem :: Parser Text
parseRecordPatternItem =
  do
    _ <- space
    name <- parseName
    _ <- space
    _ <- choice [char ',', lookAhead (try (char '}'))]
    return name

parsePatternNoAlias :: Int -> Int -> Parser Text
parsePatternNoAlias minColumn indent =
  choice
    [ try $ parseConsPattern minColumn indent,
      try $ parseTuplePattern NeedsBrackets indent,
      parseList 0 indent,
      parseRecordPattern,
      try parseFunctionCallPattern,
      parseVerbatim,
      parseSimpleStringLiteral
    ]

parsePatternInsideConsPattern :: Int -> Int -> Parser Text
parsePatternInsideConsPattern minColumn indent =
  choice
    [ try $ parseTuple 0 NeedsBrackets indent,
      parseAliasedPattern minColumn indent,
      parseList 0 indent,
      try $ parseFunctionCall minColumn indent,
      parseVerbatim
    ]

parseConsPattern :: Int -> Int -> Parser Text
parseConsPattern minColumn indent =
  do
    left <- parsePatternInsideConsPattern minColumn indent
    _ <- space
    _ <- chunk "::"
    _ <- space
    right <- parsePatternInsideConsPattern minColumn indent
    return $ left <> " :: " <> right

parseArgumentExpression :: Int -> Parser Text
parseArgumentExpression indent =
  choice
    [ try $ parseTuple 0 NeedsBrackets indent,
      parseInfixInBrackets,
      parseList 0 indent,
      try $ parseRecord 0 indent,
      parseRecordUpdate indent,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseCharLiteral
    ]

parseCallable :: Int -> Int -> Parser Text
parseCallable minColumn indent =
  choice
    [ try $ parseAnonymousFunctionInParenthesis minColumn indent,
      parseInfixInBrackets,
      parseName
    ]

parseAnonymousFunctionInParenthesis :: Int -> Int -> Parser Text
parseAnonymousFunctionInParenthesis minColumn indent =
  do
    _ <- char '('
    _ <- space
    f <- parseAnonymousFunction minColumn indent
    _ <- space
    _ <- char ')'
    return $ "(" <> f <> ")"

parseInfixInBrackets :: Parser Text
parseInfixInBrackets =
  do
    _ <- char '('
    _ <- space
    infix_ <- parseInfix
    _ <- space
    _ <- char ')'
    return $ "(" <> infix_ <> ")"

parseFunctionCall :: Int -> Int -> Parser Text
parseFunctionCall minColumn indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    startColumn <- fmap (unPos . sourceColumn) getSourcePos
    if startColumn <= minColumn
      then fail "callable is too far left"
      else do
        f <- parseCallable minColumn indent
        items <- some $
          try $
            do
              _ <- space
              column <- fmap (unPos . sourceColumn) getSourcePos
              if column <= minColumn
                then fail "argument is too far left"
                else do
                  arg <- parseArgumentExpression (indent + 4)
                  argEndRow <- fmap (unPos . sourceLine) getSourcePos
                  return (arg, argEndRow)
        endRow <- fmap (unPos . sourceLine) getSourcePos
        return $ mconcat $ f : map (addArgSpaces startRow endRow indent) items

addArgSpaces :: Int -> Int -> Int -> (Text, Int) -> Text
addArgSpaces startRow endRow indent (arg, row) =
  if row == startRow
    then " " <> arg
    else
      if endRow > startRow
        then (pack $ '\n' : (take (floorToFour (indent + 4)) $ repeat ' ')) <> arg
        else " " <> arg

floorToFour :: Int -> Int
floorToFour i =
  4 * (i `div` 4)

parseFunctionCallPattern :: Parser Text
parseFunctionCallPattern =
  do
    startColumn <- fmap (unPos . sourceColumn) getSourcePos
    f <- parseName
    items <- some $
      try $
        do
          _ <- space
          endColumn <- fmap (unPos . sourceColumn) getSourcePos
          if endColumn < startColumn
            then fail "column too low"
            else parsePatternNoAlias 1 1
    return $ intercalate " " (f : items)

parseInfix :: Parser Text
parseInfix =
  do
    column <- fmap (unPos . sourceColumn) getSourcePos
    infix_ <-
      if column == 0
        then fail "can't have an infix at column zero"
        else choice $ map chunk infixes
    _ <- lookAhead $ try $ afterInfixChar
    return infix_

afterInfixChar :: Parser Char
afterInfixChar =
  choice $ map char ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789()[] \n" :: String)

infixes :: [Text]
infixes =
  ["==", "&&", ">>", "||", "<|", "|=", "++", "+", "|>", "|.", "::", ">", "<", "/=", "-", ".", "*", "^"]

parseInfixedExpression :: Int -> Int -> Parser Text
parseInfixedExpression minColumn indent =
  choice
    [ try $ parseCaseOf indent,
      try $ parseLetIn minColumn indent,
      try $ parseTuple 0 NeedsBrackets indent,
      parseList 0 indent,
      try parseEmptyRecord,
      try $ parseRecord 0 indent,
      parseRecordUpdate indent,
      try $ parseFunctionCall minColumn indent,
      try parseInfixInBrackets,
      parseCharLiteral,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseAnonymousFunction minColumn indent
    ]

parseInfixed :: Int -> Int -> Parser Text
parseInfixed minColumn indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    firstExpression <- parseInfixedExpression minColumn indent

    items <- some $
      try $
        do
          comment <- commentSpaceParser (indent + 4)
          infix_ <- parseInfix
          _ <- space
          expression <- parseInfixedExpression minColumn (floorToFour (indent + 4))
          return (comment, infix_, expression)
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $
      mconcat
        [ firstExpression,
          mconcat $ map (addInfixWhitespace (endRow > startRow) indent) items
        ]

addInfixWhitespace :: Bool -> Int -> (Text, Text, Text) -> Text
addInfixWhitespace isMultiline indent (comment, infix_, expression) =
  let newIndent = floorToFour (indent + 4)
   in if isMultiline
        then
          if infix_ == "<|"
            then
              mconcat
                [ " <|\n",
                  pack (take newIndent (repeat ' ')),
                  expression
                ]
            else
              mconcat
                [ "\n" <> pack (take newIndent (repeat ' ')),
                  comment,
                  if comment == ""
                    then ""
                    else "\n" <> pack (take newIndent (repeat ' ')),
                  infix_,
                  " ",
                  expression
                ]
        else
          if infix_ == "."
            then infix_ <> expression
            else " " <> infix_ <> " " <> expression

space1 :: Parser ()
space1 =
  do
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    return ()

parseLetIn :: Int -> Int -> Parser Text
parseLetIn minColumn indent =
  do
    _ <- chunk "let"
    _ <- space1
    column <- fmap (unPos . sourceColumn) getSourcePos
    let_ <- some $
      try $
        do
          items <- parseLetBind (column + 1) (indent + 4)
          _ <- space
          return items
    _ <- chunk "in"
    _ <- space
    in_ <- parseExpression 0 minColumn DoesntNeedBrackets indent
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

parseLetBind :: Int -> Int -> Parser Text
parseLetBind minColumn indent =
  do
    signature <- choice [try parseTypeSignature, return ""]
    left <- parseExpression 0 1 DoesntNeedBrackets indent
    _ <- space
    _ <- char '='
    _ <- space
    right <- parseExpression 0 minColumn DoesntNeedBrackets indent
    let leftSpaces = pack $ take indent $ repeat ' '
    let rightSpaces = pack $ take (indent + 4) $ repeat ' '
    return $
      mconcat
        [ if signature == ""
            then ""
            else leftSpaces <> signature <> "\n",
          leftSpaces,
          left,
          " =\n",
          rightSpaces,
          right
        ]

parseIfThenElse :: Int -> Int -> Parser Text
parseIfThenElse minColumn indent =
  do
    _ <- chunk "if"
    _ <- space1
    if_ <- parseExpression 0 minColumn DoesntNeedBrackets (indent + 4)
    _ <- space
    _ <- chunk "then"
    _ <- space1
    then_ <- parseExpression 0 minColumn DoesntNeedBrackets (indent + 4)
    _ <- space
    _ <- chunk "else"
    _ <- space1
    commentAfterElse <- commentSpaceParser (indent + 4)
    else_ <- parseExpression 0 minColumn DoesntNeedBrackets (indent + 4)
    return $
      mconcat
        [ "if",
          if Text.elem '\n' if_
            then "\n" <> pack (take (indent + 4) (repeat ' '))
            else " ",
          if_,
          if Text.elem '\n' if_
            then "\n" <> pack (take indent (repeat ' '))
            else " ",
          "then\n" <> pack (take (indent + 4) (repeat ' ')),
          then_,
          "\n\n" <> pack (take indent (repeat ' ')),
          "else\n" <> pack (take (indent + 4) (repeat ' ')),
          commentAfterElse,
          if commentAfterElse == ""
            then ""
            else "\n" <> pack (take (indent + 4) (repeat ' ')),
          else_
        ]

parseCaseOf :: Int -> Parser Text
parseCaseOf indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- chunk "case"
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    caseOf <- parseExpression 0 1 DoesntNeedBrackets (indent + 4)
    _ <- space
    _ <- chunk "of"
    endRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    column <- fmap (unPos . sourceColumn) getSourcePos
    branches <- some (parseCaseOfBranch column (indent + 4))
    return $
      mconcat
        [ "case",
          if endRow > startRow
            then "\n" <> pack (take (indent + 4) (repeat ' '))
            else " ",
          caseOf,
          if endRow > startRow
            then "\n" <> pack (take indent (repeat ' ') <> "of\n")
            else " of\n",
          intercalate "\n\n" branches
        ]

parseCaseOfBranch :: Int -> Int -> Parser Text
parseCaseOfBranch minColumn indent =
  do
    column <- fmap (unPos . sourceColumn) getSourcePos
    if column /= minColumn
      then fail "invalid column"
      else do
        left <- parsePattern (minColumn - 4) indent
        _ <- space
        _ <- chunk "->"
        _ <- space
        comment <- commentSpaceParser (indent + 4)
        right <- parseExpression 0 (minColumn + 1) DoesntNeedBrackets (indent + 4)
        _ <- space
        return $
          mconcat
            [ pack $ take indent $ repeat ' ',
              left,
              " ->\n",
              if comment == ""
                then ""
                else pack $ take (indent + 4) $ repeat ' ',
              comment,
              if comment == ""
                then ""
                else "\n",
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
        (\ch -> ch `elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz._" :: String))
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

parseTuplePatternItem :: Int -> Parser Text
parseTuplePatternItem indent =
  do
    commentBefore <- commentSpaceParser indent
    expression <- parsePattern 1 indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- choice [char ',', lookAhead (try (char ')'))]
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

parseMultiListItem :: Int -> Int -> Char -> Parser Text
parseMultiListItem nesting indent end =
  do
    commentBefore <- commentSpaceParser indent
    expression <- parseExpression nesting 1 DoesntNeedBrackets indent
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
            else "\n\n" <> pack (take indent (repeat ' ')) <> commentAfter
        ]

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

parseTuplePattern :: Context -> Int -> Parser Text
parseTuplePattern context indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- many (parseTuplePatternItem (indent + 2))
    _ <- char ')'
    endRow <- fmap (unPos . sourceLine) getSourcePos
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
            if endRow > startRow
              then
                return $
                  mconcat
                    [ "( ",
                      intercalate ("\n" <> (pack $ take indent $ repeat ' ') <> ", ") items,
                      "\n" <> (pack $ take indent $ repeat ' ') <> ")"
                    ]
              else
                return $
                  mconcat
                    [ "( ",
                      intercalate ", " items,
                      " )"
                    ]

parseTuple :: Int -> Context -> Int -> Parser Text
parseTuple nesting context indent =
  choice
    [ try $ parseParenthesised context indent,
      parseMultiTuple nesting indent
    ]

parseParenthesised :: Context -> Int -> Parser Text
parseParenthesised context indent =
  do
    startLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    item <- parseExpression 0 1 DoesntNeedBrackets indent
    _ <- space
    _ <- char ')'
    endLine <- fmap (unPos . sourceLine) getSourcePos
    if endLine > startLine
      then case context of
        NeedsBrackets ->
          return $
            mconcat
              [ "(",
                item,
                "\n" <> (pack $ take indent $ repeat ' ') <> ")"
              ]
        DoesntNeedBrackets ->
          return item
      else case context of
        NeedsBrackets ->
          return $ "(" <> item <> ")"
        DoesntNeedBrackets ->
          return item

parseMultiTuple :: Int -> Int -> Parser Text
parseMultiTuple nesting indent =
  do
    startLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- many (parseMultiListItem (nesting + 1) (indent + 2) ')')
    _ <- char ')'
    endLine <- fmap (unPos . sourceLine) getSourcePos
    if null items
      then return "()"
      else
        if endLine > startLine
          then
            return $
              mconcat
                [ "( ",
                  intercalate ("\n" <> (pack $ take indent $ repeat ' ') <> ", ") items,
                  "\n" <> (pack $ take indent $ repeat ' ') <> ")"
                ]
          else
            return $
              mconcat
                [ "( ",
                  intercalate ", " items,
                  " )"
                ]

data Context
  = NeedsBrackets
  | DoesntNeedBrackets
  deriving (Eq)

parseList :: Int -> Int -> Parser Text
parseList nesting indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '['
    comment <- commentSpaceParser (indent + 2)
    items <- many (parseMultiListItem (nesting + 1) indent ']')
    _ <- char ']'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    let nestingSpace =
          if endRow > startRow
            then mconcat (take nesting (repeat "  "))
            else ""
    let indentation =
          if endRow > startRow
            then "\n" <> (pack $ take indent $ repeat ' ')
            else ""
    let endSpace =
          if endRow > startRow
            then ""
            else " "
    if null items
      then
        return $
          if comment == ""
            then "[]"
            else "[" <> comment <> "\n" <> pack (take indent (repeat ' ')) <> nestingSpace <> "]"
      else
        return $
          mconcat
            [ "[ ",
              if comment == ""
                then ""
                else comment <> indentation <> "  ",
              intercalate (indentation <> nestingSpace <> ", ") items,
              indentation <> endSpace <> nestingSpace <> "]"
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
