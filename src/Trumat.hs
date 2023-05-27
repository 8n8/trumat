module Trumat (trumat) where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, intercalate, isInfixOf, pack, takeEnd, unpack)
import qualified Data.Text as Text
import Data.Void (Void)
import Debug.Trace (trace)
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
import Text.Megaparsec.Debug (dbg)
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
    fst,
    head,
    length,
    map,
    max,
    mconcat,
    not,
    null,
    repeat,
    return,
    reverse,
    show,
    snd,
    take,
    zip,
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

parseExportDocs :: Parser [[Text]]
parseExportDocs =
  do
    _ <- consumeExportList
    _ <- space
    _ <- chunk "{-|"
    results <- many $ try parseExportDocsRowOnly
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

parseDocRow :: Parser Text
parseDocRow =
  choice
    [ chunk "\n",
      do
        _ <- chunk "    "
        code <- takeWhileP Nothing (\ch -> ch /= '\n')
        _ <- char '\n'
        return $ "    " <> code <> "\n",
      try $ do
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        docs <- parseExportDocsRow
        _ <- char '\n'
        return $ "@docs " <> intercalate ", " docs <> "\n",
      do
        pieces <-
          some $
            do
              text <- takeWhile1P Nothing (\ch -> ch /= '@' && ch /= '-' && ch /= '\n')
              _ <- notFollowedBy $ lookAhead $ chunk "-}"
              choice
                [ do
                    _ <- char '-'
                    return $ text <> "-",
                  return text
                ]
        return $
          if Text.strip (mconcat pieces) == "-"
            then ""
            else mconcat pieces
    ]

parseOtherDocRow :: Parser Text
parseOtherDocRow =
  choice
    [ do
        _ <- chunk "\n    "
        code <- takeWhileP Nothing (\ch -> ch /= '\n')
        return $ "\n    " <> code,
      do
        text <- takeWhile1P Nothing (\ch -> ch /= '@' && ch /= '-')
        _ <- notFollowedBy $ lookAhead $ chunk "-}"
        if Text.strip text == ""
          then return ""
          else return $ Text.strip text,
      do
        _ <- space1
        return "",
      do
        _ <- char '-'
        _ <- notFollowedBy $ lookAhead $ char '}'
        return ""
    ]

parseExportDocsRowOnly :: Parser [Text]
parseExportDocsRowOnly =
  do
    _ <-
      some $
        choice
          [ chunk "\n",
            do
              _ <- chunk "    "
              code <- takeWhileP Nothing (\ch -> ch /= '\n')
              _ <- char '\n'
              return $ "    " <> Text.strip code <> "\n",
            do
              pieces <-
                some $
                  do
                    text <- takeWhile1P Nothing (\ch -> ch /= '@' && ch /= '-' && ch /= '\n')
                    _ <- notFollowedBy $ lookAhead $ chunk "-}"
                    _ <- choice [char '-', return ' ']
                    return text
              return $ mconcat pieces
          ]
    _ <- chunk "@docs"
    _ <- takeWhile1P Nothing (\ch -> ch == ' ')
    some parseOneExportDoc

parseExportDocsRow :: Parser [Text]
parseExportDocsRow =
  do
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

parseExposing :: Int -> [[Text]] -> Parser Text
parseExposing indent docs =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- some parseExport
    _ <- space
    _ <- char ')'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $ formatExports indent (endRow > startRow) docs items

formatExportRow :: [Text] -> [Text] -> Text
formatExportRow items docRow =
  let documented = filter (\doc -> elem doc items) docRow
   in intercalate ", " documented

log :: (Show a) => String -> a -> a
log description value =
  trace (description <> ": " <> show value) value

formatExports :: Int -> Bool -> [[Text]] -> [Text] -> Text
formatExports indent originalIsMultiline docs items =
  let unformattedRows = removeUndocumented items docs
      rows = filter (\row -> row /= "") $ (map (formatExportRow items) unformattedRows) <> undocumented
      undocumented = getUndocumented docs items
      isMultiline = (not (null (removeUndocumented items docs)) && length items > 1) || originalIsMultiline
   in case rows of
        [] ->
          "()"
        [single] ->
          " (" <> single <> ")"
        multiple ->
          mconcat
            [ if isMultiline
                then "\n" <> pack (take indent (repeat ' ')) <> "( "
                else " (",
              intercalate
                ( if isMultiline
                    then "\n" <> pack (take indent (repeat ' ')) <> ", "
                    else ", "
                )
                multiple,
              if isMultiline
                then "\n" <> pack (take indent (repeat ' '))
                else "",
              ")"
            ]

removeUndocumented :: [Text] -> [[Text]] -> [[Text]]
removeUndocumented used docs =
  let docsWithExposeAll = addExposeAllToDocs used docs
      usedDocs = removeUnusedDocs used docsWithExposeAll
   in removeEmptyLists usedDocs

removeUnusedDocs :: [Text] -> [[Text]] -> [[Text]]
removeUnusedDocs used docs =
  map (\docRow -> filter (\doc -> doc `elem` used) docRow) docs

addExposeAllToDocs :: [Text] -> [[Text]] -> [[Text]]
addExposeAllToDocs used docs =
  let hasExposeAll = makeHasExposeAll used
   in map
        (map (\doc -> if Set.member doc hasExposeAll then doc <> "(..)" else doc))
        docs

makeHasExposeAll :: [Text] -> Set Text
makeHasExposeAll names =
  Set.fromList $
    map (\name -> Text.dropEnd 4 name) $
      filter (\name -> Text.takeEnd 4 name == "(..)") names

--   removeEmptyLists $
--     map (\docsRow -> filter (\docsItem -> docsItem `elem` (map trimExposeAll used)) docsRow) $
--        used

removeEmptyLists :: [[a]] -> [[a]]
removeEmptyLists items =
  filter (\item -> not (null item)) items

trimExposeAll :: Text -> Text
trimExposeAll text =
  if takeEnd 4 text == "(..)"
    then Text.dropEnd 4 text
    else text

getUndocumented :: [[Text]] -> [Text] -> [Text]
getUndocumented docs items =
  let docSet = Set.fromList $ mconcat docs
   in filter (\item -> not (Set.member (trimExposeAll item) docSet)) items

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
    rows <- many parseDocRow
    _ <- chunk "-}"
    return $
      mconcat
        [ "{-|",
          if Text.strip (mconcat rows) == ""
            then "\n\n\n"
            else mconcat rows,
          "-}"
        ]

parseModuleDeclaration :: Parser Text
parseModuleDeclaration =
  do
    port <-
      choice
        [ do
            p <- chunk "port"
            _ <- space
            return ("port " :: Text),
          return ""
        ]
    _ <- chunk "module "
    name <- parseName
    _ <- chunk " exposing"
    _ <- space
    docs <-
      choice
        [ lookAhead $ try parseExportDocs,
          return []
        ]

    exports <- parseExposing 4 docs
    _ <- space
    moduleDocs <- choice [parseModuleDocs, return ""]
    _ <- space
    title <- choice [parseSectionComment, return ""]
    return $
      mconcat
        [ port,
          "module ",
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
    _ <- space
    documentation <- choice [parseDocumentation, return ""]
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
    comment <- commentSpaceParser 4
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
          comment,
          if comment == "" then "" else "\n    ",
          type_
        ]

customTypeDeclaration :: Parser Text
customTypeDeclaration =
  do
    _ <- space
    documentation <- choice [parseDocumentation, return ""]
    _ <- space
    _ <- "type"
    _ <- space
    name <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- space
    firstBranch <- parseBranch '='
    nextBranches <- many (parseBranch '|')
    let branches = firstBranch : nextBranches
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

parseBranch :: Char -> Parser Text
parseBranch startChar =
  do
    _ <- char startChar
    _ <- space
    column <- fmap sourceColumn getSourcePos
    if unPos column == 0
      then fail "column == 0"
      else try $ do
        startRow <- fmap (unPos . sourceLine) getSourcePos
        commentBefore <- commentSpaceParser 6
        branchName <- parseName
        afterNameRow <- fmap (unPos . sourceLine) getSourcePos
        startColumn <- fmap (unPos . sourceColumn) getSourcePos
        if startColumn == 1
          then fail "column is too low"
          else do
            _ <- space
            parameters <- parseTypeDeclarationParameters 2
            endRow <- fmap (unPos . sourceLine) getSourcePos
            _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
            afterEmptySpaceRow <- fmap (unPos . sourceLine) getSourcePos
            commentAfter <- commentSpaceParser 1
            afterCommentRow <- fmap (unPos . sourceLine) getSourcePos
            return $
              mconcat
                [ commentBefore,
                  if commentBefore == ""
                    then ""
                    else "\n      ",
                  branchName,
                  if parameters == ""
                    then ""
                    else
                      if endRow > startRow
                        then "\n        "
                        else " ",
                  parameters,
                  if commentAfter == ""
                    then ""
                    else
                      ( if afterEmptySpaceRow == afterNameRow
                          then " "
                          else "\n      "
                      )
                        <> commentAfter
                ]

parseDocumentation :: Parser Text
parseDocumentation =
  do
    _ <- chunk "{-|"
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
    _ <- space
    documentation <- choice [parseDocumentation, return ""]
    _ <- space
    signature <- choice [try $ parseTypeSignature 1 0, return ""]
    _ <- space
    name <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- char '='
    commentBeforeExpression <- commentSpaceParser 4
    expression <- parseExpression 2 DoesntNeedBrackets 4
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

parseTypeDeclarationParameters :: Int -> Parser Text
parseTypeDeclarationParameters startColumn =
  do
    parameters <-
      many $
        try $
          do
            _ <- space
            parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
            if parameterColumn <= startColumn
              then fail "invalid indentation"
              else try $ do
                parameter <- parseType 8
                return parameter
    return $ intercalate " " parameters

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
              parameter <- parsePattern NeedsBrackets startColumn 0
              _ <- space
              return parameter
    return $ intercalate " " parameters

parseTypeSignature :: Int -> Int -> Parser Text
parseTypeSignature startColumn indent =
  do
    name <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    startRow <- fmap (unPos . sourceLine) getSourcePos
    types <- some $
      try $ do
        column <- fmap (unPos . sourceColumn) getSourcePos
        if column <= startColumn
          then fail "invalid indentation"
          else do
            type_ <- parseType (indent + 4)
            _ <-
              choice
                [ try $ do
                    space
                    _ <- chunk "->"
                    space,
                  return ()
                ]
            return type_
    endRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- space
    return $
      mconcat
        [ name,
          " :",
          if endRow > startRow
            then "\n" <> pack (take (indent + 4) (repeat ' '))
            else " ",
          intercalate
            ( mconcat
                [ if endRow > startRow
                    then "\n" <> pack (take (indent + 4) (repeat ' '))
                    else " ",
                  "-> "
                ]
            )
            types
        ]

parseType :: Int -> Parser Text
parseType indent =
  choice
    [ try parseTypeWithParameters,
      try parseEmptyRecord,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      try parseFunctionType,
      parseTupleType indent,
      parseVerbatim
    ]

parseAliasedType :: Int -> Parser Text
parseAliasedType indent =
  choice
    [ try $ parseBareFunctionType 2 indent,
      parseTypeWithParameters,
      parseEmptyRecord,
      parseRecordType indent,
      parseTupleType indent
    ]

parseNotFollowedByArrow :: Parser Text -> Parser Text
parseNotFollowedByArrow p =
  do
    item <- p
    _ <- lookAhead $
      do
        _ <- commentSpaceParser 0
        notFollowedBy $ chunk "->"
    return item

parseBareFunctionType :: Int -> Int -> Parser Text
parseBareFunctionType minColumn indent =
  do
    types <- some $
      do
        column <- fmap (unPos . sourceColumn) getSourcePos
        if column < minColumn
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
    items <- many $
      do
        _ <- space
        choice
          [ try $ parseTupleTypeItem indent,
            parseBareFunctionType 2 indent
          ]
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
    _ <- choice [char ',', lookAhead (char ')')]
    return type_

parseExtensibleRecordType :: Int -> Parser Text
parseExtensibleRecordType indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '{'
    _ <- space
    name <- parseName
    _ <- space
    _ <- char '|'
    _ <- space
    items <- many (parseRecordTypeItem (indent + 2))
    _ <- char '}'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    if null items
      then return "{}"
      else
        return $
          mconcat
            [ "{ ",
              name,
              if endRow > startRow
                then "\n" <> (pack $ take (indent + 4) $ repeat ' ') <> "| "
                else " | ",
              intercalate
                ( if endRow > startRow
                    then "\n" <> (pack $ take (indent + 4) $ repeat ' ') <> ", "
                    else ", "
                )
                items,
              if endRow > startRow
                then "\n" <> (pack $ take indent $ repeat ' ') <> "}"
                else " }"
            ]

parseRecordType :: Int -> Parser Text
parseRecordType indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '{'
    _ <- space
    items <- many (parseRecordTypeItem (indent + 2))
    _ <- char '}'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    if null items
      then return "{}"
      else
        return $
          mconcat
            [ "{ ",
              intercalate
                ( if endRow > startRow
                    then "\n" <> (pack $ take indent $ repeat ' ') <> ", "
                    else ", "
                )
                items,
              if endRow > startRow
                then "\n" <> (pack $ take indent $ repeat ' ') <> "}"
                else " }"
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
    commentBefore <- commentSpaceParser indent
    startRow <- fmap (unPos . sourceLine) getSourcePos
    name <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    right <-
      choice
        [ try $ parseNotFollowedByArrow $ parseType indent,
          parseBareFunctionType 2 indent
        ]
    endRow <- fmap (unPos . sourceLine) getSourcePos
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- space
    _ <- choice [char ',', lookAhead (char '}')]
    _ <- space
    return $
      mconcat
        [ commentBefore,
          if commentBefore == "" then "" else "\n" <> pack (take indent (repeat ' ')),
          name,
          " :",
          if endRow > startRow
            then "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
            else " ",
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
    _ <- space
    parameters <- parseTypeParameters startColumn
    if parameters == ""
      then return name
      else return $ name <> " " <> parameters

parseTypeParameters :: Int -> Parser Text
parseTypeParameters startColumn =
  do
    parameters <-
      some $
        try $ do
          _ <- space
          parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
          if parameterColumn <= startColumn
            then fail "invalid indentation"
            else do
              parameter <- parseTypeParameter 8
              return parameter
    return $ intercalate " " parameters

parseImport :: Parser Text
parseImport =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- chunk "import"
    _ <- space1
    commentBetween <- commentSpaceParser 1
    name <- parseName
    _ <- space
    as_ <-
      choice
        [ do
            column <- fmap (unPos . sourceColumn) getSourcePos
            if column == 1
              then fail "can't have import 'as' at start of line"
              else do
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
            parseExposing 8 [],
          return ""
        ]
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $
      mconcat
        [ "import ",
          commentBetween,
          if commentBetween == "" then "" else " ",
          name,
          if as_ == ""
            then ""
            else " as " <> as_,
          if exposing_ == ""
            then ""
            else
              if endRow == startRow
                then " exposing" <> exposing_
                else "\n    exposing" <> exposing_
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
            try parseSectionComment,
            try portDeclaration,
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

portDeclaration :: Parser Text
portDeclaration =
  do
    _ <- space
    documentation <- choice [parseDocumentation, return ""]
    _ <- space
    _ <- chunk "port"
    _ <- space1
    signature <- parseTypeSignature 1 0
    return $ (if documentation == "" then "" else documentation <> "\n") <> "port " <> signature

parseSectionComment :: Parser Text
parseSectionComment =
  choice
    [ try $ do
        _ <- space
        block <- parseNonDocBlockComment
        _ <- space
        line <- parseLineComment
        _ <- space
        return $ "\n" <> block <> "\n" <> line,
      do
        _ <- space
        lines_ <- some parseLineComment
        return $ "\n" <> intercalate "\n" lines_,
      try $ do
        _ <- space
        block <- parseNonDocBlockComment
        return $ "\n" <> block
    ]

parseNonDocBlockComment :: Parser Text
parseNonDocBlockComment =
  do
    _ <- chunk "{-"
    _ <- lookAhead $ notFollowedBy (char '|')
    contents <-
      many $
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '-'),
            try $ do
              _ <- char '-'
              _ <- notFollowedBy $ lookAhead $ char '}'
              return "-"
          ]
    _ <- chunk "-}"
    return $ "{-" <> mconcat contents <> "-}"

notFollowedByInfix :: Parser Text -> Parser Text
notFollowedByInfix p =
  do
    item <- p
    _ <- lookAhead $
      do
        _ <- commentSpaceParser 0
        notFollowedBy parseInfix
    return item

makeDottable :: Parser Text -> Parser Text
makeDottable p =
  do
    item <- p
    dottings <-
      some $
        do
          _ <- char '.'
          parseName
    return $ intercalate "." (item : dottings)

notDottable :: Parser Text -> Parser Text
notDottable p =
  do
    item <- p
    _ <- notFollowedBy $ char '.'
    return item

parseCaseOfInBrackets :: Int -> Int -> Parser Text
parseCaseOfInBrackets minColumn indent =
  do
    _ <- char '('
    _ <- space
    caseOf <- parseCaseOf indent
    _ <- space
    _ <- char ')'
    return $
      mconcat
        [ "(",
          caseOf,
          "\n" <> pack (take indent $ repeat ' '),
          ")"
        ]

parseExpression :: Int -> Context -> Int -> Parser Text
parseExpression minColumn context indent =
  choice
    [ try $ parseCaseOf indent,
      try $ parseGlsl indent,
      try $ notFollowedByInfix $ parseList indent,
      try $ parseIfThenElse minColumn indent,
      try $ parseLetIn minColumn indent,
      try $ notFollowedByInfix $ parseRecord indent,
      try $ notFollowedByInfix $ parseRecordUpdate indent,
      try $ notFollowedByInfix (parseFunctionCall minColumn indent),
      try $ parseInfixed minColumn indent,
      try parseInfixInBrackets,
      try $ notDottable $ parseParenthesised context indent,
      try $ makeDottable $ parseParenthesised NeedsBrackets indent,
      parseTuple context indent,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseCharLiteral,
      parseAnonymousFunction minColumn indent
    ]

parseGlsl :: Int -> Parser Text
parseGlsl indent =
  do
    _ <- chunk "[glsl|"
    pieces <-
      some $
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '|'),
            try $ do
              _ <- char '|'
              _ <- lookAhead $ notFollowedBy (char ']')
              return "|"
          ]
    _ <- chunk "|]"
    return $ "[glsl|" <> mconcat pieces <> "|]"

parseAnonymousFunction :: Int -> Int -> Parser Text
parseAnonymousFunction minColumn indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '\\'
    pattern <- parseParameters minColumn
    _ <- space
    _ <- chunk "->"
    comment <- commentSpaceParser (floorToFour (indent + 4))
    body <- parseExpression minColumn DoesntNeedBrackets (floorToFour (indent + 4))
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $
      mconcat
        [ "\\",
          pattern,
          " ->",
          if endRow > startRow
            then "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
            else " ",
          comment,
          if comment == ""
            then ""
            else "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' ')),
          body
        ]

parseRecord :: Int -> Parser Text
parseRecord indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '{'
    _ <- space
    items <- many (parseRecordItem (indent + 2))
    _ <- char '}'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    let indentation =
          if endRow > startRow
            then "\n" <> (pack $ take indent $ repeat ' ')
            else ""
    let endSpace =
          if endRow > startRow
            then ""
            else " "
    if null items
      then return "{}"
      else
        return $
          mconcat
            [ "{ ",
              intercalate (indentation <> ", ") items,
              indentation <> endSpace <> "}"
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
    items <- many (parseRecordItem (indent + 4))
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

parseRecordItem :: Int -> Parser Text
parseRecordItem indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    name <- parseName
    _ <- space
    _ <- char '='
    _ <- space
    rowBeforeComment <- fmap (unPos . sourceLine) getSourcePos
    commentBefore <- commentSpaceParser indent
    right <- parseExpression 1 DoesntNeedBrackets (floorToFour (indent + 4))
    endRow <- fmap (unPos . sourceLine) getSourcePos
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- space
    _ <- choice [char ',', lookAhead (char '}')]
    _ <- space
    return $
      mconcat
        [ name,
          " =",
          if endRow > startRow
            then
              if commentBefore == ""
                then "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
                else
                  mconcat
                    [ "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' ')),
                      commentBefore,
                      if endRow > rowBeforeComment
                        then "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
                        else " "
                    ]
            else
              ( if commentBefore == ""
                  then ""
                  else " " <> commentBefore
              )
                <> " ",
          right,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> pack (take (floorToFour indent) (repeat ' ')) <> commentAfter
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

parsePattern :: Context -> Int -> Int -> Parser Text
parsePattern context minColumn indent =
  choice
    [ try $ do
        pattern <- parsePatternNoAlias minColumn indent
        _ <-
          notFollowedBy $
            lookAhead $
              do
                _ <- space1
                _ <- chunk "as"
                _ <- space1
                return ()
        return pattern,
      try $ parseAliasedPattern context minColumn indent
    ]

parseTypeParameter :: Int -> Parser Text
parseTypeParameter indent =
  choice
    [ try parseFunctionType,
      parseTupleType indent,
      parseList indent,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      try parseFunctionCallPattern,
      parseVerbatim,
      parseSimpleStringLiteral
    ]

parseAliasedPattern :: Context -> Int -> Int -> Parser Text
parseAliasedPattern context minColumn indent =
  do
    _ <- choice [char '(' >> return (), return ()]
    pattern <- parsePatternBeforeAs minColumn indent
    _ <- space
    _ <- chunk "as"
    _ <- space
    name <- parseName
    _ <- space
    _ <- choice [char ')' >> return (), return ()]
    return $
      mconcat
        [ case context of
            NeedsBrackets ->
              "("
            DoesntNeedBrackets ->
              "",
          pattern,
          " as ",
          name,
          case context of
            NeedsBrackets ->
              ")"
            DoesntNeedBrackets ->
              ""
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
    _ <- choice [char ',', lookAhead (char '}')]
    return name

parsePatternBeforeAs :: Int -> Int -> Parser Text
parsePatternBeforeAs minColumn indent =
  choice
    [ try $ parseTuplePattern NeedsBrackets indent,
      parseList indent,
      parseRecordPattern
    ]

parsePatternNoAlias :: Int -> Int -> Parser Text
parsePatternNoAlias minColumn indent =
  choice
    [ try $ parseConsPattern minColumn indent,
      try $ parseTuplePattern NeedsBrackets indent,
      parseList indent,
      parseRecordPattern,
      try parseFunctionCallPattern,
      parseVerbatim,
      parseCharLiteral,
      parseSimpleStringLiteral
    ]

parsePatternInsideConsPattern :: Int -> Int -> Parser Text
parsePatternInsideConsPattern minColumn indent =
  choice
    [ try $ parseTuple NeedsBrackets indent,
      parseAliasedPattern NeedsBrackets minColumn indent,
      parseList indent,
      try $ parseFunctionCall minColumn indent,
      parseVerbatim,
      parseCharLiteral
    ]

parseConsPattern :: Int -> Int -> Parser Text
parseConsPattern minColumn indent =
  do
    left <- parsePatternInsideConsPattern minColumn indent
    items <-
      some
        ( try $ do
            _ <- space
            _ <- chunk "::"
            _ <- space
            parsePatternInsideConsPattern minColumn indent
        )
    return $ intercalate " :: " (left : items)

parseArgumentExpression :: Int -> Parser Text
parseArgumentExpression indent =
  choice
    [ try $ notDottable $ parseTuple NeedsBrackets indent,
      try $ makeDottable $ parseTuple NeedsBrackets indent,
      parseInfixInBrackets,
      parseList indent,
      try $ parseRecord indent,
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
      try $ parseCaseOfInBrackets minColumn indent,
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
              comment <- commentSpaceParser (indent + 4)
              column <- fmap (unPos . sourceColumn) getSourcePos
              if column <= minColumn
                then fail "argument is too far left"
                else do
                  arg <- parseArgumentExpression (floorToFour (indent + 4))
                  argEndRow <- fmap (unPos . sourceLine) getSourcePos
                  return (comment, arg, argEndRow)
        endRow <- fmap (unPos . sourceLine) getSourcePos
        return $ mconcat $ f : map (addArgSpaces startRow endRow indent) items

addArgSpaces :: Int -> Int -> Int -> (Text, Text, Int) -> Text
addArgSpaces startRow endRow indent (comment, arg, row) =
  let indentation =
        if endRow == startRow || row == startRow || numNewlinesInMultiline arg == endRow - startRow
          then " "
          else (pack $ '\n' : (take (floorToFour (indent + 4)) $ repeat ' '))
   in if comment == ""
        then indentation <> arg
        else indentation <> comment <> indentation <> arg

numNewlinesInMultiline :: Text -> Int
numNewlinesInMultiline arg =
  let multiline = snd $ Text.breakOn "\"\"\"" (fst $ Text.breakOnEnd "\"\"\"" arg)
   in Text.count "\n" multiline

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
            else parsePattern NeedsBrackets 1 1
    return $ intercalate " " (f : items)

parseInfix :: Parser Text
parseInfix =
  do
    column <- fmap (unPos . sourceColumn) getSourcePos
    infix_ <-
      if column == 0
        then fail "can't have an infix at column zero"
        else choice $ map chunk infixes
    _ <- lookAhead afterInfixChar
    return infix_

afterInfixChar :: Parser Char
afterInfixChar =
  choice $ map char ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789()[] \n" :: String)

infixes :: [Text]
infixes =
  ["==", "&&", "//", ">>", "<<", "||", "<=", ">=", "<|", "|=", "++", "+", "|>", "|.", "::", ">", "<", "/=", "-", "*", "^", "/"]

parseInfixedExpression :: Int -> Int -> Parser Text
parseInfixedExpression minColumn indent =
  choice
    [ try $ parseCaseOf indent,
      try $ parseIfThenElse minColumn indent,
      try $ parseLetIn minColumn indent,
      try $ parseTuple NeedsBrackets indent,
      parseList indent,
      try parseEmptyRecord,
      try $ parseRecord indent,
      parseRecordUpdate indent,
      try $ parseFunctionCall minColumn indent,
      parseInfixInBrackets,
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
    midRow <- fmap (unPos . sourceLine) getSourcePos
    items <- parseInfixedItems minColumn (floorToFour indent) []
    endRow <- fmap (unPos . sourceLine) getSourcePos
    if null items
      then fail "zero infix items"
      else do
        return $
          mconcat
            [ firstExpression,
              mconcat $ map (addInfixWhitespace (length items < 2) (midRow > startRow) (takeEnd 3 firstExpression == "\"\"\"") (endRow > startRow)) items
            ]

parseInfixedItems ::
  Int ->
  Int ->
  [(Int, Bool, Text, Text, Text, Text)] ->
  Parser [(Int, Bool, Text, Text, Text, Text)]
parseInfixedItems minColumn indent accum =
  choice
    [ try $
        do
          startRow <- fmap (unPos . sourceLine) getSourcePos
          commentBefore <- commentSpaceParser (indent + 4)
          infix_ <- parseInfix
          afterInfixRow <- fmap (unPos . sourceLine) getSourcePos
          commentAfter <- commentSpaceParser (indent + 4)
          expression <- parseInfixedExpression minColumn (indent + 4)
          parseInfixedItems
            minColumn
            ( if infix_ == "<|"
                then indent + 4
                else indent
            )
            ((indent + 4, afterInfixRow == startRow, commentBefore, infix_, commentAfter, expression) : accum),
      return $ reverse accum
    ]

addInfixWhitespace :: Bool -> Bool -> Bool -> Bool -> (Int, Bool, Text, Text, Text, Text) -> Text
addInfixWhitespace oneOrTwo firstIsMultiline followsTripleQuoteString isMultiline (indent, isOnSameRowAsPrevious, commentBefore, infix_, commentAfter, expression) =
  let newIndent = floorToFour indent
   in if isMultiline
        then
          if infix_ == "<|"
            then
              if firstIsMultiline
                then
                  mconcat
                    [ "\n",
                      pack $ take (newIndent - 4) (repeat ' '),
                      "<|\n",
                      pack (take newIndent (repeat ' ')),
                      expression
                    ]
                else
                  mconcat
                    [ " <|\n",
                      pack (take newIndent (repeat ' ')),
                      expression
                    ]
            else
              if infix_ == "|>" && followsTripleQuoteString && oneOrTwo && isOnSameRowAsPrevious
                then " |> " <> expression
                else
                  mconcat
                    [ "\n" <> pack (take newIndent (repeat ' ')),
                      commentBefore,
                      if commentBefore == ""
                        then ""
                        else "\n" <> pack (take newIndent (repeat ' ')),
                      infix_,
                      " ",
                      expression
                    ]
        else
          if infix_ == "."
            then infix_ <> expression
            else
              mconcat
                [ if commentBefore == ""
                    then ""
                    else " " <> commentBefore,
                  " ",
                  infix_,
                  " ",
                  if commentAfter == ""
                    then ""
                    else commentAfter <> " ",
                  expression
                ]

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
          items <- parseLetBind (column + 1) (floorToFour (indent + 4))
          _ <- space
          return items
    _ <- chunk "in"
    comment <- commentSpaceParser indent
    in_ <- parseExpression minColumn DoesntNeedBrackets indent
    let inSpaces = "\n" <> (pack $ take indent $ repeat ' ')
    return $
      mconcat
        [ "let\n",
          intercalate "\n\n" let_,
          inSpaces,
          "in",
          inSpaces,
          comment,
          if comment == ""
            then ""
            else inSpaces,
          in_
        ]

parseLetBind :: Int -> Int -> Parser Text
parseLetBind minColumn indent =
  do
    comment <- commentSpaceParser indent
    signature <- choice [try $ parseTypeSignature minColumn indent, return ""]
    left <- parsePattern DoesntNeedBrackets 1 indent
    _ <- space
    _ <- char '='
    commentBeforeRight <- commentSpaceParser indent
    right <- parseExpression minColumn DoesntNeedBrackets (indent + 4)
    let leftSpaces = pack $ take indent $ repeat ' '
    let rightSpaces = pack $ take (indent + 4) $ repeat ' '
    return $
      mconcat
        [ if comment == ""
            then ""
            else leftSpaces <> comment <> "\n",
          if signature == ""
            then ""
            else leftSpaces <> signature <> "\n",
          leftSpaces,
          left,
          " =\n",
          if commentBeforeRight == "" then "" else rightSpaces,
          commentBeforeRight,
          if commentBeforeRight == "" then "" else "\n",
          rightSpaces,
          right
        ]

parseIfThenElse :: Int -> Int -> Parser Text
parseIfThenElse minColumn indent =
  do
    _ <- chunk "if"
    _ <- space1
    if_ <- parseExpression minColumn DoesntNeedBrackets (indent + 4)
    _ <- space
    _ <- chunk "then"
    commentBeforeThen <- commentSpaceParser (indent + 4)
    then_ <- parseExpression minColumn DoesntNeedBrackets (indent + 4)
    _ <- space
    _ <- chunk "else"
    _ <- space1
    commentAfterElse <- commentSpaceParser (indent + 4)
    let ifThen =
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
              commentBeforeThen,
              if commentBeforeThen == ""
                then ""
                else "\n" <> pack (take (indent + 4) (repeat ' ')),
              then_,
              "\n\n" <> pack (take indent (repeat ' ')),
              "else"
            ]
    choice
      [ do
          nestedIf <- parseIfThenElse minColumn indent
          return $ ifThen <> " " <> nestedIf,
        do
          else_ <- parseExpression minColumn DoesntNeedBrackets (indent + 4)
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
                "then\n" <> pack (take (floorToFour (indent + 4)) (repeat ' ')),
                commentBeforeThen,
                if commentBeforeThen == "" then "" else "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' ')),
                then_,
                "\n\n" <> pack (take indent (repeat ' ')),
                "else\n" <> pack (take (floorToFour (indent + 4)) (repeat ' ')),
                commentAfterElse,
                if commentAfterElse == ""
                  then ""
                  else "\n" <> pack (take (indent + 4) (repeat ' ')),
                else_
              ]
      ]

parseCaseOf :: Int -> Parser Text
parseCaseOf indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- chunk "case"
    commentAfterCase <- commentSpaceParser (indent + 4)
    caseOf <- parseExpression 1 DoesntNeedBrackets (indent + 4)
    _ <- space
    _ <- chunk "of"
    endRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- takeWhile1P Nothing (\ch -> ch == '\n' || ch == ' ')
    column <- fmap (unPos . sourceColumn) getSourcePos
    branches <- some (parseCaseOfBranch column (floorToFour (indent + 4)))
    return $
      mconcat
        [ "case",
          if endRow > startRow
            then "\n" <> pack (take (indent + 4) (repeat ' '))
            else " ",
          commentAfterCase,
          if commentAfterCase == ""
            then ""
            else
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
        commentAbove <- commentSpaceParser indent
        left <- parsePattern DoesntNeedBrackets (minColumn - 4) indent
        _ <- space
        _ <- chunk "->"
        _ <- space
        comment <- commentSpaceParser (indent + 4)
        right <- parseExpression (minColumn + 1) DoesntNeedBrackets (indent + 4)
        _ <- space
        return $
          mconcat
            [ pack $ take indent $ repeat ' ',
              commentAbove,
              if commentAbove == "" then "" else "\n" <> pack (take indent (repeat ' ')),
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
        (\ch -> ch `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz.0123456789_" :: String))
    let word = Text.cons firstChar remainder
    if Set.member word keywords
      then fail $ "expecting a name tail but got: " <> unpack word
      else return word

parseNegative :: Parser Text
parseNegative =
  do
    _ <- char '-'
    firstDigit <- choice $ map char ("0123456789" :: String)
    remainder <- takeWhileP Nothing (\ch -> ch `elem` ("0123456789xabcdef." :: String))
    let combined = "-" <> Text.singleton firstDigit <> remainder
    return $ case combined of
      "-0" ->
        "0"
      "-0x00" ->
        "0x00"
      _ ->
        combined

parseVerbatim :: Parser Text
parseVerbatim =
  choice
    [ try parseNegative,
      do
        word <-
          takeWhile1P
            (Just "verbatim character")
            (\ch -> ch `elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz._" :: String))
        if Set.member word keywords
          then fail $ "expecting a verbatim, but got: " <> unpack word
          else return word
    ]

parseLineComment :: Parser Text
parseLineComment =
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
          [ parseLineComment,
            parseBlockComment,
            do
              _ <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
              return ""
          ]
    return $
      intercalate
        ("\n" <> pack (take indent (repeat ' ')))
        (filter (\s -> s /= "") comments)

parseBlockComment :: Parser Text
parseBlockComment =
  do
    _ <- chunk "{-"
    contents <-
      many $
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '-'),
            try $ do
              _ <- char '-'
              _ <- notFollowedBy $ lookAhead $ char '}'
              return "-"
          ]
    _ <- chunk "-}"
    return $ "{-" <> mconcat contents <> "-}"

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
    expression <- parsePattern DoesntNeedBrackets 1 indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- choice [char ',', lookAhead (char ')')]
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

parseMultiTupleItem :: Int -> Char -> Parser Text
parseMultiTupleItem indent end =
  do
    commentBefore <- commentSpaceParser indent
    expression <- parseExpression 1 DoesntNeedBrackets indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser (floorToFour indent)
    _ <- choice [char ',', lookAhead (char end)]
    return $
      mconcat
        [ if commentBefore == ""
            then ""
            else commentBefore <> "\n" <> pack (take indent (repeat ' ')),
          expression,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n" <> pack (take indent (repeat ' ')) <> commentAfter
        ]

parseMultiListItem :: Int -> Char -> Parser Text
parseMultiListItem indent end =
  do
    commentBefore <- commentSpaceParser indent
    expression <- parseExpression 1 DoesntNeedBrackets indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser (floorToFour indent)
    _ <- choice [char ',', lookAhead (char end)]
    return $
      mconcat
        [ if commentBefore == ""
            then ""
            else commentBefore <> "\n" <> pack (take indent (repeat ' ')),
          expression,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> pack (take (floorToFour indent) (repeat ' ')) <> commentAfter
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

parseTuple :: Context -> Int -> Parser Text
parseTuple context indent =
  choice
    [ try $ parseParenthesised context indent,
      parseMultiTuple indent
    ]

parseParenthesised :: Context -> Int -> Parser Text
parseParenthesised context indent =
  do
    _ <- char '('
    _ <- space
    startLine <- fmap (unPos . sourceLine) getSourcePos
    item <- parseExpression 1 DoesntNeedBrackets (indent + 1)
    endLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- space
    _ <- char ')'
    if endLine > startLine
      then case context of
        NeedsBrackets ->
          return $
            mconcat
              [ "(",
                item,
                if Text.takeEnd 3 item == "\"\"\""
                  then ")"
                  else "\n" <> (pack $ take indent $ repeat ' ') <> ")"
              ]
        DoesntNeedBrackets ->
          return item
      else case context of
        NeedsBrackets ->
          return $ "(" <> item <> ")"
        DoesntNeedBrackets ->
          return item

parseMultiTuple :: Int -> Parser Text
parseMultiTuple indent =
  do
    startLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- many (parseMultiTupleItem (indent + 2) ')')
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

parseList :: Int -> Parser Text
parseList indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '['
    comment <- commentSpaceParser (indent + 2)
    items <- many (parseMultiListItem (indent + 2) ']')
    _ <- char ']'
    endRow <- fmap (unPos . sourceLine) getSourcePos
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
            else "[" <> comment <> "\n" <> pack (take indent (repeat ' ')) <> "]"
      else
        return $
          mconcat
            [ "[ ",
              if comment == ""
                then ""
                else comment <> indentation <> "  ",
              intercalate (indentation <> ", ") items,
              indentation <> endSpace <> "]"
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
