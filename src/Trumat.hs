module Trumat (trumat) where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, intercalate, pack, replicate, takeEnd, unpack)
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
  ( Bool (..),
    Char,
    Either (..),
    Eq,
    Int,
    Maybe (..),
    Ordering (..),
    Show,
    String,
    all,
    any,
    compare,
    div,
    elem,
    fail,
    filter,
    fmap,
    fst,
    head,
    id,
    length,
    map,
    max,
    maximum,
    mconcat,
    mod,
    not,
    null,
    repeat,
    return,
    reverse,
    show,
    snd,
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
    (>=),
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
    parseExportDocsRowOnly 1 []

consumeExportList :: Parser ()
consumeExportList =
  do
    _ <- char '('
    consumeExportListHelp 1

log :: Show a => String -> a -> a
log message value =
  trace (message <> ": " <> show value) value

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

parseUnorderedList :: Parser Text
parseUnorderedList =
  do
    indent <- getUnorderedListItemIndent
    isGappy <- lookAhead $ parseUnorderedListGappiness 1 indent False
    parseUnorderedListItemHelp 1 indent "" isGappy

getUnorderedListItemIndent :: Parser Int
getUnorderedListItemIndent =
  do
    indent <- fmap Text.length (takeWhileP Nothing (\ch -> ch == ' '))
    _ <- choice [chunk "- ", chunk "-\n"]
    return indent

parseUnorderedListGappiness :: Int -> Int -> Bool -> Parser Bool
parseUnorderedListGappiness nesting indent accumulated =
  if nesting == 0
    then return accumulated
    else do
      text <- fmap Text.strip $ takeWhileP Nothing (\ch -> ch /= '\n')
      if text == ""
        then fail "empty unordered list item"
        else do
          gappiness <- fmap (any id) $
            many $
              try $
                do
                  _ <- notFollowedBy $ do
                    _ <- space
                    _ <- chunk "-}"
                    return ()
                  _ <- notFollowedBy $ do
                    _ <- char '\n'
                    _ <- space
                    _ <- chunk "- "
                    return ()
                  newlines <- choice [chunk "\n\n", chunk "\n"]
                  _ <- lookAhead (char ' ')
                  _ <- takeWhile1P Nothing (\ch -> ch /= '\n')
                  return (Text.length newlines > 1)
          choice
            [ try $ do
                newlines <- choice [chunk "\n\n", chunk "\n"]
                newIndent <- getUnorderedListItemIndent
                parseUnorderedListGappiness
                  ( nesting + case newIndent `compare` indent of
                      GT -> 1
                      LT -> -1
                      EQ -> 0
                  )
                  newIndent
                  (newIndent == indent && (gappiness || accumulated || Text.length newlines > 1)),
              return (gappiness || accumulated)
            ]

parseUnorderedListItemHelp :: Int -> Int -> Text -> Bool -> Parser Text
parseUnorderedListItemHelp nesting indent accumulated isGappy =
  if nesting == 0
    then return accumulated
    else do
      _ <- notFollowedBy $ chunk "-}"
      text <-
        fmap
          ( \s ->
              if s == "*"
                then "*"
                else (escapeBackticks . escapeAsterisks . escapeBackslashes) s
          )
          $ fmap Text.strip
          $ fmap (\line -> Text.intercalate " " $ Text.words line)
          $ takeWhileP Nothing (\ch -> ch /= '\n')
      let numSpaces :: Int
          numSpaces =
            ((nesting - 1) * 4) + 2

          formatted :: Text
          formatted =
            replicate numSpaces " " <> "- " <> text

      do
        otherLines <- fmap Text.strip $
          fmap (intercalate "\n") $
            many $
              try $
                do
                  _ <- notFollowedBy $ do
                    _ <- char '\n'
                    _ <- space
                    _ <- choice [chunk "- ", chunk "-\n", chunk "-}"]
                    return ()
                  _ <- char '\n'
                  takeWhile1P Nothing (\ch -> ch /= '\n')
        choice
          [ try $ do
              _ <- char '\n'
              newIndent <- getUnorderedListItemIndent
              parseUnorderedListItemHelp
                ( nesting + case newIndent `compare` indent of
                    GT -> 1
                    LT -> -1
                    EQ -> 0
                )
                newIndent
                ( accumulated
                    <> ( if text == ""
                           then ""
                           else
                             (if accumulated == "" then "" else "\n")
                               <> formatted
                               <> (if otherLines /= "" && isGappy then "\n" else "")
                               <> ( if otherLines == "" || Text.takeEnd 1 otherLines == "\n"
                                      then ""
                                      else "\n" <> Text.replicate (indent + 2) " "
                                  )
                               <> otherLines
                       )
                )
                isGappy,
            return $
              accumulated
                <> ( if text == "" || text == "*"
                       then ""
                       else
                         (if accumulated == "" then "" else "\n")
                           <> (if accumulated /= "" && isGappy then "\n" else "")
                           <> formatted
                           <> (if otherLines == "" then "" else "\n    ")
                           <> otherLines
                   )
          ]

parseBacktickedCodeBlock :: Parser Text
parseBacktickedCodeBlock =
  do
    _ <- chunk "```"
    pieces <- many $ try $ do
      piece <- takeWhile1P Nothing (\ch -> ch /= '`')
      choice
        [ do
            notFollowedBy $ lookAhead $ chunk "```"
            backticks <- choice [chunk "``", chunk "`"]
            return $ piece <> backticks,
          return piece
        ]
    _ <- chunk "```"
    let code = mconcat pieces
        lines = Text.lines code
        indented = stripLeadingNewlines $ Text.intercalate "\n" $ map (\line -> if line == "" then "" else "    " <> line) lines
    return indented

removeEmptyBlockQuote :: [Text] -> [Text]
removeEmptyBlockQuote rows =
  filter (\row -> Text.strip row /= ">") rows

parseBlockQuote :: Parser Text
parseBlockQuote =
  do
    firstRow <- parseBlockQuoteLine
    subsequent <- many $
      try $ do
        _ <- char '\n'
        parseBlockQuoteLine
    let rows = firstRow : subsequent
    return $ Text.intercalate "\n" (removeEmptyBlockQuote rows)

parseBlockQuoteLine :: Parser Text
parseBlockQuoteLine =
  do
    _ <- char '>'
    contents <- parseBlockQuoteContents
    let stripped = Text.strip contents
    return $ ">" <> (if stripped == "" then "" else " ") <> stripped

parseBlockQuoteContents :: Parser Text
parseBlockQuoteContents =
  do
    pieces <-
      many $
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '\n' && ch /= '-'),
            try $ do
              _ <- char '-'
              _ <- notFollowedBy (char '}')
              return "-"
          ]
    return $ Text.intercalate " " $ Text.words $ mconcat $ map escapeAsterisks pieces

parseDocRow :: Parser Text
parseDocRow =
  choice
    [ try $ do
        _ <- some $
          do
            _ <- char '\n'
            spaces <- takeWhileP Nothing (\ch -> ch == ' ')
            if Text.length spaces >= 4
              then fail "is a code block"
              else return ()
        _ <- char '-'
        hyphens <- takeWhile1P Nothing (\ch -> ch == '-')
        return $ "\n\n-" <> hyphens,
      parseBacktickedCodeBlock,
      parseBlockQuote,
      try parseMultipleDocHeaders,
      try parseDocHeader,
      chunk "\n",
      do
        _ <- chunk "    "
        code <- takeWhileP Nothing (\ch -> ch /= '\n')
        _ <- char '\n'
        return $ "    " <> code <> "\n",
      try $ do
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        docs <- parseExportDocsRow
        headers <- parseMultipleDocHeaders
        return $
          mconcat
            [ "@docs ",
              intercalate ", " docs,
              headers
            ],
      try $ do
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        docs <- parseExportDocsRow
        header <- choice [try parseDocHeader, return ""]
        _ <- char '\n'
        return $
          mconcat
            [ "@docs ",
              intercalate ", " docs,
              header,
              "\n"
            ],
      try $ do
        docs <- parseExportDocsRow
        header <- try parseDocHeader
        return $
          mconcat
            [ "@docs ",
              intercalate ", " docs,
              header
            ],
      try parseUnorderedList,
      try parseNumberedListItems,
      do
        pieces <-
          some $
            do
              text <- try $ fmap (escapeBackticks . escapeUnderscores . escapeAsterisks . escapeBackslashes) $ parseOrdinaryTextInDoc
              _ <-
                notFollowedBy $
                  lookAhead $
                    choice
                      [chunk "-}", chunk "docs"]
              choice
                [ do
                    hyphens <- some $ char '-'
                    return $ text <> Text.pack hyphens,
                  do
                    _ <- char '@'
                    return $ text <> "@",
                  return text
                ]
        return $ mconcat pieces
    ]

parseDocHeader :: Parser Text
parseDocHeader =
  do
    _ <- space
    contents <- parseDocHeaderContents
    _ <- takeWhileP Nothing (\ch -> ch == '\n')
    return $ "\n\n\n" <> contents <> "\n\n"

parseDocHeaderContents :: Parser Text
parseDocHeaderContents =
  choice
    [ try $ do
        hashes <- takeWhile1P Nothing (\ch -> ch == '#')
        _ <- char ' '
        contents <- fmap Text.strip $ takeWhileP Nothing (\ch -> ch /= '\n')
        let gap = if contents == "" then "" else " "
        return $ hashes <> gap <> contents,
      do
        hashes <- takeWhile1P Nothing (\ch -> ch == '#')
        _ <- char '\n'
        return hashes
    ]

parseMultipleDocHeaders :: Parser Text
parseMultipleDocHeaders =
  do
    first <- do
      _ <- space
      contents <- parseDocHeaderContents
      _ <- space
      return contents
    subsequent <- some $ do
      _ <- space
      contents <- parseDocHeaderContents
      _ <- space
      return contents
    return $ "\n\n\n" <> first <> "\n\n\n" <> Text.intercalate "\n\n" subsequent <> "\n\n"

parseNumberedListItems :: Parser Text
parseNumberedListItems =
  do
    indent <- getNumberedListItemIndent
    isGappy <- lookAhead $ parseNumberedListGappiness 1 indent False
    parseNumberedListItemHelp 1 indent 1 "" isGappy

getNumberedListItemIndent :: Parser Int
getNumberedListItemIndent =
  do
    indent <- fmap Text.length (takeWhileP Nothing (\ch -> ch == ' '))
    _ <- do
      _ <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
      _ <- choice [char '.', char ')']
      return ()
    return indent

parseNumberedListGappiness :: Int -> Int -> Bool -> Parser Bool
parseNumberedListGappiness nesting indent accumulated =
  if nesting == 0
    then return accumulated
    else do
      text <- fmap Text.strip noDoubleSpacesLine
      if text == ""
        then fail "empty numbered list item"
        else do
          gappiness <- fmap (any id) $
            many $
              try $
                do
                  _ <- notFollowedBy $ do
                    _ <- space
                    _ <- chunk "-}"
                    return ()
                  _ <- notFollowedBy $ do
                    _ <- char '\n'
                    _ <- space
                    _ <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
                    _ <- choice [char '.', char ')']
                    return ()
                  newlines <- choice [chunk "\n\n", chunk "\n"]
                  _ <- lookAhead (char ' ')
                  _ <- takeWhile1P Nothing (\ch -> ch /= '\n')
                  return (Text.length newlines > 1)
          choice
            [ try $ do
                newlines <- choice [chunk "\n\n", chunk "\n"]
                newIndent <- getNumberedListItemIndent
                parseNumberedListGappiness
                  ( nesting + case newIndent `compare` indent of
                      GT -> 1
                      LT -> -1
                      EQ -> 0
                  )
                  newIndent
                  (gappiness || accumulated || Text.length newlines > 1),
              return (gappiness || accumulated)
            ]

parseNumberedListItemHelp :: Int -> Int -> Int -> Text -> Bool -> Parser Text
parseNumberedListItemHelp nesting indent number accumulated isGappy =
  if nesting == 0
    then return accumulated
    else do
      text <-
        fmap
          ( \s ->
              if s == "*"
                then "*"
                else (escapeBackticks . escapeAsterisks . escapeBackslashes) s
          )
          $ fmap Text.strip noDoubleSpacesLine
      let numSpaces :: Int
          numSpaces =
            ((nesting - 1) * 4)

          formatted :: Text
          formatted =
            replicate numSpaces " " <> Text.pack (show number) <> ". " <> (if number < 10 then " " else "") <> text

      if text == ""
        then fail "empty numbered list item"
        else do
          otherLines <- fmap Text.strip $
            fmap (intercalate "\n") $
              fmap (fmap (\line -> "    " <> line)) $
                many $
                  try $
                    do
                      _ <- notFollowedBy $ do
                        _ <- space
                        _ <- chunk "-}"
                        return ()
                      _ <- notFollowedBy $ do
                        _ <- char '\n'
                        _ <- space
                        _ <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
                        _ <- choice [char '.', char ')']
                        return ()
                      _ <- choice [chunk "\n\n", chunk "\n"]
                      _ <- takeWhileP Nothing (\ch -> ch == ' ')
                      startColumn <- fmap (unPos . sourceColumn) getSourcePos
                      if startColumn == 1
                        then fail "after end of list"
                        else takeWhile1P Nothing (\ch -> ch /= '\n')
          choice
            [ try $ do
                _ <- choice [chunk "\n\n", chunk "\n"]
                newIndent <- getNumberedListItemIndent
                parseNumberedListItemHelp
                  ( nesting + case newIndent `compare` indent of
                      GT -> 1
                      LT -> -1
                      EQ -> 0
                  )
                  newIndent
                  ( case newIndent `compare` indent of
                      GT -> 1
                      LT -> number
                      EQ -> number + 1
                  )
                  ( accumulated
                      <> (if accumulated == "" then "" else "\n")
                      <> (if accumulated /= "" && isGappy then "\n" else "")
                      <> formatted
                      <> (if otherLines /= "" && isGappy then "\n" else "")
                      <> (if otherLines == "" then "" else "\n    ")
                      <> otherLines
                  )
                  isGappy,
              return $
                accumulated
                  <> ( if text == "*"
                         then "\n\n"
                         else
                           (if accumulated == "" then "" else "\n")
                             <> (if accumulated /= "" && isGappy then "\n" else "")
                             <> formatted
                             <> (if otherLines /= "" && isGappy then "\n" else "")
                             <> (if otherLines == "" then "" else "\n    ")
                             <> otherLines
                     )
            ]

parseNumberedListSubsequentLine :: Parser Text
parseNumberedListSubsequentLine =
  do
    _ <- char '\n'
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- notFollowedBy $ do
      _ <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
      _ <- choice [char '.', char ')']
      return ()
    _ <- notFollowedBy $ do
      _ <- takeWhileP Nothing (\ch -> ch == ' ')
      _ <- chunk "-}"
      return ()
    line <- noDoubleSpacesLine
    return $ "    " <> Text.strip line

parseUnorderedListSubsequentLine :: Parser Text
parseUnorderedListSubsequentLine =
  do
    _ <- char '\n'
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- notFollowedBy $ chunk "- "
    _ <- notFollowedBy $ do
      _ <- takeWhileP Nothing (\ch -> ch == ' ')
      _ <- chunk "-}"
      return ()
    line <- noDoubleSpacesLine
    return $ "    " <> Text.strip line

parseNumberedListFirstLine :: Parser Text
parseNumberedListFirstLine =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
    _ <- choice [char '.', char ')']
    remainder <- noDoubleSpacesLine
    return $ Text.strip remainder

noDoubleSpacesLine :: Parser Text
noDoubleSpacesLine =
  fmap mconcat $
    some $
      choice
        [ takeWhile1P Nothing (\ch -> ch == ' ') >> return " ",
          takeWhile1P Nothing (\ch -> ch /= '\n' && ch /= ' ')
        ]

escapeBackslashes :: Text -> Text
escapeBackslashes text =
  case parse parseEscapeBackslashes "" text of
    Left _ ->
      text
    Right ok ->
      ok

parseEscapeBackslashes :: Parser Text
parseEscapeBackslashes =
  do
    pieces <-
      many $
        choice
          [ takeWhile1P Nothing (\ch -> ch /= '\\'),
            chunk "\\\\",
            chunk "\\*",
            chunk "\\_",
            chunk "\\`",
            do
              backslashes <- takeWhile1P Nothing (\ch -> ch == '\\')
              return $ Text.replace "\\" "\\\\" backslashes,
            takeWhile1P Nothing (\ch -> ch == '\\')
          ]
    return $ mconcat pieces

escapeBackticks :: Text -> Text
escapeBackticks text =
  case parse parseEscapeBackticks "" text of
    Left _ ->
      text
    Right ok ->
      ok

parseEscapeBackticks :: Parser Text
parseEscapeBackticks =
  do
    pieces <-
      many $
        choice
          [ try backtickQuote,
            takeWhile1P Nothing (\ch -> ch /= '\\' && ch /= '`'),
            chunk "\\_",
            chunk "\\`",
            do
              backticks <- takeWhile1P Nothing (\ch -> ch == '`')
              return $ Text.replace "`" "\\`" backticks,
            takeWhile1P Nothing (\ch -> ch == '`'),
            takeWhile1P Nothing (\ch -> ch == '\\')
          ]
    return $ mconcat pieces

backtickQuote :: Parser Text
backtickQuote =
  do
    leadingSpaces <- takeWhileP Nothing (\ch -> ch == ' ')
    leading <- takeWhile1P Nothing (\ch -> ch == '`')
    firstPiece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '`')
    otherPieces <- many $
      try $
        do
          spaces <- takeWhileP Nothing (\ch -> ch == ' ')
          piece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '`')
          return $ spaces <> piece
    trailing <- takeWhile1P Nothing (\ch -> ch == '`')
    if Text.length leading /= Text.length trailing
      then fail "there must be equal numbers of trailing and leading underscores"
      else return $ leadingSpaces <> "`" <> firstPiece <> mconcat otherPieces <> "`"

escapeUnderscores :: Text -> Text
escapeUnderscores text =
  case parse parseEscapeUnderscores "" text of
    Left _ ->
      text
    Right ok ->
      ok

parseEscapeUnderscores :: Parser Text
parseEscapeUnderscores =
  do
    pieces <-
      many $
        choice
          [ try backtickQuote,
            try underscoreBolds,
            takeWhile1P Nothing (\ch -> ch /= '\\' && ch /= '_' && ch /= '`'),
            chunk "\\_",
            chunk "\\`",
            do
              underscores <- takeWhile1P Nothing (\ch -> ch == '_')
              return $ Text.replace "_" "\\_" underscores,
            takeWhile1P Nothing (\ch -> ch == '_'),
            takeWhile1P Nothing (\ch -> ch == '`'),
            takeWhile1P Nothing (\ch -> ch == '\\')
          ]
    return $ mconcat pieces

escapeAsterisks :: Text -> Text
escapeAsterisks text =
  case parse parseEscapeAsterisks "" text of
    Left _ ->
      text
    Right ok ->
      ok

parseEscapeAsterisks :: Parser Text
parseEscapeAsterisks =
  do
    pieces <-
      many $
        choice
          [ try backtickQuote,
            try underscoreAsteriskBolds,
            takeWhile1P Nothing (\ch -> ch /= '\\' && ch /= '*' && ch /= '`'),
            chunk "\\*",
            do
              stars <- takeWhile1P Nothing (\ch -> ch == '*')
              return $ Text.replace "*" "\\*" stars,
            takeWhile1P Nothing (\ch -> ch == '*'),
            takeWhile1P Nothing (\ch -> ch == '\\'),
            takeWhile1P Nothing (\ch -> ch == '`')
          ]
    return $ mconcat pieces

underscoreBolds :: Parser Text
underscoreBolds =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    leading <- takeWhile1P Nothing (\ch -> ch == '_')
    firstPiece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '_')
    otherPieces <- many $
      try $
        do
          spaces <- takeWhileP Nothing (\ch -> ch == ' ')
          piece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '_')
          return $ spaces <> piece
    trailing <- takeWhile1P Nothing (\ch -> ch == '_')
    if Text.length leading /= Text.length trailing
      then fail "there must be equal numbers of trailing and leading underscores"
      else return $ leading <> firstPiece <> mconcat otherPieces <> trailing

underscoreAsteriskBolds :: Parser Text
underscoreAsteriskBolds =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    leading <- takeWhile1P Nothing (\ch -> ch == '*')
    firstPiece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '*')
    otherPieces <- many $
      try $
        do
          spaces <- takeWhileP Nothing (\ch -> ch == ' ')
          piece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '*')
          return $ spaces <> piece
    trailing <- takeWhile1P Nothing (\ch -> ch == '*')
    let leadingUnderscores = Text.replace "*" "_" leading
    let trailingUnderscores = Text.replace "*" "_" trailing
    if Text.length leading /= Text.length trailing
      then fail "there must be equal numbers of trailing and leading asterisks"
      else
        if Text.length leading == 1
          then return $ leadingUnderscores <> firstPiece <> mconcat otherPieces <> trailingUnderscores
          else return $ leading <> firstPiece <> mconcat otherPieces <> trailing

parseOrdinaryTextInDoc :: Parser Text
parseOrdinaryTextInDoc =
  fmap mconcat $
    some $
      choice
        [ takeWhile1P Nothing (\ch -> ch == ' ') >> return " ",
          takeWhile1P Nothing (\ch -> ch /= '@' && ch /= '-' && ch /= '\n' && ch /= ' '),
          do
            _ <- char '-'
            _ <- notFollowedBy $ char '}'
            return "-"
        ]

parseExportDocsRowOnly :: Int -> [[Text]] -> Parser [[Text]]
parseExportDocsRowOnly nesting accumulator =
  if nesting == 0
    then return $ reverse accumulator
    else
      choice
        [ do
            _ <- chunk "{-|"
            parseExportDocsRowOnly (nesting + 1) accumulator,
          do
            _ <- chunk "{-"
            parseExportDocsRowOnly (nesting + 1) accumulator,
          do
            _ <- char '{'
            parseExportDocsRowOnly nesting accumulator,
          do
            _ <- chunk "-}"
            parseExportDocsRowOnly (nesting - 1) accumulator,
          do
            _ <- char '}'
            parseExportDocsRowOnly nesting accumulator,
          do
            docs <- parseExportDocsRow
            parseExportDocsRowOnly nesting (docs : accumulator),
          do
            _ <- char '-'
            parseExportDocsRowOnly nesting accumulator,
          do
            _ <- takeWhile1P Nothing (\ch -> ch /= '@' && ch /= '{' && ch /= '}' && ch /= '-')
            parseExportDocsRowOnly nesting accumulator
        ]

parseExportDocsRow :: Parser [Text]
parseExportDocsRow =
  do
    _ <- chunk "@docs"
    _ <- takeWhile1P Nothing (\ch -> ch == ' ')
    some parseOneExportDoc

parseOneExportDoc :: Parser Text
parseOneExportDoc =
  do
    name' <- parseName
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- choice [char ',', lookAhead (char '\n')]
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return name'

parseExportRow :: Parser [Text]
parseExportRow =
  do
    items <- some parseSingleItemInExportRow
    _ <- space
    return items

parseSingleItemInExportRow :: Parser Text
parseSingleItemInExportRow =
  do
    _ <- choice [char ',', return ' ']
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    name' <- choice [parseName, parseInfixInBrackets]
    all_ <-
      choice
        [ do
            _ <- char '('
            _ <- takeWhileP Nothing (\ch -> ch /= ')')
            _ <- char ')'
            return "(..)",
          return ""
        ]
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- choice [char ',', return ' ']
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return $ name' <> all_

parseExposingHelp :: Int -> Parser (Bool, [([Text], Text)])
parseExposingHelp indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- some $
      do
        row <- parseExportRow
        comment <- commentSpaceParser 4
        return $ (row, comment)
    _ <- space
    _ <- char ')'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return (endRow > startRow, items)

parseExposing :: Int -> [[Text]] -> Parser Text
parseExposing indent docs =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- some $
      do
        row <- parseExportRow
        comment <- commentSpaceParser 4
        return $ (row, comment)
    _ <- space
    _ <- char ')'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $ formatExports indent (endRow > startRow) docs items

formatExportRow :: Int -> ([Text], Text) -> [Text] -> Text
formatExportRow indent (items, comment) docRow =
  let documented = filter (\doc -> elem doc items) docRow
   in mconcat
        [ intercalate ", " documented,
          if comment == ""
            then ""
            else "\n" <> pack (take indent (repeat ' ')),
          comment
        ]

formatExports :: Int -> Bool -> [[Text]] -> [([Text], Text)] -> Text
formatExports indent originalIsMultiline docs items =
  let unformattedRows = removeUndocumented (mconcat (map fst items)) docs
      rows = filter (\row -> row /= "") $ (map (formatExportRow (indent + 2) (mconcat items)) unformattedRows) <> undocumented
      undocumented :: [Text]
      undocumented = getUndocumented indent docs items
      isMultiline' = (not (null (removeUndocumented (mconcat (map fst items)) docs)) && length (mconcat (map fst items)) > 1) || originalIsMultiline
   in case rows of
        [] ->
          "()"
        [single] ->
          if Text.elem '\n' single
            then "\n" <> pack (take indent (repeat ' ')) <> "( " <> single <> "\n" <> pack (take indent (repeat ' ')) <> ")"
            else " (" <> single <> ")"
        multiple ->
          mconcat
            [ if isMultiline'
                then "\n" <> pack (take indent (repeat ' ')) <> "( "
                else " (",
              intercalate
                ( if isMultiline'
                    then "\n" <> pack (take indent (repeat ' ')) <> ", "
                    else ", "
                )
                multiple,
              if isMultiline'
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
    map (\name' -> Text.dropEnd 4 name') $
      filter (\name' -> Text.takeEnd 4 name' == "(..)") names

removeEmptyLists :: [[a]] -> [[a]]
removeEmptyLists items =
  filter (\item -> not (null item)) items

trimExposeAll :: Text -> Text
trimExposeAll text =
  if takeEnd 4 text == "(..)"
    then Text.dropEnd 4 text
    else text

getUndocumented :: Int -> [[Text]] -> [([Text], Text)] -> [Text]
getUndocumented indent docs items =
  let docSet = Set.fromList $ mconcat docs
   in List.sort
        $ map
          ( \(row, comment) ->
              mconcat
                [ intercalate ", " (List.sort row),
                  if comment == ""
                    then ""
                    else "\n" <> pack (take (indent + 2) (repeat ' ')),
                  comment
                ]
          )
        $ filter (\row -> not (null row))
        $ ( case docs of
              [] -> flattenExportRows
              _ -> id
          )
        $ map
          ( \(row, comment) ->
              ( filter (\item -> not (Set.member (trimExposeAll item) docSet)) row,
                comment
              )
          )
          items

flattenExportRows :: [([Text], Text)] -> [([Text], Text)]
flattenExportRows rows =
  flattenExportRowsHelp rows []

flattenExportRowsHelp :: [([Text], Text)] -> [([Text], Text)] -> [([Text], Text)]
flattenExportRowsHelp rows accumulator =
  case rows of
    [] ->
      reverse accumulator
    (row, comment) : remainder ->
      case reverse row of
        [] ->
          flattenExportRowsHelp remainder accumulator
        rowTop : rowRemainder ->
          flattenExportRowsHelp remainder ((reverse (([rowTop], comment) : map (\item -> ([item], "")) rowRemainder)) <> accumulator)

stripNewlinesStart :: Text -> Text
stripNewlinesStart unstripped =
  case Text.uncons unstripped of
    Nothing ->
      ""
    Just ('\n', remainder) ->
      stripNewlinesStart remainder
    Just _ ->
      unstripped

isUnorderedListItem :: Text -> Bool
isUnorderedListItem row =
  let stripped = Text.take 2 $ Text.stripStart row
   in stripped == "-" || stripped == "- "

isDigit :: Char -> Bool
isDigit ch =
  case ch of
    '0' ->
      True
    '1' -> True
    '2' -> True
    '3' -> True
    '4' -> True
    '5' -> True
    '6' -> True
    '7' -> True
    '8' -> True
    '9' -> True
    _ -> False

isNumberedListItem :: Text -> Bool
isNumberedListItem row =
  case Text.uncons row of
    Nothing ->
      False
    Just (first, remainder) ->
      isNumberedListItemAfterFirst remainder

isNumberedListItemAfterFirst :: Text -> Bool
isNumberedListItemAfterFirst remainder1 =
  case Text.uncons remainder1 of
    Nothing ->
      False
    Just ('.', _) ->
      True
    Just (first, remainder2) ->
      if isDigit first
        then isNumberedListItemAfterFirst remainder2
        else False

isListItem :: Text -> Bool
isListItem row =
  isNumberedListItem row || isUnorderedListItem row

removeTripleNewlinesInParagraphs :: [Text] -> [Text]
removeTripleNewlinesInParagraphs rows =
  removeTripleNewlinesHelp rows 0 []

removeTripleNewlinesHelp :: [Text] -> Int -> [Text] -> [Text]
removeTripleNewlinesHelp rows count accumulator =
  case rows of
    [] ->
      reverse accumulator
    "\n" : second : remainder ->
      if count == 2 && Text.take 1 second /= "#" && not (isListItem second) && Text.take 2 second /= "-}" && Text.take 4 second /= "    " && second /= "\n"
        then removeTripleNewlinesHelp (second : remainder) 0 accumulator
        else removeTripleNewlinesHelp (second : remainder) (count + 1) ("\n" : accumulator)
    top : remainder ->
      removeTripleNewlinesHelp remainder 0 (top : accumulator)

maxTwoNewlinesAfterCodeBlock :: [Text] -> [Text]
maxTwoNewlinesAfterCodeBlock rows =
  maxTwoNewlinesHelp rows 0 []

maxTwoNewlinesHelp :: [Text] -> Int -> [Text] -> [Text]
maxTwoNewlinesHelp rows count accumulator =
  case rows of
    [] ->
      reverse accumulator
    "\n" : remainder ->
      if count > 1
        then maxTwoNewlinesHelp remainder count accumulator
        else
          if count == 0
            then maxTwoNewlinesHelp remainder count ("\n" : accumulator)
            else maxTwoNewlinesHelp remainder (count + 1) ("\n" : accumulator)
    possibleCodeBlockRow : remainder ->
      if Text.take 4 possibleCodeBlockRow == "    "
        then maxTwoNewlinesHelp remainder 1 (possibleCodeBlockRow : accumulator)
        else maxTwoNewlinesHelp remainder 0 (possibleCodeBlockRow : accumulator)

formatElmInDocs :: [Text] -> [Text]
formatElmInDocs rows =
  formatElmInDocsHelp rows [] []

formatElmCodeInDocs :: Text -> Maybe Text
formatElmCodeInDocs unformatted =
  case parse parserInDocs "" unformatted of
    Left _ ->
      Nothing
    Right formatted ->
      Just formatted

parserInDocs :: Parser Text
parserInDocs =
  do
    moduleDeclaration <- parseModuleDeclarationNoCreate
    _ <- space
    parseAfterModuleDeclarationInDocComment moduleDeclaration

formatElmInDocsHelp :: [Text] -> [Text] -> [Text] -> [Text]
formatElmInDocsHelp rows accumulatedElm accumulated =
  case rows of
    [] ->
      if all (\ch -> ch == "\n") accumulatedElm
        then reverse (accumulatedElm <> accumulated)
        else reverse ((formatElmChunkInDocs (reverse accumulatedElm)) <> accumulated)
    top : remainder ->
      if Text.take 4 top == "    " || top == "\n"
        then formatElmInDocsHelp remainder (top : accumulatedElm) accumulated
        else case accumulatedElm of
          [] ->
            formatElmInDocsHelp remainder accumulatedElm (top : accumulated)
          _ ->
            let formatted = formatElmChunkInDocs (reverse accumulatedElm)
             in if all (\ch -> ch == "\n") accumulatedElm
                  then
                    formatElmInDocsHelp
                      remainder
                      []
                      ([top] <> accumulatedElm <> accumulated)
                  else formatElmInDocsHelp remainder [] ([top] <> formatted <> accumulated)

stripNewlines :: Text -> Text
stripNewlines string =
  stripLeadingNewlines (Text.reverse (stripLeadingNewlines (Text.reverse string)))

stripLeadingNewlines :: Text -> Text
stripLeadingNewlines string =
  case Text.uncons string of
    Nothing ->
      string
    Just ('\n', remainder) ->
      stripLeadingNewlines remainder
    Just (_, remainder) ->
      string

isValidElmBlock :: Text -> Bool
isValidElmBlock candidate =
  let rows = Text.lines candidate
      codeChunk = (Text.stripEnd $ Text.intercalate "\n" $ map (\row -> if row == "\n" then "\n" else Text.drop 4 row) (mconcat (map Text.lines rows))) <> "\n"
   in formatElmCodeInDocs codeChunk /= Nothing

formatElmChunkInDocs :: [Text] -> [Text]
formatElmChunkInDocs rows =
  case rows of
    [] ->
      []
    _ ->
      let leadingNewlines = getLeadingNewlines (mconcat rows)
          trailingNewlines = Text.drop 1 (getLeadingNewlines (Text.reverse (mconcat rows)))
          codeChunk = (Text.stripEnd $ Text.intercalate "\n" $ map (\row -> if row == "\n" then "\n" else Text.drop 4 row) (mconcat (map Text.lines rows))) <> "\n"
          formatted = case formatElmCodeInDocs codeChunk of
            Nothing ->
              codeChunk
            Just ok ->
              (Text.strip ok) <> "\n"
          formattedLines = Text.lines formatted
          indentedLines = map (\line -> if line == "" then "" else "    " <> line) formattedLines
          indentedFormatted = Text.replace "\n\n\n" "\n\n" $ Text.unlines indentedLines
       in [leadingNewlines <> indentedFormatted <> trailingNewlines]

getLeadingNewlines :: Text -> Text
getLeadingNewlines text =
  getLeadingNewlinesHelp text ""

getLeadingNewlinesHelp :: Text -> Text -> Text
getLeadingNewlinesHelp text newlines =
  case Text.uncons text of
    Nothing ->
      newlines
    Just ('\n', remainder) ->
      getLeadingNewlinesHelp remainder ("\n" <> newlines)
    Just _ ->
      newlines

addNewlineToTrailingCode :: [Text] -> [Text]
addNewlineToTrailingCode rows =
  case reverse rows of
    possiblyCodeBlock : previous ->
      case reverse (Text.lines possiblyCodeBlock) of
        possiblyCodeRow : remainder ->
          if Text.take 4 possiblyCodeRow == "    "
            then reverse previous <> [possiblyCodeBlock <> "\n"]
            else rows
        _ ->
          rows
    _ ->
      rows

backticksAroundCodeAfterOrderedList :: [Text] -> [Text]
backticksAroundCodeAfterOrderedList rows =
  backticksAroundCodeAfterOrderedListHelp rows []

backtickCodeChunkAfterOrderedList :: Text -> Text
backtickCodeChunkAfterOrderedList code =
  let lines = Text.lines code
      noIndent = map (Text.drop 4) lines
      unlines = Text.unlines noIndent
      stripped = Text.strip unlines
      extraNewline =
        case reverse (Text.lines (stripTrailingNewlines stripped)) of
          [] ->
            ""
          last : _ ->
            if Text.take 2 (Text.strip last) == "--"
              then "\n"
              else ""
   in "\n```" <> extraNewline <> "\n" <> stripped <> "\n```"

backticksAroundCodeAfterOrderedListHelp :: [Text] -> [Text] -> [Text]
backticksAroundCodeAfterOrderedListHelp rows accumulated =
  case rows of
    top : second : remainder ->
      if isListItem top && Text.take 6 second == "\n\n    "
        then
          backticksAroundCodeAfterOrderedListHelp
            remainder
            (backtickCodeChunkAfterOrderedList second : top : accumulated)
        else backticksAroundCodeAfterOrderedListHelp (second : remainder) (top : accumulated)
    _ ->
      (reverse accumulated) <> rows

backticksAroundCodeAfterList :: [Text] -> [Text]
backticksAroundCodeAfterList rows =
  backticksAroundCodeAfterListHelp rows []

backtickCodeChunkAfterList :: Text -> Text
backtickCodeChunkAfterList code =
  let lines = Text.lines code
      noIndent = map (Text.drop 4) lines
      unlines = Text.unlines noIndent
      stripped = Text.strip unlines
      extraNewline =
        case reverse (Text.lines (stripTrailingNewlines stripped)) of
          [] ->
            ""
          last : _ ->
            if Text.take 2 (Text.strip last) == "--"
              then "\n"
              else ""
   in "\n\n```\n" <> extraNewline <> stripped <> "\n```"

backticksAroundCodeAfterListHelp :: [Text] -> [Text] -> [Text]
backticksAroundCodeAfterListHelp rows accumulated =
  case rows of
    top : second : remainder ->
      if isListItem top
        && Text.take 4 (stripLeadingNewlines $ mconcat (second : remainder)) == "    "
        then
          backticksAroundCodeAfterListHelp
            remainder
            (backtickCodeChunkAfterList second : top : accumulated)
        else backticksAroundCodeAfterListHelp (second : remainder) (top : accumulated)
    _ ->
      (reverse accumulated) <> rows

newlinesAfterBackticks :: Text -> Text
newlinesAfterBackticks item =
  if Text.takeEnd 3 item == "```"
    then item <> "\n\n"
    else item

stripSpaces :: Text -> Text
stripSpaces text =
  stripLeadingSpaces (Text.reverse (stripLeadingSpaces (Text.reverse text)))

stripLeadingSpaces :: Text -> Text
stripLeadingSpaces text =
  case Text.uncons text of
    Nothing ->
      text
    Just (' ', remainder) ->
      stripLeadingSpaces remainder
    Just _ ->
      text

stripOrdinaryDocRow :: Text -> Text
stripOrdinaryDocRow row =
  if Text.take 4 row == "    " || isListItem row
    then row
    else stripSpaces row

stripLeadingSpacesFromDocRow :: [Text] -> [Text]
stripLeadingSpacesFromDocRow rows =
  case rows of
    [] ->
      []
    top : remainder ->
      top : map stripOrdinaryDocRow remainder

trimTrailingNewlines :: [Text] -> [Text]
trimTrailingNewlines rows =
  case reverse rows of
    "\n" : "\n" : maybeNotNewline : remainder ->
      if Text.takeEnd 1 maybeNotNewline == "\n"
        then trimTrailingNewlines ((reverse remainder) <> [Text.stripEnd maybeNotNewline, "\n", "\n"])
        else rows
    _ ->
      rows

stripTrailingNewlines :: Text -> Text
stripTrailingNewlines text =
  Text.reverse (stripLeadingNewlines (Text.reverse text))

lastIsLineComment :: Text -> Bool
lastIsLineComment codeBlock =
  case reverse $ Text.lines (stripTrailingNewlines codeBlock) of
    [] ->
      False
    last : _ ->
      Text.take 6 last == "    --"

onlyContainsComments :: Text -> Bool
onlyContainsComments block =
  let lines = Text.lines block
      noEmpty = filter (\line -> Text.strip line /= "") lines
      stripped = map Text.strip noEmpty
   in all (\line -> Text.take 2 line == "--") stripped

stripTooManyNewlinesBeforeCodeBlock :: Text -> Text
stripTooManyNewlinesBeforeCodeBlock maybeBlock =
  let extraNewline :: Text
      extraNewline = if lastIsLineComment maybeBlock && isValidElmBlock maybeBlock && not (onlyContainsComments maybeBlock) then "\n" else ""
   in if Text.take 4 (stripLeadingNewlines maybeBlock) == "    "
        then extraNewline <> "\n\n" <> stripLeadingNewlines maybeBlock
        else maybeBlock

stripTooManyNewlinesBeforeCodeBlocks :: [Text] -> [Text]
stripTooManyNewlinesBeforeCodeBlocks rows =
  map stripTooManyNewlinesBeforeCodeBlock rows

addExtraNewlinesAfterEndingBlockQuote :: [Text] -> [Text]
addExtraNewlinesAfterEndingBlockQuote rows =
  case reverse rows of
    [] ->
      []
    "\n" : last : remainder ->
      if Text.take 2 last == "> "
        then rows <> ["\n"]
        else rows
    last : remainder ->
      if Text.take 2 last == "> "
        then rows <> ["\n", "\n"]
        else rows

addExtraNewlinesAfterEndingCodeBlock :: [Text] -> [Text]
addExtraNewlinesAfterEndingCodeBlock rows =
  case reverse rows of
    [] ->
      []
    last : remainder ->
      case reverse (Text.lines (Text.stripEnd last)) of
        [] ->
          rows
        lastInLast : remainderOfLast ->
          if Text.take 6 lastInLast == "    --" && isValidElmBlock last
            then rows <> ["\n", "\n"]
            else rows

addExtraNewlinesOnStartingCodeBlock :: [Text] -> [Text]
addExtraNewlinesOnStartingCodeBlock rows =
  case rows of
    [] ->
      []
    top : remainder ->
      case ( Text.take 6 (stripLeadingNewlines top) == "    {-",
             lastIsLineComment top,
             isValidElmBlock top
           ) of
        (True, False, _) ->
          "\n\n\n\n" <> (stripLeadingNewlines top) : remainder
        (False, True, True) ->
          "\n\n\n" <> (stripLeadingNewlines top) : remainder
        (False, True, False) ->
          "\n\n" <> (stripLeadingNewlines top) : remainder
        (True, True, _) ->
          "\n\n" <> (stripLeadingNewlines top) : remainder
        _ -> rows

emptyLineBeforeNumberedList :: [Text] -> [Text]
emptyLineBeforeNumberedList rows =
  emptyLineBeforeNumberedListHelp rows []

emptyLineBeforeNumberedListHelp :: [Text] -> [Text] -> [Text]
emptyLineBeforeNumberedListHelp rows accumulated =
  case rows of
    [] ->
      reverse accumulated
    top : second : third : remainder ->
      if top /= "\n" && second == "\n" && isListItem third
        then emptyLineBeforeNumberedListHelp remainder (third : second : "\n" : top : accumulated)
        else
          emptyLineBeforeNumberedListHelp
            (second : third : remainder)
            (top : accumulated)
    top : remainder ->
      emptyLineBeforeNumberedListHelp
        remainder
        (top : accumulated)

removeSingleAsterisk :: [Text] -> [Text]
removeSingleAsterisk rows =
  if Text.strip (mconcat rows) == "*" then [] else rows

atLeastTwoNewlinesBeforeBlockQuote :: [Text] -> [Text]
atLeastTwoNewlinesBeforeBlockQuote rows =
  case atLeastTwoNewlinesBeforeBlockQuoteHelp rows [] of
    first : remainder ->
      if Text.take 2 first == "> "
        then "\n\n" <> first : remainder
        else first : remainder
    other ->
      other

atLeastTwoNewlinesBeforeBlockQuoteHelp :: [Text] -> [Text] -> [Text]
atLeastTwoNewlinesBeforeBlockQuoteHelp rows accumulated =
  case rows of
    [] ->
      reverse accumulated
    first : "\n" : third : remainder ->
      if first /= "\n" && Text.take 2 third == "> "
        then
          atLeastTwoNewlinesBeforeBlockQuoteHelp
            (third : remainder)
            ("\n" : "\n" : first : accumulated)
        else
          atLeastTwoNewlinesBeforeBlockQuoteHelp
            ("\n" : third : remainder)
            (first : accumulated)
    first : remainder ->
      atLeastTwoNewlinesBeforeBlockQuoteHelp remainder (first : accumulated)

atLeastTwoNewlinesBeforeAtDocs :: [Text] -> [Text]
atLeastTwoNewlinesBeforeAtDocs rows =
  case atLeastTwoNewlinesBeforeAtDocsHelp rows [] of
    "\n" : second : remainder ->
      if Text.take 6 second == "@docs "
        then "\n" : "\n" : second : remainder
        else "\n" : second : remainder
    other ->
      other

atLeastTwoNewlinesBeforeAtDocsHelp :: [Text] -> [Text] -> [Text]
atLeastTwoNewlinesBeforeAtDocsHelp rows accumulated =
  case rows of
    [] ->
      reverse accumulated
    first : "\n" : third : remainder ->
      if first /= "\n" && Text.take 6 first /= "@docs " && Text.take 6 third == "@docs "
        then
          atLeastTwoNewlinesBeforeAtDocsHelp
            (third : remainder)
            ("\n" : "\n" : first : accumulated)
        else
          atLeastTwoNewlinesBeforeAtDocsHelp
            ("\n" : third : remainder)
            (first : accumulated)
    first : remainder ->
      atLeastTwoNewlinesBeforeAtDocsHelp remainder (first : accumulated)

stripTooManyNewlinesBetweenHeaderAndCode :: [Text] -> [Text]
stripTooManyNewlinesBetweenHeaderAndCode rows =
  stripTooManyNewlinesBetweenHeaderAndCodeHelp rows []

stripTooManyNewlinesBetweenHeaderAndCodeHelp :: [Text] -> [Text] -> [Text]
stripTooManyNewlinesBetweenHeaderAndCodeHelp rows accum =
  case rows of
    maybeHeader : maybeCode : remainder ->
      if Text.take 1 (stripLeadingNewlines maybeHeader) == "#" && Text.take 6 maybeCode == "\n\n    "
        then
          stripTooManyNewlinesBetweenHeaderAndCodeHelp
            remainder
            (maybeCode : Text.stripEnd maybeHeader : accum)
        else
          stripTooManyNewlinesBetweenHeaderAndCodeHelp
            (maybeCode : remainder)
            (maybeHeader : accum)
    [] ->
      reverse accum
    [top] ->
      reverse (top : accum)

removeTooManyNewlinesAfterAtDocsAfterHeader :: [Text] -> [Text]
removeTooManyNewlinesAfterAtDocsAfterHeader rows =
  removeTooManyNewlinesAfterAtDocsAfterHeaderHelp rows []

removeTooManyNewlinesAfterAtDocsAfterHeaderHelp :: [Text] -> [Text] -> [Text]
removeTooManyNewlinesAfterAtDocsAfterHeaderHelp rows accum =
  case rows of
    [] ->
      reverse accum
    [only] ->
      reverse (only : accum)
    top : second : remainder ->
      if Text.take 6 top == "@docs "
        && Text.isInfixOf "#" top
        && Text.take 6 second == "\n\n    "
        then
          removeTooManyNewlinesAfterAtDocsAfterHeaderHelp
            remainder
            (second : Text.dropEnd 2 top : accum)
        else
          removeTooManyNewlinesAfterAtDocsAfterHeaderHelp
            (second : remainder)
            (top : accum)

removeTooManyNewlinesAfterAtDocs :: [Text] -> [Text]
removeTooManyNewlinesAfterAtDocs rows =
  removeTooManyNewlinesAfterAtDocsHelp rows []

removeTooManyNewlinesAfterAtDocsHelp :: [Text] -> [Text] -> [Text]
removeTooManyNewlinesAfterAtDocsHelp rows accum =
  case rows of
    [] ->
      reverse accum
    [only] ->
      reverse (only : accum)
    top : second : remainder ->
      if Text.take 6 top == "@docs " && Text.take 6 second == "\n\n    "
        then
          removeTooManyNewlinesAfterAtDocsHelp
            remainder
            (second : Text.dropEnd 1 top : accum)
        else
          removeTooManyNewlinesAfterAtDocsHelp
            (second : remainder)
            (top : accum)

removeTooManyTrailingEmptyLines :: [Text] -> [Text]
removeTooManyTrailingEmptyLines rows =
  if any (\row -> Text.take 6 (stripLeadingNewlines row) == "@docs ") rows
    then rows
    else removeTooManyTrailingEmptyLinesHelp (reverse rows)

removeTooManyTrailingEmptyLinesHelp :: [Text] -> [Text]
removeTooManyTrailingEmptyLinesHelp rows =
  case rows of
    "\n" : "\n" : third : remainder ->
      if Text.take 4 third /= "    "
        && Text.take 2 (stripLeadingNewlines third) /= "> "
        && not (Text.elem '\n' third)
        && Text.take 2 (Text.strip third) /= "- "
        && third /= "}"
        && third /= "{"
        then removeTooManyTrailingEmptyLinesHelp ("\n" : third : remainder)
        else reverse rows
    _ ->
      reverse rows

formatDocContainingOnlyLineComment :: [Text] -> [Text]
formatDocContainingOnlyLineComment rows =
  case filter (\row -> Text.strip row /= "") rows of
    [] ->
      rows
    [only] ->
      if Text.take 6 (stripLeadingNewlines only) == "    --"
        then ["\n\n    " <> Text.strip only <> "\n\n\n\n"]
        else rows
    _ ->
      rows

lastIsCommentOnlyCodeBlock :: Text -> Bool
lastIsCommentOnlyCodeBlock text =
  lastIsCommentOnlyCodeBlockHelp (reverse (Text.lines text)) False

lastIsCommentOnlyCodeBlockHelp :: [Text] -> Bool -> Bool
lastIsCommentOnlyCodeBlockHelp reversedLines accum =
  case reversedLines of
    [] ->
      accum
    "" : remainder ->
      lastIsCommentOnlyCodeBlockHelp remainder accum
    "\n" : remainder ->
      lastIsCommentOnlyCodeBlockHelp remainder accum
    top : remainder ->
      if Text.take 6 top == "    --"
        then lastIsCommentOnlyCodeBlockHelp remainder True
        else
          if Text.take 4 top == "    "
            then False
            else accum

parseModuleDocsInner :: Parser Text
parseModuleDocsInner =
  do
    rows <-
      fmap (filter (\row -> stripSpaces row /= "")) $
        fmap formatDocContainingOnlyLineComment $
          fmap removeTooManyTrailingEmptyLines $
            fmap atLeastTwoNewlinesBeforeBlockQuote $
              fmap atLeastTwoNewlinesBeforeAtDocs $
                fmap addExtraNewlinesAfterEndingBlockQuote $
                  fmap addExtraNewlinesAfterEndingCodeBlock $
                    fmap emptyLineBeforeNumberedList $
                      fmap addExtraNewlinesOnStartingCodeBlock $
                        fmap stripTooManyNewlinesBetweenHeaderAndCode $
                          fmap removeTooManyNewlinesAfterAtDocsAfterHeader $
                            fmap stripTooManyNewlinesBeforeCodeBlocks $
                              fmap trimTrailingNewlines $
                                fmap stripLeadingSpacesFromDocRow $
                                  fmap (map newlinesAfterBackticks) $
                                    fmap backticksAroundCodeAfterList $
                                      fmap addNewlineToTrailingCode $
                                        fmap removeTooManyNewlinesAfterAtDocs $
                                          fmap formatElmInDocs $
                                            fmap maxTwoNewlinesAfterCodeBlock $
                                              fmap removeTripleNewlinesInParagraphs $
                                                some $
                                                  try parseDocRow

    let stripped = stripNewlinesStart $ mconcat rows
        first = Text.take 1 stripped
        flat = if first == "#" || first == ">" || first == "-" || first == "@" || Text.take 4 stripped == "    " || isListItem stripped then mconcat rows else " " <> (Text.stripStart $ mconcat rows)
        firstIsList = case filter (\item -> Text.strip item /= "") rows of
          [] -> False
          top : _ -> isListItem top
    return $
      if Text.strip (mconcat rows) == ""
        then "\n\n"
        else
          if Text.isInfixOf "@docs" flat
            then flat
            else
              if Text.take 2 flat == "\n\n" || Text.elem '\n' (Text.strip flat)
                then
                  ( if Text.count "\n\n" (Text.strip flat) == 0 && not firstIsList
                      then flat
                      else
                        if lastIsCommentOnlyCodeBlock flat
                          then flat
                          else Text.stripEnd flat <> "\n\n"
                  )
                else
                  if firstIsList
                    then "\n\n" <> stripNewlinesStart flat
                    else Text.stripEnd flat <> "\n"

parseModuleDocsHelp :: Int -> Text -> Parser Text
parseModuleDocsHelp nesting contents =
  if nesting == 0
    then return contents
    else do
      choice
        [ do
            _ <- chunk "{-"
            parseModuleDocsHelp (nesting + 1) (contents <> "{-"),
          do
            _ <- try $ do
              _ <- takeWhileP Nothing (\ch -> ch == ' ')
              chunk "-}"
            parseModuleDocsHelp
              (nesting - 1)
              ( if Text.strip contents == "{-"
                  || Text.strip contents == "{-|"
                  then Text.strip contents <> " -}"
                  else
                    if Text.strip (Text.drop 3 contents) == "-" || Text.strip (Text.drop 3 contents) == "\\*"
                      then "{-|\n\n\n-}"
                      else contents <> "-}"
              ),
          try $ do
            piece <- parseModuleDocsInner
            parseModuleDocsHelp nesting (contents <> piece),
          do
            piece <- fmap escapeAsterisks $ takeWhile1P Nothing (\ch -> ch /= '-' && ch /= '{')
            parseDocumentationHelp nesting (contents <> (if Text.takeEnd 3 contents == "{-|" && Text.take 1 piece /= " " then " " else "") <> piece),
          do
            _ <- char '-'
            parseModuleDocsHelp nesting (contents <> "-"),
          do
            _ <- char '{'
            parseModuleDocsHelp nesting (contents <> "{")
        ]

parseDocComment :: Parser Text
parseDocComment =
  do
    _ <- chunk "{-|"
    parseModuleDocsHelp 1 "{-|"

parseModuleDeclaration :: Parser Text
parseModuleDeclaration =
  do
    port <-
      choice
        [ do
            _ <- chunk "port"
            _ <- space
            return ("port " :: Text),
          return ""
        ]
    _ <- chunk "module"
    commentAfterModule <- commentSpaceParser 4
    name' <- parseName
    commentAfterName <- commentSpaceParser 4
    _ <- chunk "exposing"
    commentAfterExposing <- commentSpaceParser 4
    docs <-
      choice
        [ lookAhead $ try parseExportDocs,
          return []
        ]

    exports <- parseExposing 4 docs
    _ <- space
    moduleDocs <- choice [parseDocComment, return ""]
    _ <- space
    return $
      mconcat
        [ port,
          "module",
          if commentAfterModule == "" then " " else "\n    " <> commentAfterModule <> "\n    ",
          name',
          if commentAfterName == "" then "" else "\n    ",
          commentAfterName,
          if commentAfterModule == "" && commentAfterName == "" && commentAfterExposing == "" then " " else "\n    ",
          "exposing",
          if commentAfterExposing == "" then "" else "\n    ",
          commentAfterExposing,
          if commentAfterModule == "" && commentAfterName == "" && commentAfterExposing == "" then "" else "\n   ",
          exports,
          if moduleDocs == ""
            then ""
            else "\n\n" <> moduleDocs
        ]

parseTopLevelNames :: Parser [Text]
parseTopLevelNames =
  do
    names <-
      fmap (\items -> filter (\item -> item /= "") items) $
        many $
          choice
            [ do
                _ <- parseImport
                _ <- space
                return "",
              try getTypeAliasDeclarationName,
              try getCustomTypeDeclarationName,
              try (parseSectionComment >> return ""),
              try getPortDeclarationName,
              try getTopLevelBindName,
              (parseDocCommentAtEndOfModule >> return "")
            ]
    _ <- eof
    return names

createModuleDeclaration :: Parser Text
createModuleDeclaration =
  do
    topLevelNames <- lookAhead parseTopLevelNames
    return $
      mconcat
        [ "module Main exposing (",
          intercalate ", " (List.sort topLevelNames),
          ")"
        ]

parseModuleDeclarationNoCreate :: Parser Text
parseModuleDeclarationNoCreate =
  do
    _ <- space
    commentBefore <- choice [parseTopLevelComment, return ""]
    declaration <-
      choice
        [ parseModuleDeclaration,
          return ""
        ]
    _ <- space
    title <- choice [parseTitle, return ""]
    return $
      mconcat
        [ commentBefore,
          if commentBefore == ""
            then ""
            else "\n\n\n",
          declaration,
          if title == ""
            then ""
            else "\n" <> title
        ]

parseModuleDeclarationWithTitle :: Parser Text
parseModuleDeclarationWithTitle =
  do
    _ <- space
    commentBefore <- choice [parseTopLevelComment, return ""]
    declaration <-
      choice
        [ parseModuleDeclaration,
          createModuleDeclaration
        ]
    _ <- space
    title <- choice [parseTitle, return ""]
    return $
      mconcat
        [ commentBefore,
          if commentBefore == ""
            then ""
            else "\n\n\n",
          declaration,
          if title == ""
            then ""
            else "\n" <> title
        ]

getTypeAliasDeclarationName :: Parser Text
getTypeAliasDeclarationName =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    _ <- "type"
    _ <- space
    _ <- "alias"
    _ <- space
    name' <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- space
    _ <- char '='
    comment <- commentSpaceParser 4
    type_ <- parseAliasedType 4
    _ <- space
    return name'

parseTypeAliasDeclaration :: Parser Text
parseTypeAliasDeclaration =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    _ <- "type"
    _ <- space1
    _ <- "alias"
    _ <- space
    name' <- parseName
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
          name',
          if parameters == ""
            then ""
            else " ",
          parameters,
          " =\n    ",
          comment,
          if comment == "" then "" else "\n    ",
          type_
        ]

getCustomTypeDeclarationName :: Parser Text
getCustomTypeDeclarationName =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    _ <- "type"
    _ <- space
    name' <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- space
    firstBranch <- parseBranch '='
    nextBranches <- many (parseBranch '|')
    return (name' <> "(..)")

parseCustomTypeDeclaration :: Parser Text
parseCustomTypeDeclaration =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    _ <- "type"
    _ <- space1
    name' <- parseName
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
          name',
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
        commentBefore <- commentSpaceParser 6
        branchName <- parseName
        afterNameRow <- fmap (unPos . sourceLine) getSourcePos
        startColumn <- fmap (unPos . sourceColumn) getSourcePos
        if startColumn == 1
          then fail "column is too low"
          else
            choice
              [ try $
                  parseBranchParametersWithComments
                    commentBefore
                    branchName
                    afterNameRow,
                parseBranchNoParameters
                  commentBefore
                  branchName
                  afterNameRow
              ]

parseBranchNoParameters :: Text -> Text -> Int -> Parser Text
parseBranchNoParameters commentBefore branchName afterNameRow =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    afterEmptySpaceRow <- fmap (unPos . sourceLine) getSourcePos
    afterEmptySpaceColumn <- fmap (unPos . sourceColumn) getSourcePos
    commentAfter <-
      if afterEmptySpaceColumn > 1
        then commentSpaceParser 6
        else return ""
    return $
      mconcat
        [ commentBefore,
          if commentBefore == ""
            then ""
            else
              if not (Text.elem '\n' commentBefore)
                && Text.take 2 commentBefore == "{-"
                then " "
                else "\n      ",
          branchName,
          if commentAfter == ""
            then ""
            else
              ( if afterEmptySpaceRow == afterNameRow
                  then " "
                  else "\n      "
              )
                <> commentAfter
        ]

parseBranchParametersWithComments :: Text -> Text -> Int -> Parser Text
parseBranchParametersWithComments commentBefore branchName afterNameRow =
  do
    commentAfterName <- commentSpaceParser 6
    parameters <- parseTypeDeclarationParameters 2
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    afterEmptySpaceRow <- fmap (unPos . sourceLine) getSourcePos
    commentAfter <-
      do
        afterEmptySpaceColumn <- fmap (unPos . sourceColumn) getSourcePos
        if afterEmptySpaceColumn == 1
          then return ""
          else commentSpaceParser 6
    return $
      mconcat
        [ commentBefore,
          if commentBefore == ""
            then ""
            else
              if not (Text.elem '\n' commentBefore)
                && Text.take 2 commentBefore == "{-"
                then " "
                else "\n      ",
          branchName,
          if commentAfterName == ""
            then ""
            else
              if parameters == ""
                then ""
                else
                  if afterEmptySpaceRow == afterNameRow
                    then " "
                    else "\n        ",
          if parameters == ""
            then ""
            else commentAfterName,
          if commentAfterName == ""
            then ""
            else
              if parameters == ""
                then ""
                else
                  if afterEmptySpaceRow == afterNameRow
                    then " "
                    else "\n       ",
          if parameters == ""
            then ""
            else
              if Text.elem '\n' (branchName <> parameters)
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

isEmptyDocComment :: Text -> Bool
isEmptyDocComment doc =
  Text.strip (Text.drop 3 (Text.dropEnd 2 doc)) == ""

trimTrailingSpaces :: Text -> Text
trimTrailingSpaces string =
  Text.strip (Text.unlines (map Text.stripEnd (Text.lines string)))

parseDocumentationHelp :: Int -> Text -> Parser Text
parseDocumentationHelp nesting contents =
  if nesting == 0
    then return (trimTrailingSpaces contents)
    else do
      choice
        [ do
            _ <- chunk "{-"
            parseDocumentationHelp (nesting + 1) (contents <> "{-"),
          do
            _ <- chunk "-}"
            let newContents = contents <> (if Text.takeEnd 1 contents == "\n" || Text.takeEnd 2 (Text.strip contents) == "{-" || nesting > 1 then "" else "\n") <> "-}"
                cleanEmpty = if isEmptyDocComment newContents then "{-| -}" else newContents
            parseDocumentationHelp (nesting - 1) cleanEmpty,
          do
            piece <- takeWhile1P Nothing (\ch -> ch /= '-' && ch /= '{')
            parseDocumentationHelp nesting (contents <> piece),
          do
            _ <- char '-'
            parseDocumentationHelp nesting (contents <> "-"),
          do
            _ <- char '{'
            parseDocumentationHelp nesting (contents <> "{")
        ]

getTopLevelBindName :: Parser Text
getTopLevelBindName =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    signature <- choice [try $ parseTypeSignature 1 0, return ""]
    _ <- space
    name' <- parseName
    _ <- space
    parameters <- parseParameters 0
    _ <- char '='
    commentBeforeExpression <- commentSpaceParser 4
    expression <- parseExpression 2 DoesntNeedBrackets 4
    _ <- space
    return name'

parseTopLevelBind :: Parser Text
parseTopLevelBind =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    signature <- choice [try $ parseTypeSignature 1 0, return ""]
    _ <- space
    name' <- parseName
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
          name',
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
    startRow_ <- fmap (unPos . sourceLine) getSourcePos
    parameters <-
      some $
        try $
          do
            _ <- space
            parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
            if parameterColumn <= startColumn
              then fail "invalid indentation"
              else try $ do
                startRow <- fmap (unPos . sourceLine) getSourcePos
                commentBefore <- commentSpaceParser 8
                endRow <- fmap (unPos . sourceLine) getSourcePos
                parameter <- parseType 2 8
                return $
                  mconcat
                    [ commentBefore,
                      if commentBefore == ""
                        then ""
                        else
                          if startRow == endRow
                            then " "
                            else "\n        ",
                      parameter
                    ]
    endRow_ <- fmap (unPos . sourceLine) getSourcePos
    return $ intercalate (if endRow_ == startRow_ then " " else "\n        ") parameters

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
              parameter <- parseParameter startColumn 0
              _ <- space
              return parameter
    return $ intercalate " " parameters

getTypeSignatureName :: Int -> Int -> Parser Text
getTypeSignatureName startColumn indent =
  do
    name' <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    startRow <- fmap (unPos . sourceLine) getSourcePos
    type_ <- parseBareFunctionType startColumn (indent + 4)
    endRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- space
    return name'

parseTypeSignature :: Int -> Int -> Parser Text
parseTypeSignature startColumn indent =
  do
    name' <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    startRow <- fmap (unPos . sourceLine) getSourcePos
    comment <- commentSpaceParser (indent + 4)
    type_ <- parseBareFunctionType startColumn (indent + 4)
    endRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- space
    let gap =
          if endRow > startRow
            then "\n" <> pack (take (indent + 4) (repeat ' '))
            else " "
    return $
      mconcat
        [ name',
          " :",
          gap,
          comment,
          if Text.length comment == 0
            then ""
            else gap,
          type_
        ]

withMinColumn :: Int -> Parser Text -> Parser Text
withMinColumn minColumn p =
  do
    startColumn <- fmap (unPos . sourceColumn) getSourcePos
    if startColumn < minColumn
      then fail "column too low"
      else p

parseTypeArgumentInUnnecessaryParensHelp :: Int -> Int -> Parser Text
parseTypeArgumentInUnnecessaryParensHelp minColumn indent =
  choice
    [ try parseEmptyRecord,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      withMinColumn minColumn parseVerbatim
    ]

parseTypeWhenNoParensNeeded :: Int -> Int -> Parser Text
parseTypeWhenNoParensNeeded minColumn indent =
  choice
    [ try $ parseTypeWithArguments indent,
      try parseEmptyRecord,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      withMinColumn minColumn parseVerbatim
    ]

parseTypeInFunctionType :: Int -> Int -> Parser Text
parseTypeInFunctionType minColumn indent =
  choice
    [ try $ parseTypeWithArguments indent,
      try parseEmptyRecord,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      try $ parseTypeInUnnecessaryParens indent,
      try $ parseFunctionType minColumn indent,
      parseTupleType indent,
      withMinColumn minColumn parseVerbatim
    ]

parseType :: Int -> Int -> Parser Text
parseType minColumn indent =
  choice
    [ try $ parseTypeWithArguments indent,
      try parseEmptyRecord,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      try $ parseFunctionType minColumn indent,
      parseTupleType indent,
      withMinColumn minColumn parseVerbatim
    ]

parseAliasedType :: Int -> Parser Text
parseAliasedType indent =
  choice
    [ try $ parseBareFunctionType 2 indent,
      try $ parseTypeWithArguments indent,
      try parseEmptyRecord,
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

parseOneFunctionTypeItem :: Int -> Int -> Parser Text
parseOneFunctionTypeItem minColumn indent =
  do
    column <- fmap (unPos . sourceColumn) getSourcePos
    if column < minColumn
      then fail "too far left"
      else do
        type_ <- parseTypeInFunctionType minColumn indent
        choice
          [ try $ do
              _ <- space
              _ <- chunk "->"
              _ <- space
              return type_,
            return type_
          ]

parseBareFunctionType :: Int -> Int -> Parser Text
parseBareFunctionType minColumn indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    first <- parseOneFunctionTypeItem minColumn indent
    remainder <- many $ parseOneFunctionTypeItem minColumn (floorToFour (indent + 4))
    let types = first : remainder
    endRow <- fmap (unPos . sourceLine) getSourcePos
    if endRow == startRow
      then return $ intercalate " -> " types
      else
        return $
          mconcat $
            mconcat
              [ [first],
                map
                  ( \item ->
                      if Text.elem '\n' item
                        then "\n" <> pack (take indent (repeat ' ')) <> "->\n" <> (pack $ take (floorToFour (indent + 4)) (repeat ' ')) <> item
                        else "\n" <> pack (take indent (repeat ' ')) <> "-> " <> item
                  )
                  remainder
              ]

parseFunctionType :: Int -> Int -> Parser Text
parseFunctionType minColumn indent =
  do
    _ <- char '('
    _ <- space
    bare <- parseBareFunctionType minColumn (indent + 1)
    _ <- space
    _ <- char ')'
    return $
      mconcat
        [ "(",
          bare,
          if Text.elem '\n' bare
            then "\n" <> pack (take indent (repeat ' '))
            else "",
          ")"
        ]

parseTypeArgumentInUnnecessaryParens :: Int -> Parser Text
parseTypeArgumentInUnnecessaryParens indent =
  do
    startLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- parseSpaces
    item <- try $ parseTypeArgumentInUnnecessaryParensHelp 1 (indent + 2)
    _ <- space
    _ <- char ')'
    endLine <- fmap (unPos . sourceLine) getSourcePos
    return item

parseTypeInUnnecessaryParens :: Int -> Parser Text
parseTypeInUnnecessaryParens indent =
  do
    startLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- parseSpaces
    item <- try $ parseTypeWhenNoParensNeeded 1 (indent + 2)
    _ <- space
    _ <- char ')'
    endLine <- fmap (unPos . sourceLine) getSourcePos
    return item

parseTupleType :: Int -> Parser Text
parseTupleType indent =
  do
    startLine <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- parseSpaces
    items <- many $
      try $
        do
          _ <- space
          choice
            [ try $ parseTupleTypeItem (indent + 2),
              parseBareFunctionTypeInTuple 2 (indent + 2)
            ]
    _ <- space
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
                  intercalate ("\n" <> replicate indent " " <> ", ") items,
                  "\n" <> replicate indent " " <> ")"
                ]
          else
            return $
              mconcat
                [ "( ",
                  intercalate ", " items,
                  " )"
                ]

parseBareFunctionTypeInTuple :: Int -> Int -> Parser Text
parseBareFunctionTypeInTuple minColumn indent =
  do
    _ <- space
    type_ <- parseBareFunctionType minColumn indent
    _ <- space
    _ <- choice [char ',', lookAhead (char ')')]
    return type_

parseTupleTypeItem :: Int -> Parser Text
parseTupleTypeItem indent =
  do
    _ <- space
    type_ <- parseType 1 indent
    _ <- space
    _ <- choice [char ',', lookAhead (char ')')]
    return type_

parseExtensibleRecordType :: Int -> Parser Text
parseExtensibleRecordType indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '{'
    _ <- space
    name' <- parseName
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
              name',
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

parseRecordTypeItem :: Int -> Parser Text
parseRecordTypeItem indent =
  do
    commentBefore <- commentSpaceParser indent
    startRow <- fmap (unPos . sourceLine) getSourcePos
    name' <- parseName
    _ <- space
    _ <- char ':'
    _ <- space
    right <-
      choice
        [ try $ parseNotFollowedByArrow $ parseType 1 (indent + 2),
          parseBareFunctionType 2 (indent + 2)
        ]
    endRow <- fmap (unPos . sourceLine) getSourcePos
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser (indent - 2)
    _ <- space
    _ <- choice [char ',', lookAhead (char '}')]
    _ <- space
    return $
      mconcat
        [ commentBefore,
          if commentBefore == "" then "" else "\n" <> pack (take indent (repeat ' ')),
          name',
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

parseTypeWithArguments :: Int -> Parser Text
parseTypeWithArguments indent =
  do
    startColumn <- fmap (unPos . sourceColumn) getSourcePos
    startRow <- fmap (unPos . sourceLine) getSourcePos
    name' <- parseName
    _ <- space
    afterSpaceRow <- fmap (unPos . sourceLine) getSourcePos
    parameters <- parseTypeArguments startColumn (floorToFour (indent + 4))
    if parameters == ""
      then return name'
      else
        return $
          ( name'
              <> ( if afterSpaceRow > startRow
                     then "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
                     else " "
                 )
              <> parameters
          )

parseTypeArguments :: Int -> Int -> Parser Text
parseTypeArguments minColumn indent =
  do
    _ <- space
    startRow <- fmap (unPos . sourceLine) getSourcePos
    parameters <-
      some $
        try $ do
          _ <- space
          parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
          if parameterColumn <= minColumn
            then fail "invalid indentation"
            else do
              parameter <- parseTypeParameter minColumn indent
              return parameter
    endRow <- fmap (unPos . sourceLine) getSourcePos
    let inBetween =
          if endRow > startRow
            then "\n" <> replicate indent " "
            else " "
    return $ intercalate inBetween parameters

parseImport :: Parser Text
parseImport =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- chunk "import"
    _ <- space1
    commentBetween' <- commentSpaceParser 1
    name' <- parseName
    _ <- space
    as' <-
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
    exposing' <-
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
          commentBetween',
          if commentBetween' == "" then "" else " ",
          name',
          if as' == "" || as' == name'
            then ""
            else " as " <> as',
          if exposing' == ""
            then ""
            else
              if endRow == startRow
                then " exposing" <> exposing'
                else "\n    exposing" <> exposing'
        ]

parser :: Parser Text
parser =
  do
    moduleDeclaration <- parseModuleDeclarationWithTitle
    _ <- space
    parseAfterModuleDeclaration moduleDeclaration

withTrailing :: Int -> Parser Text -> Parser Text
withTrailing numTrailing parser' =
  do
    result <- parser'
    return $ result <> replicate numTrailing "\n"

parseAfterModuleDeclarationInDocComment :: Text -> Parser Text
parseAfterModuleDeclarationInDocComment moduleDeclaration =
  do
    imports <-
      fmap (intercalate "\n" . List.sort . List.nub) $
        many $
          do
            import_ <- try parseImport
            _ <- space
            return import_
    _ <- space
    topLevelBinds <-
      many $
        choice
          [ try $ withTrailing 2 parseTypeAliasDeclaration,
            try $ withTrailing 2 parseCustomTypeDeclaration,
            try $ withTrailing 1 parseSectionComment,
            try $ withTrailing 2 parsePortDeclaration,
            try $ withTrailing 2 parseTopLevelBind,
            try $ withTrailing 2 parseIndependentDocComment,
            withTrailing 2 parseDocCommentAtEndOfModule
          ]
    _ <- eof
    let binds = stripTrailingNewlines $ mconcat topLevelBinds
    return $
      mconcat
        [ moduleDeclaration,
          if imports == ""
            then ""
            else if moduleDeclaration == "" then "" else "\n\n",
          imports,
          if binds == "" then "" else "\n\n\n",
          binds,
          "\n"
        ]

parseImportParts :: Text -> Import
parseImportParts import_ =
  case parse parseImportPartsHelp "" import_ of
    Left _ ->
      Import "" False "" [] "" False
    Right ok ->
      ok

data Import = Import
  { commentBetween :: Text,
    isMultiline :: Bool,
    as_ :: Text,
    exposing_ :: [([Text], Text)],
    name :: Text,
    isMultilineExposing :: Bool
  }

parseImportPartsHelp :: Parser Import
parseImportPartsHelp =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- chunk "import"
    _ <- space1
    commentBetween' <- commentSpaceParser 1
    name' <- parseName
    _ <- space
    as' <-
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
    (isMultilineExposing', exposing') <-
      choice
        [ do
            _ <- chunk "exposing"
            _ <- space
            parseExposingHelp 8,
          return (False, [])
        ]
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $
      Import
        { commentBetween = commentBetween',
          name = name',
          isMultiline = endRow /= startRow,
          as_ = as',
          exposing_ = exposing',
          isMultilineExposing = isMultilineExposing'
        }

importToText :: Import -> Text
importToText import_ =
  let exposingText = formatExports 8 (isMultilineExposing import_) [] (exposing_ import_)
   in mconcat
        [ "import ",
          commentBetween import_,
          if commentBetween import_ == "" then "" else " ",
          name import_,
          if as_ import_ == "" || as_ import_ == name import_
            then ""
            else " as " <> as_ import_,
          if exposingText == "()"
            then ""
            else
              if isMultiline import_
                then "\n    exposing" <> exposingText
                else " exposing" <> exposingText
        ]

deduplicateImports :: [Text] -> [Text]
deduplicateImports imports =
  map importToText $
    deduplicateImportsHelp (map parseImportParts imports) Map.empty

combineExposing :: [([Text], Text)] -> [([Text], Text)] -> [([Text], Text)]
combineExposing a b =
  a <> b

combineImports :: Import -> Import -> Import
combineImports a b =
  Import
    { commentBetween = commentBetween a,
      isMultiline = isMultiline a || isMultiline b,
      as_ = as_ a,
      exposing_ = combineExposing (exposing_ a) (exposing_ b),
      name = name a,
      isMultilineExposing = isMultilineExposing a || isMultilineExposing b
    }

deduplicateImportsHelp :: [Import] -> Map Text Import -> [Import]
deduplicateImportsHelp imports accum =
  case imports of
    [] ->
      map snd $ Map.toList accum
    top : remainder ->
      case Map.lookup (name top) accum of
        Just current ->
          deduplicateImportsHelp
            remainder
            (Map.insert (name top) (combineImports top current) accum)
        Nothing ->
          deduplicateImportsHelp
            remainder
            (Map.insert (name top) top accum)

parseAfterModuleDeclaration :: Text -> Parser Text
parseAfterModuleDeclaration moduleDeclaration =
  do
    imports <-
      fmap (intercalate "\n" . List.sort . deduplicateImports) $
        many $
          do
            import_ <- try parseImport
            _ <- space
            return import_
    _ <- space
    topLevelBinds <-
      many $
        choice
          [ try parseTypeAliasDeclaration,
            try parseCustomTypeDeclaration,
            try $ parseSectionComment,
            try parsePortDeclaration,
            try parseTopLevelBind,
            try parseIndependentDocComment,
            parseDocCommentAtEndOfModule
          ]
    _ <- eof
    let binds = intercalate "\n\n\n" topLevelBinds
    return $
      mconcat
        [ moduleDeclaration,
          if imports == ""
            then ""
            else if moduleDeclaration == "" then "" else "\n\n",
          imports,
          if binds == "" then "" else "\n\n\n",
          binds,
          "\n"
        ]

getPortDeclarationName :: Parser Text
getPortDeclarationName =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    _ <- chunk "port"
    _ <- space1
    getTypeSignatureName 1 0

parsePortDeclaration :: Parser Text
parsePortDeclaration =
  do
    _ <- space
    documentation <- choice [parseDocComment, return ""]
    _ <- space
    _ <- chunk "port"
    _ <- space1
    signature <- parseTypeSignature 1 0
    return $ (if documentation == "" then "" else documentation <> "\n") <> "port " <> signature

parseSectionComment :: Parser Text
parseSectionComment =
  do
    comment <- parseTopLevelComment
    return $ "\n" <> comment

parseTitle :: Parser Text
parseTitle =
  do
    comment <- parseTitleHelp
    return $ "\n" <> comment

parseTitleHelp :: Parser Text
parseTitleHelp =
  choice
    [ try $ do
        block1 <- try $
          do
            _ <- space
            block <- parseNonDocBlockComment
            return block
        blocks <- some $
          try $
            do
              _ <- space
              block <- parseNonDocBlockComment
              return block
        _ <- space
        return $ intercalate " " (block1 : blocks),
      try $ do
        _ <- space
        block <- parseNonDocBlockComment
        _ <- space
        line <- parseLineComment
        _ <- space
        return $ block <> "\n" <> line,
      try $ do
        _ <- space
        linesBefore <- some $
          do
            line <- parseLineComment
            _ <- space
            return line
        block <- parseDocComment
        _ <- space
        linesAfter <- some $
          do
            line <- parseLineComment
            _ <- space
            return line
        return $ block <> "\n\n" <> intercalate "\n" (linesBefore <> linesAfter),
      do
        _ <- space
        lines_ <- some $
          do
            line <- parseLineComment
            _ <- space
            return line
        _ <- space
        return $ intercalate "\n" lines_,
      try $ do
        _ <- space
        block <- parseNonDocBlockComment
        _ <- space
        return block
    ]

parseTopLevelComment :: Parser Text
parseTopLevelComment =
  do
    pieces <- some $ try parseTopLevelCommentPiece
    _ <- space
    return $ intercalate "\n" pieces

parseTopLevelCommentPiece :: Parser Text
parseTopLevelCommentPiece =
  do
    _ <- space
    choice [parseNonDocBlockComment, parseLineComment]

parseNonDocBlockComment :: Parser Text
parseNonDocBlockComment =
  do
    _ <- chunk "{-"
    _ <- lookAhead $ notFollowedBy (char '|')
    parseBlockCommentHelp 1 "{-"

parseIndependentDocComment :: Parser Text
parseIndependentDocComment =
  do
    _ <- chunk "{-|"
    comment <- parseModuleDocsHelp 1 "{-|"
    _ <- space
    lineComments <-
      some $ do
        line <- parseLineComment
        _ <- space
        return line
    return $ comment <> "\n\n\n\n" <> intercalate "\n" lineComments

parseDocCommentAtEndOfModule :: Parser Text
parseDocCommentAtEndOfModule =
  do
    _ <- chunk "{-|"
    comment <- parseModuleDocsHelp 1 "{-|"
    _ <- space
    lineComments <-
      choice
        [ some $ do
            line <- parseLineComment
            _ <- space
            return line,
          return []
        ]
    _ <- space
    _ <- lookAhead eof
    return $ comment <> (if lineComments == [] then "" else "\n\n\n\n") <> intercalate "\n" lineComments

indentDocRows :: Text -> Text
indentDocRows raw =
  case Text.lines raw of
    [] ->
      ""
    top : remainder ->
      Text.intercalate "\n" (top : map Text.strip remainder)
        <> (if Text.takeEnd 1 raw == "\n" then "\n" else "")

parseBlockCommentHelp :: Int -> Text -> Parser Text
parseBlockCommentHelp nesting contents =
  if nesting == 0
    then return contents
    else do
      choice
        [ do
            _ <- chunk "{-"
            parseBlockCommentHelp (nesting + 1) (contents <> "{-"),
          do
            _ <- chunk "-}"
            let noStart = Text.drop 2 contents
            let cleaned =
                  if Text.elem '\n' (Text.stripEnd noStart) || Text.strip noStart == ""
                    then contents
                    else (Text.strip contents) <> (if Text.strip noStart == "" then "" else " ")
            parseBlockCommentHelp (nesting - 1) (cleaned <> "-}"),
          do
            piece <- takeWhile1P Nothing (\ch -> ch /= '-' && ch /= '{')
            parseBlockCommentHelp nesting (contents <> (indentBlockRows piece)),
          do
            _ <- char '-'
            parseBlockCommentHelp nesting (contents <> "-"),
          do
            _ <- char '{'
            parseBlockCommentHelp nesting (contents <> "{")
        ]

indentBlockRows :: Text -> Text
indentBlockRows raw =
  case Text.lines raw of
    [] ->
      ""
    top : remainder ->
      Text.intercalate "\n" (top : map indentBlockRow remainder)
        <> (if Text.takeEnd 1 raw == "\n" then "\n" else "")

indentBlockRow :: Text -> Text
indentBlockRow raw =
  if raw == ""
    then ""
    else
      if Text.take 3 raw == "   "
        then raw
        else
          if Text.take 2 raw == "  "
            then " " <> raw
            else
              if Text.take 1 raw == " "
                then "  " <> raw
                else "   " <> raw

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

parseCaseOfInBrackets :: Int -> Parser Text
parseCaseOfInBrackets indent =
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

parseUnnecessaryBracketsInArgument :: Int -> Int -> Parser Text
parseUnnecessaryBracketsInArgument minColumn indent =
  do
    _ <- char '('
    _ <- space
    contents <-
      choice
        [ try $ parseRecord indent,
          parseRecordUpdate indent,
          parseVerbatim
        ]
    _ <- space
    _ <- char ')'
    return contents

parseUnnecessaryBracketsInInfix :: Int -> Int -> Parser Text
parseUnnecessaryBracketsInInfix minColumn indent =
  do
    _ <- char '('
    _ <- space
    contents <-
      choice
        [ try $ parseFunctionCall minColumn indent,
          try $ parseRecord indent,
          parseRecordUpdate indent,
          parseVerbatim
        ]
    _ <- space
    _ <- char ')'
    return contents

parseInfixedInBrackets :: Int -> Int -> Parser Text
parseInfixedInBrackets minColumn indent =
  do
    _ <- char '('
    _ <- space
    infixed <- parseInfixed minColumn indent
    _ <- space
    _ <- char ')'
    return $
      mconcat
        [ "(",
          infixed,
          if Text.elem '\n' infixed
            then "\n" <> replicate indent " "
            else "",
          ")"
        ]

parseExpression :: Int -> Context -> Int -> Parser Text
parseExpression minColumn context indent =
  choice
    [ try $ parseCaseOf indent,
      try parseGlsl,
      try $ notFollowedByInfix $ parseList indent,
      try $ parseIfThenElse minColumn indent,
      try $ parseLetIn minColumn indent,
      try $ notFollowedByInfix $ parseRecord indent,
      try $ notFollowedByInfix $ parseRecordUpdate indent,
      try $ notFollowedByInfix (parseFunctionCall minColumn indent),
      try $ notFollowedByInfix parseNumberLiteral,
      try $ parseInfixed minColumn indent,
      try parseInfixInBrackets,
      try $ notDottable $ parseParenthesised context indent,
      try $ makeDottable $ parseParenthesised NeedsBrackets indent,
      parseVerbatim,
      parseTuple context indent,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseCharLiteral,
      parseAnonymousFunction minColumn indent
    ]

parseGlsl :: Parser Text
parseGlsl =
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
          if endRow > startRow || Text.elem '\n' body
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
    let spaces = "\n" <> pack (take (floorToFour (indent + 4)) (repeat ' '))
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
    name_ <- parseName
    _ <- space
    _ <- char '='
    _ <- space
    rowBeforeComment <- fmap (unPos . sourceLine) getSourcePos
    commentBefore <- commentSpaceParser (floorToFour (indent + 4))
    right <- parseExpression 1 DoesntNeedBrackets (floorToFour (indent + 4))
    endRow <- fmap (unPos . sourceLine) getSourcePos
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser (indent - 2)
    _ <- space
    _ <- choice [char ',', lookAhead (char '}')]
    _ <- space
    return $
      mconcat
        [ name_,
          " =",
          if endRow > startRow || Text.elem '\n' right
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
    [ char '\160' >> return "\\u{00A0}",
      takeWhile1P Nothing (\ch -> ch /= '"' && ch /= '\\' && ch /= '\160'),
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
            codePoint <- fmap Text.toUpper (takeP Nothing 4)
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
    [ char '\160' >> return "\\u{00A0}",
      takeWhile1P Nothing (\ch -> ch /= '"' && ch /= '\\' && ch /= '\160'),
      chunk "\\\"",
      do
        _ <- chunk "\\u{"
        codePoint <- fmap Text.toUpper (takeP Nothing 4)
        _ <- char '}'
        return $ "\\u{" <> codePoint <> "}",
      chunk "\\n",
      chunk "\\t",
      chunk "\\\\"
    ]

parseParameter :: Int -> Int -> Parser Text
parseParameter minColumn indent =
  choice
    [ try $ parseTuplePattern NeedsBrackets indent,
      parseList indent,
      parseRecordPattern,
      try $ parseVerbatim,
      parseCharLiteral,
      parseSimpleStringLiteral
    ]

parsePattern :: Context -> Int -> Int -> Parser Text
parsePattern context minColumn indent =
  choice
    [ try $ do
        pattern <- parsePatternNoAlias context minColumn indent
        _ <-
          notFollowedBy $
            lookAhead $
              do
                _ <- space1
                _ <- chunk "as"
                _ <- space1
                return ()
        return pattern,
      try $ parseAliasedPattern context indent
    ]

parseTypeParameter :: Int -> Int -> Parser Text
parseTypeParameter minColumn indent =
  choice
    [ try $ parseTypeArgumentInUnnecessaryParens indent,
      try $ parseFunctionType minColumn indent,
      parseTupleType indent,
      parseList indent,
      try $ parseRecordType indent,
      parseExtensibleRecordType indent,
      try parseFunctionCallPattern,
      parseVerbatim,
      parseSimpleStringLiteral
    ]

parseAliasedPattern :: Context -> Int -> Parser Text
parseAliasedPattern context indent =
  choice
    [ try $ parseAliasedPatternNoBrackets context indent,
      do
        _ <- char '('
        pattern <- parseAliasedPatternNoBrackets context indent
        _ <- char ')'
        return pattern
    ]

parseAliasedPatternNoBrackets :: Context -> Int -> Parser Text
parseAliasedPatternNoBrackets context indent =
  do
    pattern <- parsePatternBeforeAs indent
    _ <- space
    _ <- chunk "as"
    _ <- space
    name_ <- parseName
    _ <- space
    return $
      mconcat
        [ case context of
            NeedsBrackets ->
              "("
            DoesntNeedBrackets ->
              "",
          pattern,
          " as ",
          name_,
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
    name_ <- parseName
    _ <- space
    _ <- choice [char ',', lookAhead (char '}')]
    return name_

parsePatternBeforeAs :: Int -> Parser Text
parsePatternBeforeAs indent =
  choice
    [ try $ parseTuplePattern NeedsBrackets indent,
      try $ do
        pattern <- parseFunctionCallPattern
        return $ "(" <> pattern <> ")",
      parseList indent,
      parseRecordPattern
    ]

parsePatternNoAliasArgument :: Int -> Int -> Parser Text
parsePatternNoAliasArgument minColumn indent =
  choice
    [ try $ parseConsPattern minColumn indent,
      try $ parseTuplePattern NeedsBrackets indent,
      parseList indent,
      parseRecordPattern,
      parseVerbatim,
      parseCharLiteral,
      parseSimpleStringLiteral
    ]

parsePatternNoAlias :: Context -> Int -> Int -> Parser Text
parsePatternNoAlias context minColumn indent =
  choice
    [ try $ parseConsPattern minColumn indent,
      try $ parseTuplePattern context indent,
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
      try $ parseAliasedPattern NeedsBrackets indent,
      parseRecordPattern,
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
      try parseNumberLiteral,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseCharLiteral
    ]

parseCallable :: Int -> Int -> Parser Text
parseCallable minColumn indent =
  choice
    [ try $ parseAnonymousFunctionInParenthesis minColumn indent,
      try $ parseCaseOfInBrackets indent,
      try parseInfixInBrackets,
      try $ parseInfixedInBrackets minColumn indent,
      try $ parseIfThenElseInBrackets minColumn indent,
      parseName
    ]

parseIfThenElseInBrackets :: Int -> Int -> Parser Text
parseIfThenElseInBrackets minColumn indent =
  do
    _ <- char '('
    _ <- space
    ifThenElse <- parseIfThenElse minColumn (indent + 1)
    _ <- space
    _ <- char ')'
    return $ "(" <> ifThenElse <> "\n" <> replicate indent " " <> ")"

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
    f <- parseCallable startColumn indent
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    firstArgument <- try $ parseArgument minColumn (indent + 4)
    subsequent <- many $ try $ parseArgument minColumn (indent + 4)
    endRow <- fmap (unPos . sourceLine) getSourcePos
    let allLinesInStringLiteral = endRow - startRow == maxMultiString && otherArgsFlat
        args = List.map (\(_, _, arg, _) -> arg) (firstArgument : subsequent)
        multiStringLiterals = filter (\arg -> Text.take 3 arg == "\"\"\"") args
        maxMultiString = safeMaximum $ List.map (\arg -> Text.count "\n" arg) multiStringLiterals
        otherArgs = filter (\arg -> Text.take 3 arg /= "\"\"\"") args
        otherArgsFlat = List.all (\arg -> not (Text.elem '\n' arg)) otherArgs
        atLeastOneMultiline = List.any (\(_, _, arg, _) -> Text.elem '\n' arg) (firstArgument : subsequent)
    return $ mconcat $ f : (addArgSpaces atLeastOneMultiline True allLinesInStringLiteral startRow endRow indent firstArgument) : map (addArgSpaces atLeastOneMultiline False allLinesInStringLiteral startRow endRow indent) subsequent

allArgLinesInStringLiteral :: Text -> Bool
allArgLinesInStringLiteral arg =
  let linesInMulti = Text.count "\n" $ getOnlyTripleStringLiteral arg
      linesInArg = Text.count "\n" arg
   in linesInMulti > 0 && linesInMulti == linesInArg

getOnlyTripleStringLiteral :: Text -> Text
getOnlyTripleStringLiteral arg =
  fst $ Text.breakOnEnd "\"\"\"" $ snd $ Text.breakOn "\"\"\"" arg

addArgSpaces :: Bool -> Bool -> Bool -> Int -> Int -> Int -> (Text, Int, Text, Int) -> Text
addArgSpaces atLeastOneMultiline isFirstArg allLinesInStringLiteral startRow endRow indent (comment, argStartRow, arg, argEndRow) =
  let indentation =
        if (startRow == endRow && not atLeastOneMultiline) || ((not (Text.elem '\n' arg)) && isFirstArg && argStartRow == startRow) || (argStartRow == startRow && allArgLinesInStringLiteral arg) || allLinesInStringLiteral
          then " "
          else (pack $ '\n' : (take (floorToFour (indent + 4)) $ repeat ' '))
   in if comment == ""
        then indentation <> arg
        else indentation <> comment <> indentation <> arg

safeMaximum :: [Int] -> Int
safeMaximum list =
  case list of
    [] ->
      0
    _ ->
      maximum list

parseArgument :: Int -> Int -> Parser (Text, Int, Text, Int)
parseArgument minColumn indent =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    column <- fmap (unPos . sourceColumn) getSourcePos
    if column < minColumn
      then fail "argument is too far left"
      else do
        comment <- commentSpaceParser (floorToFour indent)
        columnBeforeArg <- fmap (unPos . sourceColumn) getSourcePos
        if columnBeforeArg < minColumn
          then fail "argument is too far left"
          else do
            argStartRow <- fmap (unPos . sourceLine) getSourcePos
            arg <- parseArgumentExpression (floorToFour indent)
            argEndRow <- fmap (unPos . sourceLine) getSourcePos
            return (comment, argStartRow, arg, argEndRow)

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
            else parsePatternNoAliasArgument 1 1
    return $ intercalate " " (f : items)

parseInfix :: Parser Text
parseInfix =
  do
    column <- fmap (unPos . sourceColumn) getSourcePos
    infix_ <-
      if column == 0
        then fail "can't have an infix at column zero"
        else choice $ parseNegativeInfix : map chunk infixes
    _ <- lookAhead afterInfixChar
    return infix_

parseNegativeInfix :: Parser Text
parseNegativeInfix =
  do
    _ <- chunk "- "
    return "-"

afterInfixChar :: Parser Char
afterInfixChar =
  choice $ map char ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789()[] \n" :: String)

infixes :: [Text]
infixes =
  ["==", "&&", "//", ">>", "<<", "||", "<=", ">=", "<|", "|=", "++", "+", "|>", "|.", "::", "</>", ">", "<", "/=", "*", "^", "/"]

parseInfixedExpression :: Text -> Int -> Int -> Parser Text
parseInfixedExpression infix_ minColumn indent =
  choice
    [ try $ parseInfixedCaseOf infix_ indent,
      try $ do
        expression <- parseIfThenElse minColumn (if Text.length infix_ == 2 && infix_ /= "<|" then floorToFour (indent + 4) else indent + (Text.length infix_ `mod` 2))
        return $
          mconcat
            [ if infix_ /= "<|"
                then "("
                else "",
              expression,
              if infix_ /= "<|"
                then "\n" <> replicate indent " " <> ")"
                else ""
            ],
      try $ parseIfThenElse minColumn indent,
      try $ parseLetIn minColumn indent,
      try $ notDottable $ parseUnnecessaryBracketsInInfix minColumn indent,
      try $ parseFunctionCall minColumn indent,
      try $ makeDottable $ parseTuple NeedsBrackets indent,
      try $ parseTuple NeedsBrackets indent,
      let listIndent =
            if Text.length infix_ == 0 || infix_ == "<|"
              then indent
              else (floorToFour indent) + Text.length infix_ + 1
       in parseList listIndent,
      try parseEmptyRecord,
      try $ parseRecord indent,
      parseRecordUpdate indent,
      parseInfixInBrackets,
      parseCharLiteral,
      parseVerbatim,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      do
        f <- parseAnonymousFunction minColumn indent
        return $
          mconcat
            [ if infix_ == "|>"
                then "("
                else "",
              f,
              if infix_ == "|>"
                then ")"
                else ""
            ]
    ]

parseInfixed :: Int -> Int -> Parser Text
parseInfixed minColumn indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    firstExpression <- parseInfixedExpression "" minColumn indent
    midRow <- fmap (unPos . sourceLine) getSourcePos
    items <- parseInfixedItems minColumn (floorToFour indent) []
    endRow <- fmap (unPos . sourceLine) getSourcePos

    let addWhitespace :: ((Int, Bool, Text, Text, Text, Text), Bool) -> Text
        addWhitespace (infixItem, previousIsMultiline) =
          if endRow > startRow || List.any id isMultiline_
            then addMultilineInfixWhitespace indent infixItem firstIsMultilineString previousIsMultiline
            else addSingleLineInfixWhitespace infixItem

        firstIsMultilineString :: Bool
        firstIsMultilineString =
          Text.take 3 firstExpression == "\"\"\""

        isMultiline_ :: [Bool]
        isMultiline_ =
          map (\item -> Text.elem '\n' item) (firstExpression : (map (\(_, _, _, _, _, item) -> item) (items)))

    if null items
      then fail "zero infix items"
      else do
        return $
          mconcat
            [ firstExpression,
              mconcat $ map addWhitespace (List.zip items isMultiline_)
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
          commentAfter <- commentSpaceParser (indent + 4 + Text.length infix_ + 1)
          beforeExpressionRow <- fmap (unPos . sourceLine) getSourcePos
          let expressionIndent =
                if beforeExpressionRow == afterInfixRow && infix_ /= "<|"
                  then indent + 4 + Text.length infix_ + 1
                  else indent + 4
          expression <- parseInfixedExpression infix_ minColumn expressionIndent
          parseInfixedItems
            minColumn
            ( if infix_ == "<|"
                then indent + 4
                else indent
            )
            ((indent + 4, afterInfixRow == startRow, commentBefore, infix_, commentAfter, expression) : accum),
      return $ reverse accum
    ]

addSingleLineInfixWhitespace :: (Int, Bool, Text, Text, Text, Text) -> Text
addSingleLineInfixWhitespace (indent, isOnSameRowAsPrevious, commentBefore, infix_, commentAfter, expression) =
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

addMultilineInfixWhitespace :: Int -> (Int, Bool, Text, Text, Text, Text) -> Bool -> Bool -> Text
addMultilineInfixWhitespace minColumn (indent, isOnSameRowAsPrevious, commentBefore, infix_, commentAfter, expression) precededByMultilineString firstIsMultiline =
  let newIndent = floorToFour indent
   in if infix_ == "<|"
        then
          mconcat
            [ if not firstIsMultiline
                then " "
                else "\n" <> replicate (max minColumn (newIndent - 4)) " ",
              "<|\n",
              replicate newIndent " ",
              expression
            ]
        else
          if isOnSameRowAsPrevious && commentBefore == "" && commentAfter == "" && precededByMultilineString
            then " " <> infix_ <> " " <> expression
            else
              mconcat
                [ "\n" <> replicate newIndent " ",
                  commentBefore,
                  if commentBefore == ""
                    then ""
                    else "\n" <> replicate newIndent " ",
                  infix_,
                  " ",
                  commentAfter,
                  if commentAfter == ""
                    then ""
                    else
                      if Text.elem '\n' commentAfter || commentAfter == "{--}" || Text.take 2 commentAfter == "--"
                        then "\n" <> replicate (newIndent + Text.length infix_ + 1) " "
                        else " ",
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
    _ <- space
    column <- fmap (unPos . sourceColumn) getSourcePos
    let_ <- some $
      try $
        do
          items <- parseLetBind column (floorToFour (indent + 4))
          _ <- space
          return items
    commentBeforeIn <- commentSpaceParser (floorToFour (indent + 4))
    _ <- chunk "in"
    comment <- commentSpaceParser indent
    in_ <- parseExpression minColumn DoesntNeedBrackets indent
    let inSpaces = "\n" <> replicate indent " "
    return $
      mconcat
        [ "let\n",
          intercalate "\n\n" let_,
          if commentBeforeIn == ""
            then ""
            else "\n\n" <> replicate (indent + 4) " ",
          commentBeforeIn,
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
    left <- parsePattern NeedsBrackets minColumn indent
    _ <- space
    _ <- char '='
    commentBeforeRight <- commentSpaceParser (indent + 4)
    right <- parseExpression (minColumn + 1) DoesntNeedBrackets (indent + 4)
    let leftSpaces = replicate indent " "
    let rightSpaces = replicate (indent + 4) " "
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
          -- if commentAfterRight == "" then "" else "\n\n" <> leftSpaces,
          -- commentAfterRight
        ]

parseIfThenElse :: Int -> Int -> Parser Text
parseIfThenElse minColumn indent =
  do
    _ <- chunk "if"
    _ <- space1
    commentBeforeIf <- commentSpaceParser (indent + 4)
    if_ <- parseExpression minColumn DoesntNeedBrackets (floorToFour (indent + 4))
    commentAfterIf <- commentSpaceParser (indent + 4)
    _ <- chunk "then"
    commentBeforeThen <- commentSpaceParser (indent + 4)
    then_ <- parseExpression minColumn DoesntNeedBrackets (floorToFour (indent + 4))
    commentAfterThen <- commentSpaceParser (indent + 4)
    _ <- chunk "else"
    _ <- space1
    commentAfterElse <- commentSpaceParser (floorToFour (indent + 4))
    let ifThen =
          mconcat
            [ "if",
              if Text.elem '\n' if_
                then "\n" <> replicate (floorToFour (indent + 4)) " "
                else " ",
              if_,
              if Text.elem '\n' if_
                then "\n" <> replicate indent " "
                else " ",
              "then\n" <> replicate (floorToFour (indent + 4)) " ",
              commentBeforeThen,
              if commentBeforeThen == ""
                then ""
                else "\n" <> replicate (indent + 4) " ",
              then_,
              "\n\n" <> replicate indent " ",
              "else"
            ]
    choice
      [ do
          nestedIf <- parseIfThenElse minColumn indent
          return $ ifThen <> " " <> nestedIf,
        do
          else_ <- parseExpression minColumn DoesntNeedBrackets (floorToFour (indent + 4))
          return $
            mconcat
              [ "if",
                if Text.elem '\n' if_ || commentAfterIf /= "" || commentBeforeIf /= ""
                  then "\n" <> replicate (floorToFour (indent + 4)) " "
                  else " ",
                commentBeforeIf,
                if commentBeforeIf == ""
                  then ""
                  else "\n" <> replicate (floorToFour (indent + 4)) " ",
                if_,
                if commentAfterIf == ""
                  then ""
                  else "\n" <> replicate (floorToFour (indent + 4)) " ",
                commentAfterIf,
                if Text.elem '\n' if_ || commentAfterIf /= "" || commentBeforeIf /= ""
                  then "\n" <> replicate indent " "
                  else " ",
                "then\n" <> replicate (floorToFour (indent + 4)) " ",
                commentBeforeThen,
                if commentBeforeThen == "" then "" else "\n" <> replicate (floorToFour (indent + 4)) " ",
                then_,
                if commentAfterThen == ""
                  then ""
                  else "\n" <> replicate (indent + 4) " ",
                commentAfterThen,
                "\n\n" <> replicate indent " ",
                "else\n" <> replicate (floorToFour (indent + 4)) " ",
                commentAfterElse,
                if commentAfterElse == ""
                  then ""
                  else "\n" <> replicate (floorToFour (indent + 4)) " ",
                else_
              ]
      ]

parseInfixedCaseOf :: Text -> Int -> Parser Text
parseInfixedCaseOf infix_ indent =
  do
    caseOf <- parseCaseOf (if infix_ == "::" then indent + 4 else indent)
    if infix_ == "::"
      then -- I don't know why it needs the -1 in the indent. But this is what
      -- elm-format does.
        return $ "(" <> caseOf <> "\n" <> replicate ((floorToFour indent) + Text.length infix_ + 1) " " <> ")"
      else return caseOf

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
    branches <- some $ try (space >> parseCaseOfBranch column (floorToFour (indent + 4)))
    return $
      mconcat
        [ "case",
          if endRow > startRow
            then "\n" <> replicate (indent + 4) " "
            else " ",
          commentAfterCase,
          if commentAfterCase == ""
            then ""
            else
              if endRow > startRow
                then "\n" <> replicate (indent + 4) " "
                else " ",
          caseOf,
          if endRow > startRow
            then "\n" <> replicate indent " " <> "of\n"
            else " of\n",
          intercalate "\n\n" branches
        ]

parseCaseOfBranch :: Int -> Int -> Parser Text
parseCaseOfBranch minColumn indent =
  do
    commentAbove <- commentSpaceParser indent
    column <- fmap (unPos . sourceColumn) getSourcePos
    if column /= minColumn
      then fail "invalid column"
      else do
        left <- parsePattern DoesntNeedBrackets (minColumn - 4) indent
        _ <- space
        _ <- chunk "->"
        _ <- space
        comment <- commentSpaceParser (indent + 4)
        right <- parseExpression (minColumn + 1) DoesntNeedBrackets (indent + 4)
        return $
          mconcat
            [ replicate indent " ",
              commentAbove,
              if commentAbove == "" then "" else "\n" <> replicate indent " ",
              left,
              " ->\n",
              if comment == ""
                then ""
                else replicate (indent + 4) " ",
              comment,
              if comment == ""
                then ""
                else "\n",
              replicate (indent + 4) " ",
              right
            ]

keywords :: Set Text
keywords =
  Set.fromList $
    ["case", "of", "let", "in", "if", "then", "else", "->", "type", "as"]

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
      then fail $ "expecting a name but got: " <> unpack word
      else return word

parseNumberLiteral :: Parser Text
parseNumberLiteral =
  do
    negative <- choice [char '-' >> return "-", return ""]
    firstDigit <- choice $ map char ("0123456789" :: String)
    x <- choice [char 'x' >> return "x", return ""]
    remainder <- takeWhileP Nothing (\ch -> ch `elem` ("0123456789abcdefABCDEF." :: String))
    e <- choice [char 'e' >> return "e", return ""]
    exponentNegative <- choice [char '-' >> return "-", return ""]
    exponent <- takeWhileP Nothing (\ch -> ch `elem` ("0123456789" :: String))
    return $
      mconcat
        [ case Text.singleton firstDigit <> x <> remainder of
            "0" ->
              ""
            "0x00" ->
              ""
            _ ->
              negative,
          Text.singleton firstDigit,
          x,
          remainder,
          e,
          exponentNegative,
          exponent
        ]

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
        negative <- choice [char '-' >> return "-", return ""]
        word <-
          takeWhile1P
            (Just "verbatim character")
            (\ch -> ch `elem` ("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz._" :: String))
        if Set.member word keywords
          then fail $ "expecting a verbatim, but got: " <> unpack word
          else return $ negative <> word
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
            try parseNonDocBlockComment,
            do
              _ <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
              return ""
          ]
    return $
      intercalate
        ("\n" <> replicate indent " ")
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
    expression <- parsePattern DoesntNeedBrackets 1 indent
    sameLineComment <- choice [try parseSameLineComment, return ""]
    commentAfter <- commentSpaceParser indent
    _ <- choice [char ',', lookAhead (char ')')]
    return $
      mconcat
        [ if commentBefore == ""
            then ""
            else replicate indent " " <> commentBefore,
          expression,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> replicate (indent - 2) " " <> commentAfter
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
            else commentBefore <> "\n" <> replicate indent " ",
          expression,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n" <> replicate indent " " <> commentAfter
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
            else commentBefore <> "\n" <> replicate indent " ",
          expression,
          if sameLineComment == ""
            then ""
            else " " <> sameLineComment,
          if commentAfter == ""
            then ""
            else "\n\n" <> replicate (indent - 2) " " <> commentAfter
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
                      intercalate ("\n" <> replicate indent " " <> ", ") items,
                      "\n" <> replicate indent " " <> ")"
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
    [ try $ parseUnnecessaryBracketsInArgument 1 indent,
      try $ parseParenthesised context indent,
      parseMultiTuple indent
    ]

parseParenthesised :: Context -> Int -> Parser Text
parseParenthesised context indent =
  do
    _ <- char '('
    _ <- space
    commentBefore <- commentSpaceParser indent
    item <-
      parseExpression
        1
        DoesntNeedBrackets
        ( indent
            + case context of
              DoesntNeedBrackets -> 0
              NeedsBrackets -> 1
        )
    commentAfter <- commentSpaceParser indent
    _ <- char ')'
    let isMultiline_ =
          Text.elem '\n' (commentBefore <> item <> commentAfter)
    if isMultiline_
      then case context of
        NeedsBrackets ->
          return $
            mconcat
              [ "(",
                item,
                if Text.takeEnd 3 item == "\"\"\""
                  then ")"
                  else "\n" <> replicate indent " " <> ")"
              ]
        DoesntNeedBrackets ->
          return item
      else case context of
        NeedsBrackets ->
          return $
            mconcat
              [ "(",
                commentBefore,
                if commentBefore == ""
                  then ""
                  else " ",
                item,
                if commentAfter == ""
                  then ""
                  else " ",
                commentAfter,
                ")"
              ]
        DoesntNeedBrackets ->
          if commentAfter == "" && commentBefore == ""
            then return item
            else
              return $
                mconcat
                  [ "(",
                    commentBefore,
                    if commentBefore == ""
                      then ""
                      else " ",
                    item,
                    if commentAfter == ""
                      then ""
                      else " ",
                    commentAfter,
                    ")"
                  ]

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
                  intercalate ("\n" <> replicate indent " " <> ", ") items,
                  "\n" <> replicate indent " " <> ")"
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
  choice
    [ try $ parseEmptyList indent,
      parseNonEmptyList indent
    ]

parseEmptyList :: Int -> Parser Text
parseEmptyList indent =
  do
    _ <- char '['
    comment <- commentSpaceParser (indent + 1)
    _ <- char ']'
    return $
      if comment == ""
        then "[]"
        else "[" <> comment <> "\n" <> replicate indent " " <> "]"

getLinesInMultiString :: Text -> Int
getLinesInMultiString item =
  if Text.take 3 item == "\"\"\""
    then Text.count "\n" item
    else 0

parseNonEmptyList :: Int -> Parser Text
parseNonEmptyList indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '['
    comment <- commentSpaceParser (indent + 2)
    items <- many (parseMultiListItem (indent + 2) ']')
    let linesInMultiString :: Int
        linesInMultiString = List.maximum $ map getLinesInMultiString items
    _ <- char ']'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    let isMultiline_ :: Bool
        isMultiline_ = ((endRow - startRow) - linesInMultiString) > 0
    let indentation =
          if isMultiline_
            then "\n" <> replicate indent " "
            else ""
    let endSpace =
          if isMultiline_
            then ""
            else " "
    if null items
      then
        return $
          if comment == ""
            then "[]"
            else "[" <> comment <> "\n" <> replicate indent " " <> "]"
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
