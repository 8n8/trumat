module Trumat (trumat) where

import Data.Function ((&))
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
import qualified Prelude

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
    _ <- choice [chunk "- ", chunk "-\n", chunk "+ ", chunk "+\n", chunk "* "]
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
                  _ <- notFollowedBy $ try $ do
                    _ <- space
                    _ <- chunk "-}"
                    return ()
                  _ <- notFollowedBy $ try $ do
                    _ <- char '\n'
                    _ <- takeWhileP Nothing (\ch -> ch == ' ')
                    _ <- choice [chunk "- ", chunk "+ ", chunk "* "]
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
        otherLines <-
          fmap mconcat $
            many $
              try $
                do
                  _ <- notFollowedBy $ do
                    _ <- space
                    _ <- choice [chunk "- ", chunk "-\n", chunk "-}", chunk "+ ", chunk "+\n", chunk "* "]
                    return ()
                  newlines <-
                    choice
                      [ do
                          _ <- chunk "\n\n"
                          spaces_ <- takeWhileP Nothing (\ch -> ch == ' ')
                          if Text.length spaces_ < numSpaces
                            then fail "incorrect indentation"
                            else return "\n\n",
                        do
                          _ <- chunk "\n"
                          _ <- takeWhileP Nothing (\ch -> ch == ' ')
                          return "\n"
                      ]
                  line <- takeWhile1P Nothing (\ch -> ch /= '\n')
                  return $ newlines <> replicate (numSpaces + 2) " " <> line
        choice
          [ try $ do
              _ <- takeWhile1P Nothing (\ch -> ch == '\n')
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
                               <> (if accumulated /= "" && isGappy && newIndent == indent then "\n" else "")
                               <> formatted
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
                           <> otherLines
                   )
          ]

parseTypedBacktickedCodeBlock :: Parser Text
parseTypedBacktickedCodeBlock =
  do
    _ <- chunk "```"
    type_ <- takeWhile1P Nothing (\ch -> ch /= '\n' && ch /= ' ')
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
    return $ "```" <> type_ <> code <> "```"

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

parseLinkAlias :: Parser Text
parseLinkAlias =
  do
    _ <- char '['
    alias <- takeWhile1P Nothing (\ch -> ch /= ']')
    _ <- chunk "]:"
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    contents <- takeWhileP Nothing (\ch -> ch /= '\n')
    return $ "[" <> alias <> "]: " <> contents

parseNamedLinkAlias :: Parser Text
parseNamedLinkAlias =
  do
    leadingText <- takeWhileP Nothing (\ch -> ch /= '\n' && ch /= '-' && ch /= '_' && ch /= '\\' && ch /= '[')
    _ <- char '['
    firstPart <- takeWhileP Nothing (\ch -> ch /= ']')
    _ <- char ']'
    _ <- char '['
    secondPart <- takeWhileP Nothing (\ch -> ch /= ']')
    _ <- char ']'
    return $
      mconcat
        [ leadingText,
          "[",
          firstPart,
          "]",
          if secondPart == ""
            then ""
            else
              mconcat
                [ "[",
                  secondPart,
                  "]"
                ]
        ]

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
        remainder <- takeWhileP Nothing (\ch -> ch /= '\n')
        return $ (if remainder == "" || remainder == ">" then "\n\n" else "\n") <> "-" <> hyphens <> remainder,
      try parseLinkAlias,
      try parseTypedBacktickedCodeBlock,
      parseBacktickedCodeBlock,
      parseBlockQuote,
      try parseMultipleDocHeaders,
      try parseDocHeader,
      try parseSecondLevelUnderlineDocHeader,
      try parseTopLevelUnderlineDocHeader,
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
      try $ do
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        _ <- char '-'
        _ <- takeWhileP Nothing (\ch -> ch == ' ')
        _ <- char '\n'
        return "\n",
      try parseMultilineAsteriskBold,
      try parseMultilineUnderscored,
      try parseNamedLinkAlias,
      do
        pieces <-
          some $
            do
              text <- try $ fmap (angleBracketUrls . escapeBackticks . escapeUnderscores . escapeAsterisks . escapeBackslashes) $ parseOrdinaryTextInDoc
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

parseTopLevelUnderlineDocHeader :: Parser Text
parseTopLevelUnderlineDocHeader =
  do
    header <- takeWhile1P Nothing (\ch -> ch /= '\n')
    _ <- char '\n'
    _ <- char '='
    _ <- takeWhile1P Nothing (\ch -> ch == '=')
    _ <- takeWhile1P Nothing (\ch -> ch == '\n')
    return $ "\n\n\n# " <> Text.strip header <> "\n\n"

parseSecondLevelUnderlineDocHeader :: Parser Text
parseSecondLevelUnderlineDocHeader =
  do
    header <- takeWhile1P Nothing (\ch -> ch /= '\n')
    _ <- char '\n'
    _ <- char '-'
    _ <- takeWhile1P Nothing (\ch -> ch == '-')
    _ <- takeWhile1P Nothing (\ch -> ch == '\n')
    return $ "\n\n\n## " <> Text.strip header <> "\n\n"

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
                      _ <-
                        choice
                          [ do
                              _ <- chunk "\n\n"
                              _ <- takeWhileP Nothing (\ch -> ch == ' ')
                              startColumn <- fmap (unPos . sourceColumn) getSourcePos
                              if startColumn == 1
                                then fail "after end of list"
                                else return (),
                            do
                              _ <- chunk "\n"
                              _ <- takeWhileP Nothing (\ch -> ch == ' ')
                              return ()
                          ]
                      takeWhile1P Nothing (\ch -> ch /= '\n')
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
          [ try backtickQuote,
            takeWhile1P Nothing (\ch -> ch /= '\\' && ch /= '`'),
            chunk "\\\\",
            chunk "\\*",
            chunk "\\_",
            chunk "\\`",
            do
              backslashes <- takeWhile1P Nothing (\ch -> ch == '\\')
              return $ Text.replace "\\" "\\\\" backslashes,
            takeWhile1P Nothing (\ch -> ch == '\\'),
            takeWhile1P Nothing (\ch -> ch == '`')
          ]
    return $ mconcat pieces

angleBracketUrls :: Text -> Text
angleBracketUrls text =
  let leadingSpaces = getLeadingSpaces text
      trailingSpaces = getTrailingSpaces text
      words = Text.words text
   in leadingSpaces <> Text.unwords (map angleBracketUrl words) <> trailingSpaces

urlSchemes :: Set Text
urlSchemes =
  Set.fromList ["aaa", "http", "https", "acap", "adiumxtra", "afp", "afs", "aim", "apt", "attachment", "aw", "beshare", "bitcoin", "bolo", "callto", "cap", "chrome", "cid", "coap", "content", "crid", "cvs", "data", "dav", "dict", "dns", "doi", "dtn", "dvb", "ed2k", "facetime", "file", "finger", "fish", "ftp", "geo", "gg", "git", "gizmoproject", "go", "gopher", "gtalk", "h323", "hcp", "iax", "icap", "icon", "imap", "info", "ipn", "ipp", "irc", "irc6", "ircs", "iris", "itms", "jar", "jms", "keyparc", "lastfm", "ldap", "ldaps", "magnet", "mailto", "maps", "market", "message", "mid", "mms", "msnim", "msrp", "msrps", "mtqp", "mumble", "mupdate", "mvn", "news", "nfs", "ni", "nih", "nntp", "notes", "opaquelocktoken", "palm", "paparazzi", "platform", "pop", "pres", "proxy", "psyc", "query", "res", "resource", "rmi", "rsync", "rtmp", "rtsp", "secondlife", "service", "session", "sftp", "sgn", "shttp", "sieve", "sip", "sips", "skype", "smb", "sms", "snmp", "soldat", "spotify", "ssh", "steam", "svn", "tag", "teamspeak", "tel", "telnet", "tftp", "things", "thismessage", "tn3270", "tip", "tv", "udp", "unreal", "urn", "ut2004", "vemmi", "ventrilo", "webcal", "ws", "wss", "wtai", "wyciwyg", "xcon", "xfire", "xmpp", "xri", "ymsgr"]

angleBracketUrl :: Text -> Text
angleBracketUrl text =
  let (scheme, remainder) = Text.break (\ch -> ch == ':') text
   in if Text.elem ':' text && Set.member scheme urlSchemes && Text.strip remainder /= ":"
        then "<" <> text <> ">"
        else text

getLeadingSpaces :: Text -> Text
getLeadingSpaces text =
  case Text.uncons text of
    Nothing ->
      ""
    Just (' ', remainder) ->
      " " <> getLeadingSpaces remainder
    Just _ ->
      ""

getTrailingSpaces :: Text -> Text
getTrailingSpaces text =
  getLeadingSpaces (Text.reverse text)

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
    firstPiece <- takeWhile1P Nothing (\ch -> ch /= '\n' && ch /= '`')
    otherPieces <- many $
      try $
        do
          spaces <- takeWhileP Nothing (\ch -> ch == ' ')
          piece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '`')
          return $ spaces <> piece
    trailing <- takeWhile1P Nothing (\ch -> ch == '`')
    if Text.length leading /= Text.length trailing
      then fail "there must be equal numbers of trailing and leading underscores"
      else return $ leadingSpaces <> "`" <> Text.strip (firstPiece <> mconcat otherPieces) <> "`"

escapeUnderscores :: Text -> Text
escapeUnderscores text =
  case parse parseEscapeUnderscores "" text of
    Left _ ->
      text
    Right ok ->
      ok

namedUrl :: Parser Text
namedUrl =
  do
    leadingSpaces <- takeWhileP Nothing (\ch -> ch == ' ')
    _ <- char '['
    alias <- takeWhile1P Nothing (\ch -> ch /= ']')
    _ <- char ']'
    _ <- char '('
    url <- takeWhile1P Nothing (\ch -> ch /= ')')
    _ <- char ')'
    return $ leadingSpaces <> "[" <> alias <> "](" <> url <> ")"

parseEscapeUnderscores :: Parser Text
parseEscapeUnderscores =
  do
    pieces <-
      many $
        choice
          [ try backtickQuote,
            try underscoreBolds,
            try namedUrl,
            takeWhile1P Nothing (\ch -> ch /= '\\' && ch /= '_' && ch /= '`' && ch /= '['),
            chunk "\\_",
            chunk "\\`",
            do
              underscores <- takeWhile1P Nothing (\ch -> ch == '_')
              return $ Text.replace "_" "\\_" underscores,
            takeWhile1P Nothing (\ch -> ch == '_'),
            takeWhile1P Nothing (\ch -> ch == '`'),
            takeWhile1P Nothing (\ch -> ch == '\\'),
            takeWhile1P Nothing (\ch -> ch == '[')
          ]
    return $ mconcat pieces

escapeAsterisks :: Text -> Text
escapeAsterisks text =
  case parse parseEscapeAsterisks "" text of
    Left _ ->
      text
    Right ok ->
      ok

parseMultilineUnderscored :: Parser Text
parseMultilineUnderscored =
  do
    leadingText <- takeWhileP Nothing (\ch -> ch /= '\n' && ch /= '-' && ch /= '_' && ch /= '\\')
    leadingSpaces <- takeWhileP Nothing (\ch -> ch == ' ')
    leading <- takeWhile1P Nothing (\ch -> ch == '_')
    firstPiece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '_' && ch /= '-')
    otherPieces <- many $
      try $
        do
          spaces <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
          piece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '_' && ch /= '-')
          return $ spaces <> piece
    trailingSpaces <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    if not $ Text.elem '\n' ((mconcat otherPieces) <> trailingSpaces)
      then fail "expecting a newline in the text"
      else do
        trailing <- takeWhile1P Nothing (\ch -> ch == '_')
        if Text.length leading /= Text.length trailing
          then fail "there must be equal numbers of trailing and leading asterisks"
          else return $ leadingText <> leadingSpaces <> leading <> firstPiece <> mconcat otherPieces <> trailingSpaces <> trailing

parseMultilineAsteriskBold :: Parser Text
parseMultilineAsteriskBold =
  do
    leadingText <-
      fmap mconcat $
        some $
          choice
            [ try backtickQuote,
              try underscoreAsteriskBolds,
              takeWhile1P Nothing (\ch -> ch /= '*' && ch /= '-' && ch /= '`')
            ]
    leadingSpaces <- takeWhileP Nothing (\ch -> ch == ' ')
    leading <- takeWhile1P Nothing (\ch -> ch == '*')
    firstPiece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '*' && ch /= '-')
    otherPieces <- many $
      try $
        do
          spaces <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
          piece <- takeWhile1P Nothing (\ch -> ch /= ' ' && ch /= '\n' && ch /= '*')
          return $ spaces <> piece
    trailingSpaces <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    if not $ Text.elem '\n' ((mconcat otherPieces) <> trailingSpaces)
      then fail "expecting a newline in the text"
      else do
        trailing <- takeWhile1P Nothing (\ch -> ch == '*')
        let leadingUnderscores = Text.replace "*" "_" leading
        let trailingUnderscores = Text.replace "*" "_" trailing
        if Text.length leading /= Text.length trailing
          then fail "there must be equal numbers of trailing and leading asterisks"
          else
            if Text.length leading == 1
              then return $ leadingText <> leadingSpaces <> leadingUnderscores <> firstPiece <> mconcat otherPieces <> trailingSpaces <> trailingUnderscores
              else return $ leadingText <> leadingSpaces <> leading <> firstPiece <> mconcat otherPieces <> trailingSpaces <> trailing

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
    leadingSpaces <- takeWhileP Nothing (\ch -> ch == ' ')
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
      else return $ leadingSpaces <> leading <> firstPiece <> mconcat otherPieces <> trailing

underscoreAsteriskBolds :: Parser Text
underscoreAsteriskBolds =
  do
    leadingSpaces <- takeWhileP Nothing (\ch -> ch == ' ')
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
          then return $ leadingSpaces <> leadingUnderscores <> firstPiece <> mconcat otherPieces <> trailingUnderscores
          else return $ leadingSpaces <> leading <> firstPiece <> mconcat otherPieces <> trailing

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
            return "-",
          try $ do
            _ <- char '@'
            _ <- notFollowedBy $ chunk "docs "
            return "@"
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
            parseExportDocsRowOnly nesting accumulator,
          do
            _ <- char '@'
            _ <- notFollowedBy $ try $ chunk "docs "
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

parseExportRow :: Parser [(Text, Text, Text)]
parseExportRow =
  do
    items <- some parseSingleItemInExportRow
    _ <- space
    return items

parseSingleItemInExportRow :: Parser (Text, Text, Text)
parseSingleItemInExportRow =
  do
    _ <- choice [char ',', return ' ']
    commentBefore <- commentSpaceParser 6
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
    commentIndent <-
      lookAhead $
        choice
          [ try $ do
              _ <- commentSpaceParser 0
              _ <- char ')'
              return 4,
            return 6
          ]
    commentAfter <- commentSpaceParser commentIndent
    _ <- choice [char ',', return ' ']
    _ <- takeWhileP Nothing (\ch -> ch == ' ')
    return (commentBefore, name' <> all_, commentAfter)

parseExposingHelp :: Int -> Parser (Bool, [[(Text, Text, Text)]])
parseExposingHelp indent =
  do
    startRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- char '('
    _ <- space
    items <- some parseExportRow
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
    items <- some parseExportRow
    _ <- space
    _ <- char ')'
    endRow <- fmap (unPos . sourceLine) getSourcePos
    return $ formatExports indent (endRow > startRow) docs items

formatDocumentedExportRow :: [Text] -> [Text] -> [[Text]] -> [Text] -> [Text]
formatDocumentedExportRow undocumented documentedComments docs items =
  if null documentedComments || not (null undocumented) || length docs > 1
    then [Text.intercalate ", " items]
    else items

snd3 :: (a, b, c) -> b
snd3 (_, b, _) =
  b

exportItemIsMultiline :: (Text, Text, Text) -> Bool
exportItemIsMultiline (comment1, item, comment2) =
  Text.take 2 comment1 == "--"
    || Text.take 2 comment2 == "--"
    || Text.elem '\n' comment1
    || Text.elem '\n' comment2

formatUndocumented :: Int -> [(Text, Text, Text)] -> Text
formatUndocumented indent row =
  Text.intercalate ", " (map (formatUndocumentedItem indent) row)

formatUndocumentedItem :: Int -> (Text, Text, Text) -> Text
formatUndocumentedItem indent (comment1, item, comment2) =
  let gap comment =
        if comment == ""
          then ""
          else
            if Text.take 2 comment == "--" || Text.elem '\n' comment || comment == "{--}"
              then "\n" <> Text.replicate (indent + 2) " "
              else " "
   in mconcat
        [ comment1,
          gap comment1,
          item,
          gap comment2,
          comment2
        ]

formatCommentsOnDocumented :: [Text] -> Text
formatCommentsOnDocumented comments =
  Text.intercalate "\n    " comments

isSingleLineExports :: [[Text]] -> [Text] -> [Text] -> [Text] -> Bool
isSingleLineExports docs commentsOnDocumented documented undocumented =
  case (documented, undocumented) of
    ([], []) ->
      True
    ((_ : _), []) ->
      (not $ any isMultilineComment commentsOnDocumented)
        && (length docs < 2)
    _ ->
      False

formatExports :: Int -> Bool -> [[Text]] -> [[(Text, Text, Text)]] -> Text
formatExports indent originalIsMultiline docs items =
  let unformattedDocumentedRows :: [[Text]]
      unformattedDocumentedRows =
        removeUndocumented (map snd3 (mconcat items)) docs

      formattedDocumentedRows :: [Text]
      formattedDocumentedRows =
        mconcat $
          map
            ( formatDocumentedExportRow
                formattedUndocumentedRows
                commentsOnDocumented
                docs
            )
            unformattedDocumentedRows

      commentsOnDocumented :: [Text]
      commentsOnDocumented =
        getDocumentedComments docs items

      formattedCommentsOnDocumented :: Text
      formattedCommentsOnDocumented =
        formatCommentsOnDocumented commentsOnDocumented

      formattedUndocumentedRows :: [Text]
      formattedUndocumentedRows =
        map (formatUndocumented indent) $ getUndocumented indent docs items

      formattedRows :: [Text]
      formattedRows =
        filter
          (\row -> row /= "")
          $ formattedDocumentedRows <> formattedUndocumentedRows

      isMultiline' :: Bool
      isMultiline' =
        ( originalIsMultiline
            || ( case removeUndocumented (map snd3 (mconcat items)) docs of
                   [] ->
                     False
                   [_] ->
                     False
                   _ ->
                     True
               )
            || ( not (null formattedDocumentedRows)
                   && not (null formattedUndocumentedRows)
               )
        )
          && not
            ( isSingleLineExports
                docs
                commentsOnDocumented
                formattedDocumentedRows
                formattedUndocumentedRows
            )
   in case formattedRows of
        [] ->
          "()"
        [single] ->
          if Text.elem '\n' single
            || any isMultilineComment commentsOnDocumented
            then
              [ "\n",
                Text.replicate indent " ",
                "( ",
                single,
                if null commentsOnDocumented
                  then ""
                  else "\n" <> Text.replicate indent " ",
                Text.intercalate
                  ("\n" <> Text.replicate indent " ")
                  commentsOnDocumented,
                "\n",
                Text.replicate indent " ",
                ")"
              ]
                & mconcat
            else
              [ " (",
                single,
                if null commentsOnDocumented
                  then ""
                  else " ",
                Text.intercalate " " commentsOnDocumented,
                ")"
              ]
                & mconcat
        multiple ->
          [ if isMultiline'
              then "\n" <> pack (take indent (repeat ' ')) <> "( "
              else " (",
            intercalate
              ( if isMultiline'
                  then "\n" <> pack (take indent (repeat ' ')) <> ", "
                  else ", "
              )
              multiple,
            if formattedCommentsOnDocumented == ""
              then ""
              else
                if isMultilineComment formattedCommentsOnDocumented
                  then
                    "\n    "
                      <> ( if null formattedUndocumentedRows
                             then ""
                             else "  "
                         )
                  else " ",
            formattedCommentsOnDocumented,
            if isMultiline'
              then "\n" <> pack (take indent (repeat ' '))
              else "",
            ")"
          ]
            & mconcat

isMultilineComment :: Text -> Bool
isMultilineComment text =
  text == "{--}" || Text.take 2 text == "--" || Text.elem '\n' text

removeUndocumented :: [Text] -> [[Text]] -> [[Text]]
removeUndocumented used docs =
  let docsWithExposeAll :: [[Text]]
      docsWithExposeAll =
        addExposeAllToDocs used docs

      usedDocs :: [[Text]]
      usedDocs =
        removeUnusedDocs used docsWithExposeAll
   in removeEmptyLists usedDocs

getDocumentedComments :: [[Text]] -> [[(Text, Text, Text)]] -> [Text]
getDocumentedComments docs rows =
  let docset = Set.fromList $ mconcat docs
   in mconcat
        $ map
          ( \(comment1, _, comment2) ->
              mconcat $
                [ if comment1 == "" then [] else [comment1],
                  if comment2 == "" then [] else [comment2]
                ]
          )
        $ filter (\(_, name, _) -> Set.member name docset)
        $ mconcat rows

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

getUndocumented :: Int -> [[Text]] -> [[(Text, Text, Text)]] -> [[(Text, Text, Text)]]
getUndocumented indent docs items =
  let docSet = Set.fromList $ mconcat docs
   in removeDocumented docSet items
        & flattenIfNoDocs docs
        & List.sortOn
          ( \row ->
              case row of
                [] ->
                  ""
                (_, topName, _) : _ ->
                  topName
          )

flattenIfNoDocs :: [[Text]] -> [[(Text, Text, Text)]] -> [[(Text, Text, Text)]]
flattenIfNoDocs docs rows =
  case docs of
    [] ->
      map (\item -> [item]) (mconcat rows)
    _ ->
      rows

exportCommentSpace :: Int -> Text -> Text
exportCommentSpace indent comment =
  if comment == ""
    then ""
    else
      if (not (Text.elem '\n' comment))
        && Text.take 2 comment == "{-"
        && comment /= "{--}"
        then " "
        else "\n" <> replicate (indent + 2) " "

removeEmptyExportRows :: [[(Text, Text, Text)]] -> [[(Text, Text, Text)]]
removeEmptyExportRows rows =
  filter (\row -> not (null row)) rows

removeDocumented :: Set Text -> [[(Text, Text, Text)]] -> [[(Text, Text, Text)]]
removeDocumented docSet items =
  filter
    (\row -> not (null row))
    ( map
        (filter (\(_, item, _) -> not (Set.member (trimExposeAll item) docSet)))
        items
    )

-- flattenExportRows :: [[(Text, Text, Text)]] -> [[(Text, Text, Text)]]
-- flattenExportRows rows =
--   flattenExportRowsHelp rows []
--
-- flattenExportRowsHelp :: [[(Text, Text, Text)]] -> [[(Text, Text, Text)]] -> [[(Text, Text, Text)]]
-- flattenExportRowsHelp rows accumulator =
--   case rows of
--     [] ->
--       reverse accumulator
--     top : remainder ->
--       case reverse row of
--         [] ->
--           flattenExportRowsHelp remainder accumulator
--         rowTop : rowRemainder ->
--           flattenExportRowsHelp
--             remainder
--             ((reverse ((comment1, [rowTop], comment2) : map (\item -> ("", [item], "")) rowRemainder)) <> accumulator)
--
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
      if isDigit first
        then isNumberedListItemAfterFirst remainder
        else False

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

maxTwoNewlinesAfterTypedCodeBlock :: [Text] -> [Text]
maxTwoNewlinesAfterTypedCodeBlock rows =
  maxTwoNewlinesTypedHelp rows False []

maxTwoNewlinesTypedHelp :: [Text] -> Bool -> [Text] -> [Text]
maxTwoNewlinesTypedHelp rows afterBlock accumulator =
  case rows of
    [] ->
      reverse accumulator
    top : remainder ->
      if Text.take 3 top == "```"
        then maxTwoNewlinesTypedHelp remainder True (top : accumulator)
        else
          if afterBlock && top == "\n"
            then maxTwoNewlinesTypedHelp remainder True accumulator
            else maxTwoNewlinesTypedHelp remainder False (top : accumulator)

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

formatElmExpressionInDocs :: Text -> Maybe Text
formatElmExpressionInDocs unformatted =
  case parse (try parseExpressionInDocsWithSpace) "" unformatted of
    Left _ ->
      Nothing
    Right formatted ->
      Just formatted

parseExpressionInDocsWithSpace :: Parser Text
parseExpressionInDocsWithSpace =
  do
    _ <- space
    expression <- parseExpressionInDocs 0 DoesntNeedBrackets 0
    _ <- space
    eof
    return expression

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
    _ <- takeWhileP Nothing (\ch -> ch == '\n')
    moduleDeclaration <- parseModuleDeclarationNoCreate
    _ <- takeWhileP Nothing (\ch -> ch == '\n')
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
              case formatElmExpressionInDocs codeChunk of
                Nothing ->
                  codeChunk
                Just ok ->
                  (Text.strip ok) <> "\n"
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

addNewlineToTrailingLinkAliases :: [Text] -> [Text]
addNewlineToTrailingLinkAliases rows =
  case reverse rows of
    "\n" : possiblyLinkAlias : previous ->
      if isLinkAlias possiblyLinkAlias
        then reverse ((Text.stripEnd possiblyLinkAlias) <> "\n\n" : previous)
        else rows
    _ ->
      rows

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

stripTooManyNewlinesBetweenCodeAndHeader :: [Text] -> [Text]
stripTooManyNewlinesBetweenCodeAndHeader rows =
  stripTooManyNewlinesBetweenCodeAndHeaderHelp rows []

stripTooManyNewlinesBetweenCodeAndHeaderHelp :: [Text] -> [Text] -> [Text]
stripTooManyNewlinesBetweenCodeAndHeaderHelp rows accum =
  case rows of
    maybeCode : maybeHeader : remainder ->
      if Text.take 1 (stripLeadingNewlines maybeHeader) == "#" && Text.take 6 maybeCode == "\n\n    "
        then
          stripTooManyNewlinesBetweenCodeAndHeaderHelp
            remainder
            (maybeHeader : Text.stripEnd maybeCode : accum)
        else
          stripTooManyNewlinesBetweenCodeAndHeaderHelp
            (maybeHeader : remainder)
            (maybeCode : accum)
    [] ->
      reverse accum
    [top] ->
      reverse (top : accum)

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
    || any isLinkAlias rows
    then rows
    else removeTooManyTrailingEmptyLinesHelp (reverse rows)

isLinkAlias :: Text -> Bool
isLinkAlias row =
  case parse parseLinkAlias "" row of
    Left _ ->
      False
    Right _ ->
      True

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

addTwoNewlinesAfterTrailingTypedCodeBlock :: [Text] -> [Text]
addTwoNewlinesAfterTrailingTypedCodeBlock rows =
  case reverse rows of
    last : remainder ->
      if Text.take 3 last == "```"
        then rows <> ["\n", "\n"]
        else rows
    [] ->
      []

removeNewlinesBeforeLeadingHyphens :: [Text] -> [Text]
removeNewlinesBeforeLeadingHyphens rows =
  case rows of
    top : remainder ->
      if Text.take 4 (stripLeadingNewlines top) /= "    "
        && Text.take 2 (Text.stripStart top) == "--"
        && Text.take 3 (Text.stripStart top) /= "-->"
        then (" " <> Text.stripStart top) : remainder
        else rows
    [] ->
      []

noMoreThanTwoNewlinesBeforeLeadingAtDocs :: [Text] -> [Text]
noMoreThanTwoNewlinesBeforeLeadingAtDocs rows =
  case removeLeadingNewlinesFromList rows of
    top : remainder ->
      if Text.take 6 top == "@docs "
        then "\n" : "\n" : top : remainder
        else rows
    [] ->
      []

removeLeadingNewlinesFromList :: [Text] -> [Text]
removeLeadingNewlinesFromList rows =
  case rows of
    "\n" : remainder ->
      removeLeadingNewlinesFromList remainder
    _ ->
      rows

addExtraSpaceBetweenSameLineItems :: [Text] -> [Text]
addExtraSpaceBetweenSameLineItems rows =
  addExtraSpaceBetweenSameLineItemsHelp rows []

addExtraSpaceBetweenSameLineItemsHelp :: [Text] -> [Text] -> [Text]
addExtraSpaceBetweenSameLineItemsHelp rows accumulated =
  case rows of
    first : second : remainder ->
      if Text.takeEnd 1 first /= " " && Text.takeEnd 1 first /= "\n" && Text.take 1 second /= "\n" && Text.take 1 second /= " " && Text.take 1 second /= "."
        then
          addExtraSpaceBetweenSameLineItemsHelp
            remainder
            ((" " <> second) : first : accumulated)
        else
          addExtraSpaceBetweenSameLineItemsHelp
            (second : remainder)
            (first : accumulated)
    _ ->
      reverse ((reverse rows) <> accumulated)

parseModuleDocsInner :: Parser Text
parseModuleDocsInner =
  do
    rows <-
      fmap addExtraSpaceBetweenSameLineItems $
        fmap noMoreThanTwoNewlinesBeforeLeadingAtDocs $
          fmap removeNewlinesBeforeLeadingHyphens $
            fmap addTwoNewlinesAfterTrailingTypedCodeBlock $
              fmap maxTwoNewlinesAfterTypedCodeBlock $
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
                                    fmap stripTooManyNewlinesBetweenCodeAndHeader $
                                      fmap removeTooManyNewlinesAfterAtDocsAfterHeader $
                                        fmap stripTooManyNewlinesBeforeCodeBlocks $
                                          fmap trimTrailingNewlines $
                                            fmap stripLeadingSpacesFromDocRow $
                                              fmap (map newlinesAfterBackticks) $
                                                fmap backticksAroundCodeAfterList $
                                                  fmap addNewlineToTrailingLinkAliases $
                                                    fmap addNewlineToTrailingCode $
                                                      fmap removeTooManyNewlinesAfterAtDocs $
                                                        fmap formatElmInDocs $
                                                          fmap maxTwoNewlinesAfterCodeBlock $
                                                            fmap removeTripleNewlinesInParagraphs $
                                                              some $
                                                                try parseDocRow

    let stripped = stripNewlinesStart $ mconcat rows
        first =
          if Text.take 3 stripped == "```"
            then Text.take 3 stripped
            else Text.take 1 stripped
        flat = if first == "```" || first == "#" || first == ">" || first == "-" || first == "@" || Text.take 4 stripped == "    " || isListItem stripped || any isLinkAlias rows then mconcat rows else " " <> (Text.stripStart $ mconcat rows)
        firstIsList = case filter (\item -> Text.strip item /= "") rows of
          [] -> False
          top : _ -> isListItem top
        lastIsNotSeparateAlias =
          case reverse (Text.lines (Text.strip flat)) of
            last : second : remainder ->
              case parse parseLinkAlias "" last of
                Left _ ->
                  False
                Right _ ->
                  second /= ""
            _ ->
              False
        lastIsAlias =
          case reverse (Text.lines (Text.strip flat)) of
            last : _ ->
              case parse parseLinkAlias "" last of
                Left _ ->
                  False
                Right _ ->
                  True
            [] ->
              False
    return $
      if Text.strip (mconcat rows) == ""
        then "\n\n"
        else
          if Text.isInfixOf "@docs" flat
            then flat
            else
              if Text.take 2 flat == "\n\n" || Text.elem '\n' (Text.strip flat)
                then
                  ( if Text.count "\n\n" (Text.strip flat) == 0 && not firstIsList && not lastIsAlias
                      then flat
                      else
                        if lastIsCommentOnlyCodeBlock flat || lastIsNotSeparateAlias
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
                      then "{-| -}"
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
    -- asdfasdf
    commentBefore <- choice [parseTopLevelComment, return ""]
    declaration <-
      choice
        [ parseModuleDeclaration,
          return ""
        ]
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
    _ <- lookAhead $ try $ choice [chunk "{-", chunk "--", chunk " ", chunk "\n"]
    startRow <- fmap (unPos . sourceLine) getSourcePos
    commentBeforeName <- commentSpaceParser 4
    name' <- parseName
    afterNameRow <- fmap (unPos . sourceLine) getSourcePos
    _ <- space
    parameters <- parseParameters 0
    _ <- space
    commentBeforeEquals <- commentSpaceParser 4
    endRow <- fmap (unPos . sourceLine) getSourcePos
    firstBranch <- parseBranch '='
    nextBranches <- many (parseBranch '|')
    let branches = firstBranch : nextBranches
    return $
      mconcat
        [ documentation,
          if documentation == ""
            then ""
            else "\n",
          "type",
          (if commentBeforeName == "" then "" else "\n    "),
          commentBeforeName,
          if commentBeforeName /= ""
            || afterNameRow > startRow
            || commentBeforeEquals /= ""
            then "\n    "
            else " ",
          name',
          if parameters == ""
            then ""
            else " ",
          parameters,
          (if commentBeforeEquals == "" then "" else "\n    "),
          commentBeforeEquals,
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

isOnlyLineComment :: Text -> Bool
isOnlyLineComment comment =
  Text.take 2 (Text.stripStart comment) == "--" && not (Text.elem '\n' comment)

parseBranchNoParameters :: Text -> Text -> Int -> Parser Text
parseBranchNoParameters commentBefore branchName afterNameRow =
  do
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    afterEmptySpaceRow <- fmap (unPos . sourceLine) getSourcePos
    afterEmptySpaceColumn <- fmap (unPos . sourceColumn) getSourcePos
    commentAfter <- parseCommentAfterTypeBranch afterEmptySpaceColumn
    return $
      mconcat
        [ commentBefore,
          if commentBefore == ""
            then ""
            else
              if not (Text.elem '\n' commentBefore)
                && Text.take 2 commentBefore == "{-"
                && {- I know this is really strange, but it is how
                      elm-format behaves. It can probably be considered a
                      bug in elm-format, but I left it in for
                      compatibility.
                      -} commentBefore /= "{--}"
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

parseCommentAfterTypeBranch :: Int -> Parser Text
parseCommentAfterTypeBranch afterEmptySpaceColumn =
  choice
    [ try $ do
        columnBefore <- fmap (unPos . sourceColumn) getSourcePos
        if columnBefore == 1
          then fail "starting column should be greater than 1"
          else do
            comment <- parseLineComment
            lookAhead $ do
              _ <- commentSpaceParser 6
              column <- fmap (unPos . sourceColumn) getSourcePos
              if column == 1
                then return comment
                else fail "expecting column 1",
      try $ do
        comment <-
          if afterEmptySpaceColumn > 1
            then commentSpaceParser 6
            else return ""
        lookAhead $ do
          _ <- space
          column <- fmap (unPos . sourceColumn) getSourcePos
          if column == 1 && not (isOnlyLineComment comment)
            then fail ""
            else return ()
        return comment,
      return ""
    ]

parseBranchParametersWithComments :: Text -> Text -> Int -> Parser Text
parseBranchParametersWithComments commentBefore branchName afterNameRow =
  do
    commentAfterName <- commentSpaceParser 8
    parameters <- parseTypeDeclarationParameters 2
    _ <- takeWhileP Nothing (\ch -> ch == ' ' || ch == '\n')
    afterEmptySpaceRow <- fmap (unPos . sourceLine) getSourcePos
    afterEmptySpaceColumn <- fmap (unPos . sourceColumn) getSourcePos
    commentAfter <- parseCommentAfterTypeBranch afterEmptySpaceColumn
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
                  if afterEmptySpaceRow == afterNameRow && commentAfterName /= "{--}"
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
                  if afterEmptySpaceRow == afterNameRow && commentAfterName /= "{--}"
                    then ""
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
              ( if afterEmptySpaceRow == afterNameRow && commentAfterName /= "{--}"
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
    commentBeforeEquals <- commentSpaceParser 4
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
          if parameters == "" || Text.elem '\n' parameters
            then ""
            else " ",
          parameters,
          if Text.elem '\n' parameters
            then "\n    "
            else " ",
          "=\n    ",
          commentBeforeEquals,
          if commentBeforeEquals == ""
            then ""
            else "\n    ",
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
        try $
          do
            parameterColumn <- fmap (unPos . sourceColumn) getSourcePos
            if parameterColumn <= startColumn
              then fail "invalid indentation"
              else do
                comment <- commentSpaceParser 4
                parameter <- parseParameter startColumn 0
                _ <- space
                return $
                  mconcat
                    [ if comment == ""
                        then ""
                        else
                          if comment == "{--}" || Text.elem '\n' comment
                            then "\n    "
                            else "",
                      comment,
                      if comment == ""
                        then ""
                        else
                          if comment == "{--}" || Text.elem '\n' comment
                            then "\n    "
                            else " ",
                      parameter
                    ]
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
    isMultiline_ :: Bool,
    as_ :: Text,
    exposing_ :: [[(Text, Text, Text)]],
    name_ :: Text,
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
          name_ = name',
          isMultiline_ = endRow /= startRow,
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
          name_ import_,
          if as_ import_ == "" || as_ import_ == name_ import_
            then ""
            else " as " <> as_ import_,
          if exposingText == "()"
            then ""
            else
              if isMultiline_ import_
                then "\n    exposing" <> exposingText
                else " exposing" <> exposingText
        ]

deduplicateImports :: [Text] -> [Text]
deduplicateImports imports =
  map importToText $
    deduplicateImportsHelp (map parseImportParts imports) Map.empty

combineImports :: Import -> Import -> Import
combineImports a b =
  Import
    { commentBetween = commentBetween a,
      isMultiline_ = isMultiline_ a || isMultiline_ b,
      as_ = as_ a,
      exposing_ = exposing_ a <> exposing_ b,
      name_ = name_ a,
      isMultilineExposing = isMultilineExposing a || isMultilineExposing b
    }

deduplicateImportsHelp :: [Import] -> Map Text Import -> [Import]
deduplicateImportsHelp imports accum =
  case imports of
    [] ->
      map snd $ Map.toList accum
    top : remainder ->
      case Map.lookup (name_ top) accum of
        Just current ->
          deduplicateImportsHelp
            remainder
            (Map.insert (name_ top) (combineImports top current) accum)
        Nothing ->
          deduplicateImportsHelp
            remainder
            (Map.insert (name_ top) top accum)

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
    return $
      ( if -- I know this is strange, but it's what elm-format does so I
        -- thought I would include it for compatibility.
        comment == "{--}"
          then ""
          else "\n"
      )
        <> comment

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
            block <- parseNonDocBlockComment 0
            return block
        blocks <- some $
          try $
            do
              _ <- space
              block <- parseNonDocBlockComment 0
              return block
        _ <- space
        return $ intercalate " " (block1 : blocks),
      try $ do
        _ <- space
        block <- parseNonDocBlockComment 0
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
        block <- parseNonDocBlockComment 0
        _ <- space
        return block
    ]

parseTopLevelComment :: Parser Text
parseTopLevelComment =
  do
    pieces <- some $ try parseTopLevelCommentPiece
    _ <- space
    return $ mconcat $ whitespaceInTopLevelComment pieces

whitespaceInTopLevelComment :: [Text] -> [Text]
whitespaceInTopLevelComment comments =
  case comments of
    [] ->
      []
    first : remainder ->
      first : map prefixTopLevelCommentWhitespace remainder

prefixTopLevelCommentWhitespace :: Text -> Text
prefixTopLevelCommentWhitespace comment =
  ( if comment == "{--}"
      then "\n\n\n"
      else "\n"
  )
    <> comment

parseTopLevelCommentPiece :: Parser Text
parseTopLevelCommentPiece =
  do
    _ <- space
    choice [parseNonDocBlockComment 0, parseLineComment]

parseNonDocBlockComment :: Int -> Parser Text
parseNonDocBlockComment indent =
  choice
    [ chunk "{--}",
      try $ parseNonLiteralBlockComment indent,
      parseDoubleHyphenBlockComment
    ]

parseNonLiteralBlockComment :: Int -> Parser Text
parseNonLiteralBlockComment indent =
  do
    _ <- chunk "{-"
    _ <- lookAhead $ notFollowedBy (char '|')
    _ <- lookAhead $ notFollowedBy (char '-')
    parseBlockCommentHelp indent 1 "{-"

parseDoubleHyphenBlockComment :: Parser Text
parseDoubleHyphenBlockComment =
  do
    _ <- chunk "{--"
    parseDoubleHyphenBlockCommentHelp 1 "{--"

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

parseDoubleHyphenBlockCommentHelp :: Int -> Text -> Parser Text
parseDoubleHyphenBlockCommentHelp nesting contents =
  if nesting == 0
    then return contents
    else do
      choice
        [ do
            _ <- chunk "{-"
            parseDoubleHyphenBlockCommentHelp (nesting + 1) (contents <> "{-"),
          do
            end <- choice [chunk "--}", chunk "-}"]
            parseDoubleHyphenBlockCommentHelp (nesting - 1) (contents <> end),
          do
            piece <- takeWhile1P Nothing (\ch -> ch /= '-' && ch /= '{')
            parseDoubleHyphenBlockCommentHelp nesting (contents <> piece),
          do
            _ <- char '-'
            parseDoubleHyphenBlockCommentHelp nesting (contents <> "-"),
          do
            _ <- char '{'
            parseDoubleHyphenBlockCommentHelp nesting (contents <> "{")
        ]

parseBlockCommentHelp :: Int -> Int -> Text -> Parser Text
parseBlockCommentHelp indent nesting contents =
  if nesting == 0
    then return contents
    else do
      choice
        [ do
            _ <- chunk "{-"
            parseBlockCommentHelp indent (nesting + 1) (contents <> "{-"),
          do
            _ <- chunk "-}"
            let noStart = Text.drop 2 contents
            let cleaned =
                  if Text.elem '\n' (Text.stripEnd noStart)
                    || Text.strip noStart == ""
                    then
                      if Text.stripEnd contents == "{-"
                        then contents
                        else
                          (Text.stripEnd $ fixBlockIndentation indent contents)
                            <> "\n"
                            <> Text.replicate indent " "
                    else
                      (Text.strip contents)
                        <> (if Text.strip noStart == "" then "" else " ")
            parseBlockCommentHelp indent (nesting - 1) (cleaned <> "-}"),
          do
            piece <- takeWhile1P Nothing (\ch -> ch /= '-' && ch /= '{')
            let indented = indentBlockRows piece
            let extraSpace =
                  if Text.takeEnd 2 contents == "{-" && Text.take 1 indented /= " " && Text.take 1 indented /= "\n" && Text.take 1 indented /= "|"
                    then " "
                    else ""
            parseBlockCommentHelp indent nesting (contents <> extraSpace <> indented),
          do
            _ <- char '-'
            parseBlockCommentHelp indent nesting (contents <> "-"),
          do
            _ <- char '{'
            parseBlockCommentHelp indent nesting (contents <> "{")
        ]

fixBlockIndentation :: Int -> Text -> Text
fixBlockIndentation indent text =
  let lines = Text.lines text
      firstIndent =
        case lines of
          [] ->
            0
          top : remainder ->
            if Text.take 2 top == "{-"
              then Text.length (getLeadingSpaces (Text.drop 2 top)) + 2
              else Text.length $ getLeadingSpaces top
      lowestSubsequentIndent = Prelude.minimum $ map (\line -> Text.length (getLeadingSpaces line)) (filter (\line -> line /= "" && Text.take 2 line /= "{-") lines)
      lowestIndent =
        if firstIndent < 3
          then lowestSubsequentIndent
          else Prelude.min firstIndent lowestSubsequentIndent
      stripped =
        case lines of
          [] ->
            []
          top : remainder ->
            top : map (Text.drop (lowestIndent + indent)) remainder
      withStart =
        case stripped of
          [] ->
            []
          top : remainder ->
            top : map (\item -> if item == "" then "" else Text.replicate indent " " <> "   " <> item) remainder
   in Text.unlines withStart

stripStartUpToX :: Int -> Text -> Text
stripStartUpToX max text =
  let leading = getLeadingSpaces text
      numToStrip = Prelude.min max (Text.length leading)
   in Text.drop numToStrip text

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

parseExpressionInDocs :: Int -> Context -> Int -> Parser Text
parseExpressionInDocs minColumn context indent =
  choice
    [ try $ parseCaseOf indent,
      try parseGlsl,
      try $ notFollowedByInfix $ parseList indent,
      try $ parseIfThenElse minColumn indent,
      try $ parseLetIn minColumn indent,
      try $ notFollowedByInfix $ parseRecord indent,
      try $ notFollowedByInfix $ parseRecordUpdate indent,
      try $ notFollowedByInfix parseNumberLiteral,
      try $ parseInfixed minColumn indent,
      try parseInfixInBrackets,
      try $ notDottable $ parseParenthesised context indent,
      try $ makeDottable $ parseParenthesised NeedsBrackets indent,
      parseTuple context indent,
      parseTripleStringLiteral,
      parseSimpleStringLiteral,
      parseCharLiteral,
      parseAnonymousFunction minColumn indent
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
    name <- parseName
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
        [ name,
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
    name <- parseName
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
    [ try $ parseTuplePattern NeedsBrackets indent,
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
        if ((startRow == endRow && not atLeastOneMultiline) || ((not (Text.elem '\n' arg)) && isFirstArg && argStartRow == startRow) || (argStartRow == startRow && allArgLinesInStringLiteral arg) || allLinesInStringLiteral) && comment /= "{--}"
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
          if endRow > startRow || List.any id isMultiline
            then addMultilineInfixWhitespace indent infixItem firstIsMultilineString previousIsMultiline
            else addSingleLineInfixWhitespace infixItem

        firstIsMultilineString :: Bool
        firstIsMultilineString =
          Text.take 3 firstExpression == "\"\"\""

        itemIsMultiline :: (Int, Bool, Text, Text, Text, Text) -> Bool
        itemIsMultiline (_, _, commentBefore, _, commentAfter, item) =
          Text.elem '\n' item
            || commentBefore == "{--}"
            || commentAfter == "{--}"

        isMultiline :: [Bool]
        isMultiline =
          Text.elem '\n' firstExpression : map itemIsMultiline items

    if null items
      then fail "zero infix items"
      else do
        return $
          mconcat
            [ firstExpression,
              mconcat $ map addWhitespace (List.zip items isMultiline)
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
          let floor =
               if infix_ == "<|"
               then floorToFour
               else id
          commentAfter <- commentSpaceParser (floor $ indent + 4 + Text.length infix_ + 1)
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

addMultilineInfixWhitespace ::
  Int ->
  (Int, Bool, Text, Text, Text, Text) ->
  Bool ->
  Bool ->
  Text
addMultilineInfixWhitespace
  minColumn
  ( indent,
    isOnSameRowAsPrevious,
    commentBefore,
    infix_,
    commentAfter,
    expression
    )
  precededByMultilineString
  firstIsMultiline =
    let newIndent = floorToFour indent
        lowIndent = "\n" <> replicate (Prelude.max minColumn (newIndent - 4)) " "
     in if infix_ == "<|"
          then
            mconcat
              [ if not firstIsMultiline
                  then if commentBefore == "" then " " else ""
                  else lowIndent,
                if commentBefore == ""
                  then ""
                  else lowIndent,
                commentBefore,
                if commentBefore == ""
                  then ""
                  else lowIndent,
                "<|\n",
                replicate newIndent " ",
                commentAfter,
                if commentAfter == ""
                  then ""
                  else "\n" <> replicate newIndent " ",
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
    (exponentNegative, exponent) <-
      choice
        [ try $ do
            exponentNegative <- chunk "-"
            exponent <- takeWhile1P Nothing (\ch -> ch `elem` ("0123456789" :: String))
            return (exponentNegative, exponent),
          return ("", "")
        ]
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
            try $ parseNonDocBlockComment indent,
            do
              _ <- takeWhile1P Nothing (\ch -> ch == ' ' || ch == '\n')
              return ""
          ]
    let join =
          if any isMultilineComment comments
            then "\n" <> replicate indent " "
            else " "
    return $ intercalate join (filter (\s -> s /= "") comments)

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
    let isMultiline =
          Text.elem '\n' (commentBefore <> item <> commentAfter)
    if isMultiline
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
    let isMultiline :: Bool
        isMultiline = ((endRow - startRow) - linesInMultiString) > 0
    let indentation =
          if isMultiline
            then "\n" <> replicate indent " "
            else ""
    let endSpace =
          if isMultiline
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
