module ModuleDeclaration (parse) where

import Data.Text (Text)
import Prelude
    (Bool
    , mconcat
    , ($)
    , return
    , (/=)
    , Maybe(Nothing)
    , (<>)
    , (==)
    )
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec
  , chunk
  , takeWhileP
  , choice
  )
import Text.Megaparsec.Char (char, space)

parseExports :: Parser Text
parseExports =
  do
    listType <- lookAhead parseListType
    case listType of
      SingleLine ->
        parseSingleLineExports
      MultiLine ->
        parseMultiLineExports

data ContainerType
  = SingleLine
  | MultiLine

type Parser =
  Parsec Void Text

data ModuleDeclaration
    = ModuleDeclaration
    { exportList :: [Text]
    , hasNewlines :: Bool
    , documented :: [[Text]]
    }

parse :: Parser Text
parse =
  do
    moduleDeclaration <- parseModuleDeclaration
    return $ format moduleDeclaration


parseModuleDeclaration :: Parser ModuleDeclaration
parseModuleDeclaration =
  do
    _ <- chunk "module "
    name <- parseName
    _ <- chunk " exposing"
    _ <- space
    exports <- parseExports
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
