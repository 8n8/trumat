module Trumat (trumat) where

import qualified Data.List as List
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
    (>>),
    (||),
  )

type Parser =
  Parsec Void Text

trumat :: Text -> Either String Text
trumat unformatted =
  Right unformatted
