{-# LANGUAGE NoImplicitPrelude #-}

module Machine.Expression (Machine (..), ListMachine (..), SingleLineListMachine (..), run) where

import Action
import Char (Char (..))
import Prelude (Int, Show)

data Machine
  = List ListMachine Int
  | Verbatim
  deriving (Show)

data ListMachine
  = SingleLine SingleLineListMachine
  deriving (Show)

data SingleLineListMachine
  = Start
  | AfterExpression
  | AfterComma
  | Expression Machine
  | InsertedFinalSpace
  | AfterSpaceAfterComma
  | AfterCloseBracket
  | DeletingInitialSpaces
  | InsertedInitialSpace
  deriving (Show)

runList :: ListMachine -> Char -> Int -> (ListMachine, Action)
runList machine@(SingleLine singleLine) char indent =
  case singleLine of
    InsertedInitialSpace ->
      case char of
        Other ->
          (SingleLine (Expression Verbatim), MoveRight)
        CloseBracket ->
          (machine, Fail "close bracket in list after inserting initial space")
        OpenBracket ->
          (SingleLine (Expression (List (SingleLine Start) indent)), MoveRight)
        Space ->
          (machine, Fail "space in list after inserting initial space")
        Newline ->
          (machine, Fail "newline in list")
        Comma ->
          (machine, Fail "comma before initial item in list")
        Equals ->
          (machine, Fail "equals inside list")
        AfterEnd ->
          (machine, Fail "EOF inside list")
    DeletingInitialSpaces ->
      case char of
        Equals ->
          (machine, Fail "'=' in list")
        CloseBracket ->
          (SingleLine AfterCloseBracket, MoveRight)
        Space ->
          (SingleLine DeletingInitialSpaces, Delete)
        Newline ->
          (machine, Fail "newline in list")
        OpenBracket ->
          (machine, Fail "open bracket in list")
        Comma ->
          (machine, Fail "comma before first list expression")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Other ->
          (SingleLine (Expression Verbatim), InsertSpace)
    AfterCloseBracket ->
      (machine, Finish)
    AfterSpaceAfterComma ->
      case char of
        Equals ->
          (machine, Fail "'=' in list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Newline ->
          (machine, Fail "newline in list")
        CloseBracket ->
          (machine, Fail "comma at end of list")
        OpenBracket ->
          (SingleLine (Expression (List (SingleLine Start) indent)), MoveRight)
        Space ->
          (machine, Fail "too many spaces in list")
        Comma ->
          (machine, Fail "double comma in list")
        Other ->
          (SingleLine (Expression Verbatim), MoveRight)
    InsertedFinalSpace ->
      case char of
        AfterEnd ->
          (machine, Fail "EOF in list")
        Equals ->
          (machine, Fail "'=' at end of list")
        Newline ->
          (machine, Fail "newline at end of list")
        CloseBracket ->
          (SingleLine AfterCloseBracket, MoveRight)
        OpenBracket ->
          (machine, Fail "open bracket at end of list")
        Space ->
          (machine, Fail "too many spaces at end of list")
        Comma ->
          (machine, Fail "comma at end of list")
        Other ->
          (machine, Fail "other character at end of list")
    AfterComma ->
      case char of
        Equals ->
          (machine, Fail "'=' after comma in list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Newline ->
          (machine, Fail "unexpected newline after comma in list")
        Space ->
          (SingleLine AfterSpaceAfterComma, MoveRight)
        Comma ->
          (machine, Fail "unexpected double comma in list")
        CloseBracket ->
          (machine, Fail "unexpected comma at end of list")
        OpenBracket ->
          (SingleLine AfterSpaceAfterComma, InsertSpace)
        Other ->
          (SingleLine AfterSpaceAfterComma, InsertSpace)
    Start ->
      case char of
        Equals ->
          (machine, Fail "'=' at start of list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Comma ->
          (machine, Fail "unexpected comma at start of list")
        Space ->
          (SingleLine DeletingInitialSpaces, Delete)
        CloseBracket ->
          (SingleLine AfterCloseBracket, MoveRight)
        OpenBracket ->
          (SingleLine InsertedInitialSpace, InsertSpace)
        Newline ->
          (machine, Fail "unexpected newline in list")
        Other ->
          (SingleLine InsertedInitialSpace, InsertSpace)
    Expression expression ->
      let (newMachine, action) = run expression char
       in case action of
            InsertNewline ->
              (SingleLine (Expression newMachine), InsertNewline)
            Finish ->
              case char of
                Equals ->
                  (machine, Fail "'=' after expression in list")
                CloseBracket ->
                  (SingleLine InsertedFinalSpace, InsertSpace)
                OpenBracket ->
                  (machine, Fail "'[' inside list")
                Space ->
                  (SingleLine AfterExpression, Delete)
                Newline ->
                  (machine, Fail "newline inside list")
                Comma ->
                  (SingleLine AfterComma, MoveRight)
                AfterEnd ->
                  (machine, Fail "EOF inside list")
                Other ->
                  (machine, Fail "other character after expression in list")
            MoveRight ->
              (SingleLine (Expression newMachine), MoveRight)
            Delete ->
              (SingleLine (Expression newMachine), Delete)
            InsertSpace ->
              (SingleLine (Expression newMachine), InsertSpace)
            Fail message ->
              (machine, Fail message)
    AfterExpression ->
      case char of
        Equals ->
          (machine, Fail "'=' after expression in list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Space ->
          (SingleLine AfterExpression, Delete)
        CloseBracket ->
          (SingleLine InsertedFinalSpace, InsertSpace)
        OpenBracket ->
          (machine, Fail "open bracket after expression in list")
        Newline ->
          (machine, Fail "newline after expression in list")
        Comma ->
          (SingleLine AfterComma, MoveRight)
        Other ->
          (machine, Fail "other character after expression in list")

run :: Machine -> Char -> (Machine, Action)
run machine char =
  case machine of
    Verbatim ->
      case char of
        Equals ->
          (machine, Fail "'=' in verbatim")
        AfterEnd ->
          (machine, Fail "EOF in verbatim")
        CloseBracket ->
          (machine, Finish)
        Comma ->
          (machine, Finish)
        OpenBracket ->
          (machine, Fail "unexpected open bracket in verbatim")
        Space ->
          (machine, Finish)
        Newline ->
          (machine, Finish)
        Other ->
          (Verbatim, MoveRight)
    List listMachine indent ->
      let (newMachine, action) = runList listMachine char indent
       in (List newMachine indent, action)
