{-# LANGUAGE NoImplicitPrelude #-}

module Machine.Expression (Machine (..), ListMachine (..), run) where

import Action
import Char (Char (..))
import Prelude (Int, Show)

data Machine
  = List ListMachine Int
  | Verbatim
  deriving (Show)

data ListMachine
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
runList machine char indent =
  case machine of
    InsertedInitialSpace ->
      case char of
        Other ->
          (Expression Verbatim, MoveRight)
        CloseBracket ->
          (machine, Fail "close bracket in list after inserting initial space")
        OpenBracket ->
          (Expression (List Start indent), MoveRight)
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
          (AfterCloseBracket, MoveRight)
        Space ->
          (DeletingInitialSpaces, Delete)
        Newline ->
          (machine, Fail "newline in list")
        OpenBracket ->
          (machine, Fail "open bracket in list")
        Comma ->
          (machine, Fail "comma before first list expression")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Other ->
          (Expression Verbatim, InsertSpace)
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
          (Expression (List Start indent), MoveRight)
        Space ->
          (machine, Fail "too many spaces in list")
        Comma ->
          (machine, Fail "double comma in list")
        Other ->
          (Expression Verbatim, MoveRight)
    InsertedFinalSpace ->
      case char of
        AfterEnd ->
          (machine, Fail "EOF in list")
        Equals ->
          (machine, Fail "'=' at end of list")
        Newline ->
          (machine, Fail "newline at end of list")
        CloseBracket ->
          (AfterCloseBracket, MoveRight)
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
          (AfterSpaceAfterComma, MoveRight)
        Comma ->
          (machine, Fail "unexpected double comma in list")
        CloseBracket ->
          (machine, Fail "unexpected comma at end of list")
        OpenBracket ->
          (AfterSpaceAfterComma, InsertSpace)
        Other ->
          (AfterSpaceAfterComma, InsertSpace)
    Start ->
      case char of
        Equals ->
          (machine, Fail "'=' at start of list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Comma ->
          (machine, Fail "unexpected comma at start of list")
        Space ->
          (DeletingInitialSpaces, Delete)
        CloseBracket ->
          (AfterCloseBracket, MoveRight)
        OpenBracket ->
          (InsertedInitialSpace, InsertSpace)
        Newline ->
          (machine, Fail "unexpected newline in list")
        Other ->
          (InsertedInitialSpace, InsertSpace)
    Expression expression ->
      let (newMachine, action) = run expression char
       in case action of
            InsertNewline ->
              (Expression newMachine, InsertNewline)
            Finish ->
              case char of
                Equals ->
                  (machine, Fail "'=' after expression in list")
                CloseBracket ->
                  (InsertedFinalSpace, InsertSpace)
                OpenBracket ->
                  (machine, Fail "'[' inside list")
                Space ->
                  (AfterExpression, Delete)
                Newline ->
                  (machine, Fail "newline inside list")
                Comma ->
                  (AfterComma, MoveRight)
                AfterEnd ->
                  (machine, Fail "EOF inside list")
                Other ->
                  (machine, Fail "other character after expression in list")
            MoveRight ->
              (Expression newMachine, MoveRight)
            Delete ->
              (Expression newMachine, Delete)
            InsertSpace ->
              (Expression newMachine, InsertSpace)
            Fail message ->
              (machine, Fail message)
    AfterExpression ->
      case char of
        Equals ->
          (machine, Fail "'=' after expression in list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Space ->
          (AfterExpression, Delete)
        CloseBracket ->
          (InsertedFinalSpace, InsertSpace)
        OpenBracket ->
          (machine, Fail "open bracket after expression in list")
        Newline ->
          (machine, Fail "newline after expression in list")
        Comma ->
          (AfterComma, MoveRight)
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
