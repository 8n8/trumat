{-# LANGUAGE NoImplicitPrelude #-}

module Machine.Expression (Machine (..), ListMachine (..), InlineListMachine (..), run) where

import Action
import Char (Char (..))
import Prelude (Int, Show)

data Machine
  = List ListMachine Int
  | Verbatim
  deriving (Show)

data ListMachine
  = Inline InlineListMachine
  | Broken BrokenListMachine
  deriving (Show)

data BrokenListMachine
  = DeletingInitialWhitespace
  | ExpressionB Machine
  | AfterCloseBracketB
  | DeletingInitialSpacesB
  | InsertedFinalSpaceB
  deriving (Show)

data InlineListMachine
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
    Broken brokenMachine ->
        runBrokenList brokenMachine char indent

    Inline inlineMachine ->
        runInlineList inlineMachine char indent

runBrokenList :: BrokenListMachine -> Char -> Int -> (ListMachine, Action)
runBrokenList broken char indent=
  let
    machine = Broken broken
  in
  case broken of
    ExpressionB expression ->
      let (newMachine, action) = run expression char
      in case action of
            InsertNewline ->
                (Broken (Expression newMachine), InsertNewline)
            Finish ->
              case char of
                Equals ->
                    (machine, Fail "'=' after expression in newline list")
                CloseBracket ->
                  ( InsertedFinalSpaceB, InsertNewline)
                
    DeletingInitialWhitespace ->
      case char of
        Comma ->
          (machine, Fail "comma at start of newline list")
        Equals ->
          (machine, Fail "equals at start of newline list")
        AfterEnd ->
          (machine, Fail "EOF at start of newline list")
        Other ->
          (Broken (ExpressionB Verbatim), InsertSpace)

        CloseBracket ->
          (Broken AfterCloseBracketB, MoveRight)

        OpenBracket ->
          (Broken (ExpressionB (List (Inline Start) indent)), MoveRight)
        Space ->
          (Broken DeletingInitialSpacesB, Delete)
        Newline ->
          (Broken DeletingInitialSpacesB, Delete)
          

runInlineList :: InlineListMachine -> Char -> Int -> (ListMachine, Action)
runInlineList singleLine char indent =
  let
    machine = Inline singleLine
  in
  case singleLine of
    InsertedInitialSpace ->
      case char of
        Other ->
          (Inline (Expression Verbatim), MoveRight)
        CloseBracket ->
          (machine, Fail "close bracket in list after inserting initial space")
        OpenBracket ->
          (Inline (Expression (List (Inline Start) indent)), MoveRight)
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
          (Inline AfterCloseBracket, MoveRight)
        Space ->
          (Inline DeletingInitialSpaces, Delete)
        Newline ->
          (machine, Fail "newline in list")
        OpenBracket ->
          (machine, Fail "open bracket in list")
        Comma ->
          (machine, Fail "comma before first list expression")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Other ->
          (Inline (Expression Verbatim), InsertSpace)
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
          (Inline (Expression (List (Inline Start) indent)), MoveRight)
        Space ->
          (machine, Fail "too many spaces in list")
        Comma ->
          (machine, Fail "double comma in list")
        Other ->
          (Inline (Expression Verbatim), MoveRight)
    InsertedFinalSpace ->
      case char of
        AfterEnd ->
          (machine, Fail "EOF in list")
        Equals ->
          (machine, Fail "'=' at end of list")
        Newline ->
          (machine, Fail "newline at end of list")
        CloseBracket ->
          (Inline AfterCloseBracket, MoveRight)
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
          (Inline AfterSpaceAfterComma, MoveRight)
        Comma ->
          (machine, Fail "unexpected double comma in list")
        CloseBracket ->
          (machine, Fail "unexpected comma at end of list")
        OpenBracket ->
          (Inline AfterSpaceAfterComma, InsertSpace)
        Other ->
          (Inline AfterSpaceAfterComma, InsertSpace)
    Start ->
      case char of
        Equals ->
          (machine, Fail "'=' at start of list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Comma ->
          (machine, Fail "unexpected comma at start of list")
        Space ->
          (Inline DeletingInitialSpaces, Delete)
        CloseBracket ->
          (Inline AfterCloseBracket, MoveRight)
        OpenBracket ->
          (Inline InsertedInitialSpace, InsertSpace)
        Newline ->
          (Broken DeletingInitialWhitespace, Delete)
        Other ->
          (Inline InsertedInitialSpace, InsertSpace)
    Expression expression ->
      let (newMachine, action) = run expression char
       in case action of
            InsertNewline ->
              (Inline (Expression newMachine), InsertNewline)
            Finish ->
              case char of
                Equals ->
                  (machine, Fail "'=' after expression in list")
                CloseBracket ->
                  (Inline InsertedFinalSpace, InsertSpace)
                OpenBracket ->
                  (machine, Fail "'[' inside list")
                Space ->
                  (Inline AfterExpression, Delete)
                Newline ->
                  (machine, Fail "newline inside list")
                Comma ->
                  (Inline AfterComma, MoveRight)
                AfterEnd ->
                  (machine, Fail "EOF inside list")
                Other ->
                  (machine, Fail "other character after expression in list")
            MoveRight ->
              (Inline (Expression newMachine), MoveRight)
            Delete ->
              (Inline (Expression newMachine), Delete)
            InsertSpace ->
              (Inline (Expression newMachine), InsertSpace)
            Fail message ->
              (machine, Fail message)
    AfterExpression ->
      case char of
        Equals ->
          (machine, Fail "'=' after expression in list")
        AfterEnd ->
          (machine, Fail "EOF in list")
        Space ->
          (Inline AfterExpression, Delete)
        CloseBracket ->
          (Inline InsertedFinalSpace, InsertSpace)
        OpenBracket ->
          (machine, Fail "open bracket after expression in list")
        Newline ->
          (machine, Fail "newline after expression in list")
        Comma ->
          (Inline AfterComma, MoveRight)
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
