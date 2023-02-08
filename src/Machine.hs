{-# LANGUAGE NoImplicitPrelude #-}

module Machine (Machine.Machine (..), Machine.run) where

import Action
import Char
import qualified Machine.Expression
import Prelude (Show)

data Machine
  = BeforeEquals
  | WhitespaceAfterEquals
  | Expression Machine.Expression.Machine
  | AfterFinalNewline
  deriving (Show)

run :: Machine -> Char -> (Machine, Action)
run machine char =
  case machine of
    AfterFinalNewline ->
      case char of
        Equals ->
          (machine, Fail "'=' instead of EOF")
        Comma ->
          (machine, Fail "comma instead of EOF")
        CloseBracket ->
          (machine, Fail "close bracket instead of EOF")
        OpenBracket ->
          (machine, Fail "open bracket instead of EOF")
        Other ->
          (machine, Fail "other character instead of EOF")
        Space ->
          (machine, Fail "space after final newline")
        Newline ->
          (machine, Fail "extra final newline")
        AfterEnd ->
          (machine, Finish)
    Expression expression ->
      let (newMachine, action) = Machine.Expression.run expression char
       in case action of
            InsertNewline ->
              (Expression newMachine, InsertNewline)
            Finish ->
              case char of
                Newline ->
                  (AfterFinalNewline, MoveRight)
                CloseBracket ->
                  (machine, Fail "close bracket after main expression")
                OpenBracket ->
                  (machine, Fail "open bracket after main expression")
                Space ->
                  (machine, Fail "space after main expression")
                Comma ->
                  (machine, Fail "comma after main expression")
                Equals ->
                  (machine, Fail "equals after main expression")
                AfterEnd ->
                  (machine, Fail "EOF instead of trailing newline")
                Other ->
                  (machine, Fail "other character instead of trailing newline")
            MoveRight ->
              (Expression newMachine, MoveRight)
            Delete ->
              (Expression newMachine, Delete)
            InsertSpace ->
              (Expression newMachine, InsertSpace)
            Fail message ->
              (machine, Fail message)
    WhitespaceAfterEquals ->
      case char of
        Equals ->
          (machine, Fail "second '='")
        AfterEnd ->
          (machine, Fail "EOF after equals in top-level bind")
        Space ->
          (WhitespaceAfterEquals, MoveRight)
        CloseBracket ->
          (machine, Fail "close bracket after equals")
        OpenBracket ->
          ( Expression (Machine.Expression.List (Machine.Expression.Inline Machine.Expression.Start) 4),
            MoveRight
          )
        Other ->
          (Expression Machine.Expression.Verbatim, MoveRight)
        Newline ->
          (WhitespaceAfterEquals, MoveRight)
        Comma ->
          (machine, Fail "comma after equals")
    BeforeEquals ->
      case char of
        Equals ->
          (WhitespaceAfterEquals, MoveRight)
        AfterEnd ->
          (machine, Fail "EOF before equals in top-level bind")
        Space ->
          (BeforeEquals, MoveRight)
        Other ->
          (BeforeEquals, MoveRight)
        CloseBracket ->
          (machine, Fail "close bracket before equals in module")
        OpenBracket ->
          (machine, Fail "open bracket before equals in module")
        Newline ->
          (BeforeEquals, MoveRight)
        Comma ->
          (machine, Fail "comma before equals in module")
