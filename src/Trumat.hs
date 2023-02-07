module Trumat (trumat) where

import Action
import Char
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import GapBuffer
  ( GapBuffer,
    delete,
    fill,
    get,
    insertNewline,
    insertSpace,
    moveRight,
    new,
    position,
    read,
  )
import Machine (Machine (..), run)
import Prelude (Either (..), Maybe (..), String, mconcat, return, show, ($))

trumat :: B.ByteString -> Either String B.ByteString
trumat raw =
  runST $
    do
      buffer <- GapBuffer.new
      result <- GapBuffer.fill raw buffer
      case result of
        Nothing ->
          return $ Left "couldn't fill gap buffer"
        Just () ->
          do
            char <- GapBuffer.get buffer
            trumatHelp buffer BeforeEquals char

trumatHelp ::
  GapBuffer s ->
  Machine ->
  Char ->
  ST s (Either String B.ByteString)
trumatHelp buffer oldMachine char =
  do
    let (newMachine, action) = Machine.run oldMachine char
    case action of
      InsertSpace ->
        do
          GapBuffer.insertSpace buffer
          ch <- GapBuffer.get buffer
          trumatHelp buffer newMachine ch
      InsertNewline ->
        do
          GapBuffer.insertNewline buffer
          ch <- GapBuffer.get buffer
          trumatHelp buffer newMachine ch
      MoveRight ->
        do
          GapBuffer.moveRight buffer
          ch <- GapBuffer.get buffer
          trumatHelp buffer newMachine ch
      Delete ->
        do
          GapBuffer.delete buffer
          ch <- GapBuffer.get buffer
          trumatHelp buffer newMachine ch
      Finish ->
        do
          result <- GapBuffer.read buffer
          return $ Right result
      Fail message ->
        do
          pos <- GapBuffer.position buffer
          word <- GapBuffer.get buffer
          return $
            Left $
              mconcat
                [ "error at offset ",
                  show pos,
                  " on word ",
                  show word,
                  ": ",
                  message
                ]
