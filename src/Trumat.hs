module Trumat (update, initModel, initCmd) where

-- import Array (Array)
import qualified Effect
import Data.ByteString (ByteString)

data Msg
  = FileContents (Either IOError ByteString)

-- type Model = ()

initModel =
  ()

initCmd =
  Effect.ReadFile "data.txt" FileContents

-- update :: Model -> Msg -> (Int -> Array a -> Maybe a) -> (Model, Effect.Cmd Msg b)
update model _ _ =
  (model, Effect.ReadFile "data.txt" FileContents)
