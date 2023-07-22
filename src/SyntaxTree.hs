module SyntaxTree (SyntaxTree, empty, fromTokens) where


import Tokens (Tokens)
import qualified Tokens
import Result
import Data.IORef (IORef, newIORef)
import Dict (Dict)
import qualified Dict
import Data.Word (Word16, Word32)
import Columns (Columns)
import qualified Columns
import Rows (Rows)
import Set2 (Set2)
import qualified Set2
import qualified Rows
import TokenTag
import Set1 (Set1)
import qualified Set1
import qualified PlainBindMachine
import PlainBindMachine (PlainBindMachine)

data SyntaxTree
    = SyntaxTree
    { moduleName :: IORef (Maybe Word32)
    , export :: Set1
    , topBindLeft :: Set1
    , topBindRight :: Set1
    , parameter :: Dict
    , unitValue :: Set1
    , token :: Dict
    , nextId :: IORef Word32
    }

data Transaction
    = ModuleName Word32
    | IsExport Word32
    | TopBindLeft Word32
    | TopBindRight Word32
    | Parameter Word32 Word32
    | TokenId Word32


empty :: IO SyntaxTree
empty =
    do
    moduleName <- newIORef Nothing
    export <- Set1.empty 1000
    topLevelBind <- Set2.empty 10000
    parameter <- Dict.empty 10000
    nextId <- newIORef Tokens.capacity
    pure $ SyntaxTree moduleName export topLevelBind parameter nextId


fromTokens :: Tokens -> Rows -> Columns -> SyntaxTree -> IO Result
fromTokens tokens rows columns syntaxTree =
    fromTokensHelp tokens rows columns 0 syntaxTree Start


fromTokensHelp :: Tokens -> Rows -> Columns -> Int -> SyntaxTree -> State -> IO Result
fromTokensHelp tokens rows columns index syntaxTree state =
    do
    tokenResult <- Tokens.get index tokens
    columnResult <- Columns.get index columns
    rowResult <- Rows.get index rows
    case (tokenResult, columnResult, rowResult) of
        (Left _, _, _) ->
            pure $ Ok
        (_, Left _, _) ->
            pure $ Ok
        (_, _, Left _) ->
            pure $ Ok
        (Right token, Right column, Right row) ->
            case step (Tokens.toTag token) column row state (fromIntegral index) of
                Next newState ->
                    fromTokensHelp tokens rows columns (index + 1) syntaxTree newState


data Action
    = Next State [Transaction]
    | Fail State TokenTag Word16 Word16

data Commit
    = TopLevelBindName

data State
    = Start
    | AfterModule
    | InTopLevelBind Word32 PlainBindMachine
    deriving Show

step :: TokenTag -> Word16 -> Word16 -> State -> Word32 -> Action
step token row column state tokenId =
    let
        fail :: Action
        fail = Fail state token row column
    in
    case (state, token) of
        (Start, Module) ->
            Next AfterModule
        (AfterModule, Module) ->
            fail
        (Start, UpperName) ->
            fail
        (AfterModule, UpperName) ->
            fail
        (AfterModule, LowerName) ->
            fail
        (AfterModule, CloseParentheses) ->
            fail
        (AfterModule, OpenParentheses) ->
            fail
        (Start, LowerName) ->
            Next
                (InTopLevelBind PlainBindMachine.start)
                [ TopBindLeft tokenId ]
        (InTopLevelBind bindMachine, token) ->
            case PlainBindMachine.step token bindMachine row column of
                PlainBindMachine.Next newBindMachine transactions ->
                    Next (InTopLevelBind newBindMachine) transactions
