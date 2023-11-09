port module Data.Analytics exposing (gotJsError, reportError)

import Constant
import Data.Exit as Exit
import Data.Frame as Frame
import Data.Registry.Package as Pkg
import Data.Time
import DateFormat as F
import Dict exposing (Dict)
import Http
import Json.Decode as JD
import Json.Encode as JE
import Set
import Time



-- API


port gotJsError : (String -> msg) -> Sub msg


reportError : (Result Http.Error String -> msg) -> String -> Cmd msg
reportError onResult errMsg =
    Http.post
        { url = Constant.server ++ "/api/analytics/new-ui-error"
        , body = Http.jsonBody (JE.string errMsg)
        , expect = Http.expectJson onResult JD.string
        }
