module TodoList.Decode exposing (decode)

import TodoList.Data exposing (Todo)
import Json.Decode exposing (Decoder, Value, decodeString, decodeValue, dict, string, bool, int)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Dict exposing (Dict)
import Utils


decode : Value -> Result String (Dict Int Todo)
decode value =
    value
        |> decodeValue string
        |> Result.andThen fromJson
        |> Result.andThen (Utils.mapKeysOrFail String.toInt)


fromJson : String -> Result String (Dict String Todo)
fromJson =
    decodeString (dict todoDecoder)


todoDecoder : Decoder Todo
todoDecoder =
    Pipeline.decode Todo
        |> required "label" string
        |> required "isCompleted" bool
