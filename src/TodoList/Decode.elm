module TodoList.Decode exposing (decode)

import TodoList.Data exposing (Todo)
import Json.Decode exposing (Decoder, Value, decodeString, decodeValue, list, string, bool, int, at, map2)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Dict exposing (Dict)


decode : Value -> Result String (Dict Int Todo)
decode value =
    value
        |> decodeValue string
        |> Result.andThen fromJson
        |> Result.map Dict.fromList


fromJson : String -> Result String (List ( Int, Todo ))
fromJson =
    decodeString (list todoRow)


todo : Decoder Todo
todo =
    Pipeline.decode Todo
        |> required "label" string
        |> required "isCompleted" bool


todoRow : Decoder ( Int, Todo )
todoRow =
    Pipeline.decode (,)
        |> required "0" int
        |> required "1" todo
