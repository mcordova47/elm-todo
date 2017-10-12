module TodoList.Decode exposing (decode)

import TodoList.Data exposing (Todo)
import Json.Decode exposing (Decoder, Value, decodeString, decodeValue, list, string, bool, int)
import Json.Decode.Pipeline as Pipeline exposing (required)


decode : Value -> Result String (List Todo)
decode value =
    value
        |> decodeValue string
        |> Result.andThen fromJson


fromJson : String -> Result String (List Todo)
fromJson =
    decodeString (list todoDecoder)


todoDecoder : Decoder Todo
todoDecoder =
    Pipeline.decode Todo
        |> required "label" string
        |> required "isCompleted" bool
        |> required "id" int
