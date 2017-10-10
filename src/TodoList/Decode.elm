module TodoList.Decode exposing (decode)

import TodoList.Model exposing (Todo)
import Json.Decode exposing (Decoder, decodeString, list, string, bool, int)
import Json.Decode.Pipeline as Pipeline exposing (required)


decode : String -> Result String (List Todo)
decode =
    decodeString (list todoDecoder)


todoDecoder : Decoder Todo
todoDecoder =
    Pipeline.decode Todo
        |> required "label" string
        |> required "isCompleted" bool
        |> required "id" int
