module TodoList.Encode exposing (encode)

import TodoList.Data exposing (Todo)
import Json.Encode exposing (Value, object, list, string, bool, int)
import Dict exposing (Dict)


toJson : Todo -> Value
toJson todo =
    object
        [ ("label", string todo.label)
        , ("isCompleted", bool todo.isCompleted)
        ]


encode : Dict Int Todo -> String
encode todoList =
    todoList
        |> Dict.toList
        |> List.map
            (Tuple.mapFirst toString << Tuple.mapSecond toJson)
        |> object
        |> Json.Encode.encode 0
