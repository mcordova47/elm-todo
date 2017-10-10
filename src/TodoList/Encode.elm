module TodoList.Encode exposing (encode)

import TodoList.Model exposing (Todo)
import Json.Encode exposing (Value, object, list, string, bool, int)


toJson : Todo -> Value
toJson todo =
    object
        [ ("label", string todo.label)
        , ("isCompleted", bool todo.isCompleted)
        , ("id", int todo.id)
        ]


encode : List Todo -> String
encode todoList =
    todoList
        |> List.map toJson
        |> list
        |> Json.Encode.encode 0
