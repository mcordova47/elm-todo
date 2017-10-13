module TodoList.Encode exposing (encode)

import TodoList.Data exposing (Todo)
import Json.Encode exposing (Value, object, list, string, bool, int)
import Dict exposing (Dict)


encode : Dict Int Todo -> String
encode todoList =
    todoList
        |> Dict.toList
        |> List.map todoRow
        |> list
        |> Json.Encode.encode 0


todoRow : ( Int, Todo ) -> Value
todoRow ( key, value ) =
    list 
        [ int key
        , todo value
        ]


todo : Todo -> Value
todo { label, isCompleted } =
    object
        [ ("label", string label)
        , ("isCompleted", bool isCompleted)
        ]
