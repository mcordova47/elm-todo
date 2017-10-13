module TodoList.Encode exposing (encode)

import TodoList.Data exposing (Todo)
import Json.Encode exposing (Value, object, list, string, bool, int)
import Dict exposing (Dict)


todo : Todo -> Value
todo { label, isCompleted } =
    object
        [ ("label", string label)
        , ("isCompleted", bool isCompleted)
        ]


encode : Dict Int Todo -> String
encode todoList =
    todoList
        |> Dict.toList
        |> List.map (tuple2 int todo)
        |> list
        |> Json.Encode.encode 0


tuple2 : (a -> Value) -> (b -> Value) -> ( a, b ) -> Value
tuple2 leftEncoder rightEncoder ( left, right ) =
    list 
        [ leftEncoder left
        , rightEncoder right
        ]
