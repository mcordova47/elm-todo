module TodoList.Data exposing (Todo, compare)


type alias Todo =
    { label : String
    , isCompleted : Bool
    , id : Int
    }


compare : Todo -> Todo -> Order
compare a b =
    case ( a.isCompleted, b.isCompleted ) of
        ( True, True ) ->
            EQ

        ( False, False ) ->
            EQ

        ( True, False ) ->
            GT

        ( False, True ) ->
            LT
