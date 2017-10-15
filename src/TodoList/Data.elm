module TodoList.Data exposing (Todo, compare, on)


type alias Todo =
    { label : String
    , isCompleted : Bool
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


on : (a -> b) -> (b -> b -> c) -> a -> a -> c
on mapFn fn x y =
    fn (mapFn x) (mapFn y)
