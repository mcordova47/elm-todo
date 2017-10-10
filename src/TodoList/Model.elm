module TodoList.Model exposing (Todo)

type alias Todo =
    { label : String
    , isCompleted : Bool
    , id : Int
    }
