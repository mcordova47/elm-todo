module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value, class, classList)
import Html.Events exposing (onInput, onSubmit, onClick)


-- MODEL


type alias Model =
    { draftTodo : String
    , todoList : List Todo
    , todoNumber : Int
    }


type alias Todo =
    { label : String
    , isCompleted : Bool
    , id : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { draftTodo = ""
      , todoList = []
      , todoNumber = 0
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ChangeDraft String
    | AddTodo
    | CompleteTodo Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ChangeDraft draft ->
            ( { model | draftTodo = draft }
            , Cmd.none
            )

        AddTodo ->
            let
                todoNumber =
                    model.todoNumber + 1
            in
                ( { model
                    | todoList = (Todo model.draftTodo False todoNumber) :: model.todoList
                    , draftTodo = ""
                    , todoNumber = todoNumber
                  }
                , Cmd.none
                )

        CompleteTodo todo ->
            let
                todoList =
                    List.map (toggleCheckedWhen ((==) todo.id << .id)) model.todoList
            in
                ( { model | todoList = todoList }
                , Cmd.none
                )


toggleCheckedWhen : (Todo -> Bool) -> Todo -> Todo
toggleCheckedWhen p todo =
    if p todo then
        { todo | isCompleted = not todo.isCompleted }
    else
        todo



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ class "todo-list-container" ]
        [ draftTodo model.draftTodo
        , todoList model.todoList
        ]


draftTodo : String -> Html Msg
draftTodo val =
    Html.form
        [ onSubmit AddTodo
        , class "draft-todo"
        ]
        [ Html.input
            [ value val
            , onInput ChangeDraft
            ]
            []
        ]


todoList : List Todo -> Html Msg
todoList =
    Html.div [] << List.map todoItem


todoItem : Todo -> Html Msg
todoItem todo =
    Html.div
        [ todoItemClass todo ]
        [ checkbox (CompleteTodo todo) todo.isCompleted
        , Html.div
            [ class "todo-item__label" ]
            [ Html.text todo.label ]
        ]


todoItemClass : Todo -> Html.Attribute Msg
todoItemClass todo =
    classList
        [ ( "todo-item", True )
        , ( "todo-item--completed", todo.isCompleted )
        ]


checkbox : msg -> Bool -> Html msg
checkbox msg checked =
    let
        kind =
            if checked then
                "check_box"
            else
                "check_box_outline_blank"
    in
        Html.div
            [ class "todo-item__checkbox"
            , onClick msg
            ]
            [ materialIcon kind ]


materialIcon : String -> Html msg
materialIcon kind =
    Html.i
        [ class "material-icons" ]
        [ Html.text kind ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
