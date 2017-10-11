port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value, class, classList, placeholder)
import Html.Events exposing (onInput, onSubmit, onClick)
import TodoList.Decode exposing (decode)
import TodoList.Encode exposing (encode)
import TodoList.Model exposing (Todo)


-- MODEL


type alias Model =
    { draftTodo : String
    , todoList : List Todo
    , todoNumber : Int
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
    = ChangeDraft String
    | AddTodo
    | CompleteTodo Todo
    | RetrieveCache String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDraft draft ->
            { model | draftTodo = draft } ! []

        AddTodo ->
            let
                todoNumber =
                    model.todoNumber + 1

                todoList =
                    (Todo model.draftTodo False todoNumber) :: model.todoList
            in
                { model
                    | todoList = todoList
                    , draftTodo = ""
                    , todoNumber = todoNumber
                }
                    ! [ cache (encode todoList) ]

        CompleteTodo todo ->
            let
                todoList =
                    model.todoList
                        |> List.map (toggleCheckedWhen ((==) todo.id << .id))
                        |> List.sortWith compareTodos
            in
                { model | todoList = todoList }
                    ! [ cache (encode todoList) ]

        RetrieveCache value ->
            let
                todoList =
                    decode value
                        |> Result.withDefault []

                todoNumber =
                    List.maximum (List.map .id todoList)
                        |> Maybe.withDefault 0
            in
                { model
                    | todoList = todoList
                    , todoNumber = todoNumber
                }
                    ! []


toggleCheckedWhen : (Todo -> Bool) -> Todo -> Todo
toggleCheckedWhen p todo =
    if p todo then
        { todo | isCompleted = not todo.isCompleted }
    else
        todo


compareTodos : Todo -> Todo -> Order
compareTodos a b =
    case ( a.isCompleted, b.isCompleted ) of
        ( True, True ) ->
            EQ

        ( False, False ) ->
            EQ

        ( True, False ) ->
            GT

        ( False, True ) ->
            LT


port cache : String -> Cmd msg



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
            , placeholder "Enter a Task"
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


port retrieve : (String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    retrieve RetrieveCache


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
