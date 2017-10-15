port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value, class, classList, placeholder)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode exposing (Value)
import TodoList.Decode exposing (decode)
import TodoList.Encode exposing (encode)
import TodoList.Data as Data exposing (Todo, on)
import ListControls
import Utils exposing (onlyIf, mapIf, identityInsert)
import Dict exposing (Dict)


-- MODEL


type alias Model =
    { draftTodo : String
    , todoList : Dict Int Todo
    }


init : ( Model, Cmd Msg )
init =
    { draftTodo = ""
    , todoList = Dict.empty
    }
        ! []



-- UPDATE


type Msg
    = ChangeDraft String
    | AddTodo
    | CompleteTodo Int
    | RetrieveCache Value
    | ClearCompleted
    | ClearAll


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDraft draft ->
            { model | draftTodo = draft } ! []

        AddTodo ->
            let
                newTodo =
                    Todo model.draftTodo False

                todoList =
                    identityInsert newTodo model.todoList
            in
                { model
                    | todoList = todoList
                    , draftTodo = ""
                }
                    ! [ cache (encode todoList) ]

        CompleteTodo id ->
            let
                toggleCompleted todo =
                    { todo | isCompleted = not todo.isCompleted }

                todoList =
                    Dict.update
                        id
                        (Maybe.map toggleCompleted)
                        model.todoList
            in
                { model | todoList = todoList }
                    ! [ cache (encode todoList) ]

        RetrieveCache value ->
            let
                todoList =
                    decode value
                        |> Result.withDefault Dict.empty
            in
                { model | todoList = todoList } ! []

        ClearCompleted ->
            let
                todoList =
                    Dict.filter
                        (\_ todo -> not todo.isCompleted)
                        model.todoList
            in
                { model | todoList = todoList }
                    ! [ cache (encode todoList) ]

        ClearAll ->
            { model | todoList = Dict.empty }
                ! [ cache (encode Dict.empty) ]


port cache : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ class "todo-list-container" ]
        [ draftTodo model.draftTodo
        , todoList model.todoList
        , controlPanel model.todoList
        ]


controlPanel : Dict Int Todo -> Html Msg
controlPanel todoList =
    Html.div
        [ class "control-panel" ]
        (controls todoList)


controls : Dict Int Todo -> List (Html Msg)
controls todoList =
    let
        clearAll =
            ListControls.deleteAll ClearAll
                |> onlyIf (not (Dict.isEmpty todoList))

        clearCompleted =
            ListControls.delete ClearCompleted
                |> onlyIf
                    (List.any .isCompleted (Dict.values todoList))
    in
        List.filterMap identity
            [ clearAll
            , clearCompleted
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


todoList : Dict Int Todo -> Html Msg
todoList todos =
    todos
        |> Dict.toList
        |> List.sortWith (Data.compare |> on Tuple.second)
        |> List.map todoItem
        |> Html.div []


todoItem : ( Int, Todo ) -> Html Msg
todoItem ( id, todo ) =
    Html.div
        [ todoItemClass todo ]
        [ ListControls.checkbox (CompleteTodo id) todo.isCompleted
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



-- SUBSCRIPTIONS


port retrieve : (Value -> msg) -> Sub msg


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
