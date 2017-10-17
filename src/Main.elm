port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value, class, classList, placeholder)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode exposing (Value)
import TodoList.Decode exposing (decode)
import TodoList.Encode exposing (encode)
import TodoList.Data as Data exposing (Todo)
import ListControls
import Utils exposing (renderIf, identityInsert, on)
import Dict exposing (Dict)
import Time
import Task
import Process


-- MODEL


type alias Model =
    { draftTodo : String
    , todoList : Dict Int Todo
    , alert : Maybe Alert
    }


type alias Alert =
    { message : String
    , kind : AlertKind
    }


type AlertKind
    = Success
    | Info
    | Warning
    | Error


init : ( Model, Cmd Msg )
init =
    { draftTodo = ""
    , todoList = Dict.empty
    , alert = Nothing
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
    | AddAlert Alert
    | RemoveAlert


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
                result =
                    decode value

                todoList =
                    result
                        |> Result.withDefault Dict.empty

                alert =
                    case result of
                        Ok _ ->
                            Nothing

                        Err msg ->
                            Just <| Alert msg Error
            in
                { model | todoList = todoList }
                    ! [ addAlert alert ]

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

        AddAlert alert ->
            { model | alert = Just alert }
                ! [ removeAlert ]

        RemoveAlert ->
            { model | alert = Nothing } ! []


port cache : String -> Cmd msg


addAlert : Maybe Alert -> Cmd Msg
addAlert maybeAlert =
    case maybeAlert of
        Just alert ->
            Task.succeed alert
                |> Task.perform AddAlert

        Nothing ->
            Cmd.none


removeAlert : Cmd Msg
removeAlert =
    Process.sleep (2 * Time.second)
        |> Task.perform (\_ -> RemoveAlert)


-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ todoListContainer model
        , alertMessage model.alert
        ]


todoListContainer : Model -> Html Msg
todoListContainer model =
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
                |> renderIf (not (Dict.isEmpty todoList))

        clearCompleted =
            ListControls.delete ClearCompleted
                |> renderIf
                    (List.any .isCompleted (Dict.values todoList))
    in
        [ clearAll
        , clearCompleted
        ]


alertMessage : Maybe Alert -> Html msg
alertMessage maybeAlert =
    case maybeAlert of
        Just alert ->
            Html.div
                [ class "alert-message" ]
                [ Html.text alert.message ]

        Nothing ->
            Html.text ""


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
