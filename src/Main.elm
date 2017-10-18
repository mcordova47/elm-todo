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
import Alert


-- MODEL


type alias Model =
    { draftTodo : String
    , todoList : Dict Int Todo
    , alert : Alert.Model
    }


init : ( Model, Cmd Msg )
init =
    { draftTodo = ""
    , todoList = Dict.empty
    , alert = Alert.empty
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
    | AlertMsg Alert.Msg


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
                            Alert.empty

                        Err msg ->
                            Alert.singleton msg
            in
                { model | todoList = todoList }
                    ! [ Cmd.map AlertMsg (Alert.add alert) ]

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

        AlertMsg alertMsg ->
            let
                ( alert, cmd ) =
                    Alert.update alertMsg model.alert
            in
                { model | alert = alert }
                    ! [ Cmd.map AlertMsg cmd ]


port cache : String -> Cmd msg



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ todoListContainer model
        , Html.map AlertMsg
            (Alert.view model.alert)
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
        [ clearAll todoList
        , clearCompleted todoList
        ]


clearAll : Dict Int Todo -> Html Msg
clearAll todoList =
    ListControls.deleteAll ClearAll
        |> renderIf (not (Dict.isEmpty todoList))


clearCompleted : Dict Int Todo -> Html Msg
clearCompleted todoList =
    ListControls.delete ClearCompleted
        |> renderIf
            (List.any .isCompleted (Dict.values todoList))


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
