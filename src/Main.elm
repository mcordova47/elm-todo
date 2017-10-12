port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value, class, classList, placeholder)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode exposing (Value)
import TodoList.Decode exposing (decode)
import TodoList.Encode exposing (encode)
import TodoList.Data as Data exposing (Todo)
import ListControls
import Utils exposing (onlyIf, mapIf)


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
                        |> mapIf ((==) todo.id << .id) toggleCompleted
                        |> List.sortWith Data.compare
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

        ClearCompleted ->
            let
                todoList =
                    List.filter (not << .isCompleted) model.todoList
            in
                { model | todoList = todoList }
                    ! [ cache (encode todoList) ]

        ClearAll ->
            { model | todoList = [] }
                ! [ cache (encode []) ]


toggleCompleted : Todo -> Todo
toggleCompleted todo =
    { todo | isCompleted = not todo.isCompleted }


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


controlPanel : List Todo -> Html Msg
controlPanel todoList =
    Html.div
        [ class "control-panel" ]
        (controls todoList)


controls : List Todo -> List (Html Msg)
controls todoList =
    let
        clearAll =
            ListControls.deleteAll ClearAll
                |> onlyIf (not (List.isEmpty todoList))
        clearCompleted =
            ListControls.delete ClearCompleted
                |> onlyIf (List.any .isCompleted todoList)
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


todoList : List Todo -> Html Msg
todoList =
    Html.div [] << List.map todoItem


todoItem : Todo -> Html Msg
todoItem todo =
    Html.div
        [ todoItemClass todo ]
        [ ListControls.checkbox (CompleteTodo todo) todo.isCompleted
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
