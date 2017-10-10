port module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value, class, classList)
import Html.Events exposing (onInput, onSubmit, onClick)
import Json.Encode as Encode
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (decode, required)


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
    = ChangeDraft String
    | AddTodo
    | CompleteTodo Todo
    | RetrieveCache String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDraft draft ->
            ( { model | draftTodo = draft }
            , Cmd.none
            )

        AddTodo ->
            let
                todoNumber =
                    model.todoNumber + 1
                todoList = (Todo model.draftTodo False todoNumber) :: model.todoList
            in
                ( { model
                    | todoList = todoList
                    , draftTodo = ""
                    , todoNumber = todoNumber
                  }
                , cache (Encode.encode 0 (todoListToJson todoList))
                )

        CompleteTodo todo ->
            let
                todoList =
                    model.todoList
                        |> List.map (toggleCheckedWhen ((==) todo.id << .id))
                        |> List.sortWith compareTodos

            in
                ( { model | todoList = todoList }
                , cache (Encode.encode 0 (todoListToJson todoList))
                )

        RetrieveCache value ->
            ( { model | todoList = decodeTodoList value }
            , Cmd.none
            )


decodeTodoList value =
    case (Decode.decodeString todoListDecoder value) of
        Ok todoList ->
            todoList
        Err _ ->
            []


todoListDecoder =
    Decode.list todoDecoder


todoDecoder =
    decode Todo
        |> required "label" Decode.string
        |> required "isCompleted" Decode.bool
        |> required "id" Decode.int


todoToJson : Todo -> Encode.Value
todoToJson todo =
    Encode.object
        [ ("label", Encode.string todo.label)
        , ("isCompleted", Encode.bool todo.isCompleted)
        , ("id", Encode.int todo.id)
        ]


todoListToJson : List Todo -> Encode.Value
todoListToJson todoList =
    todoList
        |> List.map todoToJson
        |> Encode.list


toggleCheckedWhen : (Todo -> Bool) -> Todo -> Todo
toggleCheckedWhen p todo =
    if p todo then
        { todo | isCompleted = not todo.isCompleted }
    else
        todo


compareTodos : Todo -> Todo -> Order
compareTodos a b =
    case (a.isCompleted, b.isCompleted) of
        (True, True) ->
            EQ

        (False, False) ->
            EQ

        (True, False) ->
            GT

        (False, True) ->
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
