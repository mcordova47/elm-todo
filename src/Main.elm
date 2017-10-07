module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput, onSubmit)


-- MODEL


type alias Model =
    { draftTodo : String
    , todoList : List Todo
    }


type alias Todo =
    { label : String
    , isCompleted : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { draftTodo = ""
      , todoList = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ChangeDraft String
    | AddTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        ChangeDraft draft ->
            ( { model | draftTodo = draft }, Cmd.none )
        AddTodo ->
            ( { model
              | todoList = (Todo model.draftTodo False) :: model.todoList
              , draftTodo = ""
              }
            , Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        [ draftTodo model.draftTodo
        , todoList model.todoList
        ]


draftTodo : String -> Html Msg
draftTodo val =
    Html.form
        [ onSubmit AddTodo
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
    Html.div [] [ Html.text todo.label ]


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
