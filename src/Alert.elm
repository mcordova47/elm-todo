module Alert exposing
    ( Model
    , Alert
    , Msg
    , empty
    , singleton
    , fromError
    , update
    , view
    , remove
    , add
    )

import Time
import Process
import Task
import Html exposing (Html)
import Html.Attributes exposing (classList)


-- MODEL


type alias Model =
    Maybe Alert


type alias Alert =
    { message : String }


empty : Model
empty =
    Nothing


singleton : String -> Model
singleton =
    Just << Alert


fromError : Result String a -> Model
fromError result =
    case result of
        Ok _ ->
            empty

        Err msg ->
            singleton msg



-- UPDATE


type Msg
    = AddAlert Alert
    | RemoveAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddAlert alert ->
            Just alert
                ! [ remove ]

        RemoveAlert ->
            Nothing ! []


add : Model -> Cmd Msg
add model =
    case model of
        Just alert ->
            Task.succeed alert
                |> Task.perform AddAlert

        Nothing ->
            Cmd.none


remove : Cmd Msg
remove =
    Process.sleep (3 * Time.second)
        |> Task.perform (\_ -> RemoveAlert)



-- VIEW


view : Model -> Html msg
view model =
    let
        message =
            model
                |> Maybe.map .message
                |> Maybe.withDefault ""
    in
        Html.div
            [ classList
                [ ( "alert-message", True )
                , ( "alert-message--hidden", message == "" )
                ]
            ]
            [ Html.text message ]
