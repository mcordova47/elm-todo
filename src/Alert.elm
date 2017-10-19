module Alert exposing
    ( Model
    , Alert
    , Msg
    , init
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
    { current : Maybe Alert
    , pending : List Alert
    }


type alias Alert =
    { message : String }


init : Model
init =
    { current = Nothing
    , pending = []
    }


empty : Maybe Alert
empty =
    Nothing


singleton : String -> Maybe Alert
singleton =
    Just << Alert


fromError : Result String a -> Maybe Alert
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
            case model.current of
                Just _ ->
                    { model | pending = model.pending ++ [ alert ] }
                        ! []
                Nothing ->
                    { model | current = Just alert }
                        ! [ remove ]

        RemoveAlert ->
            case model.pending of
                alert :: rest ->
                    Model (Just alert) rest ! [ remove ]

                [] ->
                    init ! []


add : Maybe Alert -> Cmd Msg
add maybeAlert =
    case maybeAlert of
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
            model.current
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
