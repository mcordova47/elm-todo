module Main exposing (..)

import Html exposing (Html)
import Html.Attributes


-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.img
            [ Html.Attributes.src "./img/elm.png"
            , Html.Attributes.style [ ( "border", "1px solid black" ) ]
            ]
            []
        , Html.text "Hello world"
        ]



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
