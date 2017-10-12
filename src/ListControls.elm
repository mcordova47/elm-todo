module ListControls exposing (checkbox, delete, deleteAll)

import Html exposing (Html)
import Html.Attributes exposing (class, title)
import Html.Events exposing (onClick)


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
            [ class "icon-btn"
            , onClick msg
            ]
            [ materialIcon kind ]


delete : msg -> Html msg
delete msg =
    Html.div
        [ class "icon-btn"
        , onClick msg
        , title "Clear Completed"
        ]
        [ materialIcon "delete" ]


deleteAll : msg -> Html msg
deleteAll msg =
    Html.div
        [ class "icon-btn"
        , onClick msg
        , title "Clear All"
        ]
        [ materialIcon "delete_sweep" ]


materialIcon : String -> Html msg
materialIcon kind =
    Html.i
        [ class "material-icons" ]
        [ Html.text kind ]
