module Utils exposing (onlyIf, htmlIf, identityInsert, on)

import Dict exposing (Dict)
import Html exposing (Html)


onlyIf : Bool -> a -> Maybe a
onlyIf pred x =
    if pred then
        Just x
    else
        Nothing


htmlIf : Bool -> Html msg -> Html msg
htmlIf pred html =
    if pred then
        html
    else
        Html.text ""


identityInsert : a -> Dict Int a -> Dict Int a
identityInsert value dict =
    let
        maxId =
            List.maximum (Dict.keys dict)
                |> Maybe.withDefault 0

    in
        Dict.insert (maxId + 1) value dict


on : (a -> b) -> (b -> b -> c) -> a -> a -> c
on mapFn fn x y =
    fn (mapFn x) (mapFn y)
