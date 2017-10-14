module Utils exposing (onlyIf, mapIf, identityInsert)

import Dict exposing (Dict)


onlyIf : Bool -> a -> Maybe a
onlyIf pred x =
    if pred then
        Just x
    else
        Nothing


mapIf : (a -> Bool) -> (a -> a) -> List a -> List a
mapIf p fn xs =
    let mapFn x =
        if p x then
            fn x
        else
            x
    in
        List.map mapFn xs


identityInsert : a -> Dict Int a -> Dict Int a
identityInsert value dict =
    let
        maxId =
            List.maximum (Dict.keys dict)
                |> Maybe.withDefault 0

    in
        Dict.insert (maxId + 1) value dict
