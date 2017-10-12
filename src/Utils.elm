module Utils exposing (onlyIf, mapIf)


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
