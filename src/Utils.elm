module Utils exposing (onlyIf)


onlyIf : Bool -> a -> Maybe a
onlyIf pred x =
    if pred then
        Just x
    else
        Nothing
