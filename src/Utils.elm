module Utils exposing (onlyIf, mapIf, mapKeys, mapKeysOrFail)
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


mapKeys :
    (comparable1 -> comparable)
    -> Dict comparable1 v -> Dict comparable v
mapKeys fn dict =
    dict
        |> Dict.toList
        |> List.map (Tuple.mapFirst fn)
        |> Dict.fromList


mapKeysOrFail :
    (comparable1 -> Result err comparable)
    -> Dict comparable1 v -> Result err (Dict comparable v)
mapKeysOrFail fn dict =
    dict
        |> Dict.toList
        |> List.map (mapFirstOrFail fn)
        |> joinResults
        |> Result.map Dict.fromList


mapFirstOrFail :
    (a -> Result err b)
    -> ( a, c )
    -> Result err ( b, c )
mapFirstOrFail fn ( left, right ) =
    case fn left of
        Ok val ->
            Ok ( val, right )
        Err msg ->
            Err msg


joinResults : List (Result err value) -> Result err (List value)
joinResults results =
    case results of
        [] ->
            Ok []
        result :: rest ->
            case result of
                Ok val ->
                    joinResults rest
                        |> Result.map ((::) val)
                Err msg ->
                    Err msg
