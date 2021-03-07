module Utils exposing (..)


nth : List item -> Int -> Maybe item
nth items n =
    case items of
        h :: l ->
            if n == 1 then Just h else nth l (n - 1)
        [] -> Nothing


nthOrFirst : List item -> Int -> item -> item
nthOrFirst items n default =
    case nth items n of
        Just item -> item
        Nothing -> default


secsToString : Maybe Int -> String
secsToString secs =
    case secs of
        Just seconds ->
            let
                rem = modBy 60 seconds
                minutes = (seconds - rem) // 60
            in
                (String.fromInt minutes) ++ ":" ++ (String.padLeft 2 '0' (String.fromInt rem))
        Nothing -> "???"
