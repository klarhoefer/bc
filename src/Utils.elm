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
