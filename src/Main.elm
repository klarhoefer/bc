module Main exposing (main)

import Utils exposing (nthOrFirst)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D
import String


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Track =
    { release: Float
    , track: Int
    , alt: Int
    , seconds: Maybe Int
    , name: String
    , title: String
    , artist: String
    }


type alias Model =
    { tracks: List Track
    , selected: List Track
    , opened: Maybe Int
    , seconds: Maybe Int
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( Model [] [] Nothing Nothing
    , Http.get
        { url = "bc.json"
        , expect = Http.expectJson GotData tracksDecoder
        }
    )


type Msg
    = GotData (Result Http.Error (List Track))
    | LiClicked Track

trackDecoder : D.Decoder Track
trackDecoder =
    D.map7 Track
        (D.field "release" D.float)
        (D.field "track" D.int)
        (D.field "alt" D.int)
        (D.maybe (D.field "seconds" D.int))
        (D.field "name" D.string)
        (D.field "title" D.string)
        (D.field "artist" D.string)

tracksDecoder : D.Decoder (List Track)
tracksDecoder = D.list trackDecoder

sel : Track -> Track -> Track
sel a b =
    if a.release > b.release then a else b

findLatest : List Track -> Int -> Track
findLatest tracks n =
    let
        tmp = List.filter (\t -> t.track == n) tracks
    in
        List.foldl sel (Track 1 n 0 Nothing "" "" "") tmp


selectedTracks : List Track -> List Track
selectedTracks tracks =
    List.map (findLatest tracks) (List.range 1 10)

replace : List Track -> Track -> List Track
replace tracks track =
    List.map (\t -> if t.track == track.track then track else t) tracks

maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd a b =
    Maybe.map2 (+) a b

totalSeconds : List Track -> Maybe Int
totalSeconds tracks =
    List.foldl maybeAdd (Just 0) (List.map (\t -> t.seconds) tracks)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok tracks ->
                    let
                        selected = selectedTracks tracks
                        seconds = totalSeconds selected 
                    in
                        ({model | tracks = tracks, selected = selected, seconds = seconds}, Cmd.none)
                Err _ -> (model, Cmd.none)
        
        LiClicked track ->
            case model.opened of
                Just n ->
                    let
                        opened = if n == track.track then Nothing else Just track.track
                        selected = replace model.selected track
                        seconds = totalSeconds selected 
                    in
                        ({model | opened = opened, selected = selected, seconds = seconds}, Cmd.none)
                Nothing -> ({model | opened = Just track.track}, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ ol [] (List.map (makeLi model) (List.range 1 10))
        , div [class "total"] [text ("Total: " ++ (secsToString model.seconds))]
        ]

makeLi : Model -> Int -> Html Msg
makeLi model track = li []
    [ makeSelect model track ]


sortTracks : Track -> Track -> Order
sortTracks a b =
    case compare a.release b.release of
       LT -> GT
       GT -> LT
       EQ -> compare a.alt b.alt


makeSelect : Model -> Int -> Html Msg
makeSelect model id =
    let
        selectedTrack = nthOrFirst model.selected id (Track 0 0 0 Nothing "" "" "")
        filteredTracks = List.filter (\t -> t.track == id) model.tracks
        -- sortedTracks = List.sortBy .release filteredTracks
        sortedTracks = List.sortWith sortTracks filteredTracks
        isOpen = case model.opened of
           Just n -> n == id
           Nothing -> False
    in
        ul [ classList [("tracks", True), ("opened", isOpen)]]
            (if isOpen then
                (List.map makeOption sortedTracks)
            else
                [makeOption selectedTrack]
            )


secsToString : Maybe Int -> String
secsToString secs =
    case secs of
        Just seconds ->
            let
                rem = modBy 60 seconds
                minutes = (seconds - rem) // 60
            in
                (String.fromInt minutes) ++ ":" ++ (String.padLeft 2 '0' (String.fromInt rem))
        Nothing -> ""

makeOption : Track -> Html Msg
makeOption track =
    li [class "track", onClick (LiClicked track)]
        [ div [class "release"]
            [ div [] [text ("Release " ++ track.name)]
            , div [class "option"] (if track.alt /= 0 then [text ("Option " ++ (String.fromInt track.alt))] else [])
            , div [class "seconds"] [ text (secsToString track.seconds)]
            ]
        , div []
            [ div [class "title"] [text track.title]
            , div [class "artist"] [text track.artist]
            ]
        ]
