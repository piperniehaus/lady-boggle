module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Set exposing (Set)
import Random exposing (Generator)
import Components.Types exposing (..)
import Components.BoardGenerator exposing (..)
import Http exposing (Error)
import Json.Decode as Json
import Dict exposing (Dict)


-- APP


main : Program Never Model Msg
main =
    Html.program
        { init = ( model, Cmd.none )
        , subscriptions = (\_ -> Sub.none)
        , view = view
        , update = update
        }



-- MODEL


model : Model
model =
    { board = initBoard, score = 0, currentGuess = "" }


lookupWord : String -> Cmd Msg
lookupWord word =
    Http.send lookupWordResult <| Http.get ("http://api.wordnik.com:80/v4/word.json/" ++ word ++ "?useCanonical=false&includeSuggestions=false&api_key=6c121082f765eadec23490399930023d7b0bd9fab7bba1069") <| lookupWordDecoder


lookupWordResult : Result x a -> Msg
lookupWordResult result =
    Result.withDefault WordNotFound <| Result.map (\_ -> WordFound) (Debug.log "aa" (result))


lookupWordDecoder : Json.Decoder Bool
lookupWordDecoder =
    Json.field "scrabble" Json.bool



-- Json.dict Json.float
-- get : String -> Decode.Decoder a -> Request a
-- UPDATE


type Msg
    = NoOp
    | LookupWord String
    | UpdateGuess String
    | NewGame Board
    | Shuffle
    | WordFound
    | WordNotFound
    | AddLetter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LookupWord word ->
            ( model, lookupWord word )

        WordFound ->
            ( { model | score = model.score + 1 }, Cmd.none )

        WordNotFound ->
            ( model, Cmd.none )

        UpdateGuess guess ->
            ( updateGuess guess model
            , Cmd.none
            )

        NewGame board ->
            ( { model | board = board, currentGuess = "" }, Cmd.none )

        Shuffle ->
            ( { model | currentGuess = "" }, Random.generate NewGame boardGen )

        AddLetter letter ->
            ( updateGuess (model.currentGuess ++ letter) model
            , Cmd.none
            )


updateGuess : String -> Model -> Model
updateGuess guess model =
    let
        newDict =
            Debug.log "UpdateGuess dict" <| setMatches model.board guess
    in
        { model
            | currentGuess = guess
            , board = newDict
        }


getNeighbors : Board -> Point -> List Point
getNeighbors board ( x, y ) =
    let
        realPointsOnly point =
            List.member point <| Dict.keys board
    in
        List.filter realPointsOnly <|
            Set.toList <|
                Set.remove ( x, y ) <|
                    Set.fromList
                        [ ( x, y - 1 )
                        , ( x, y + 1 )
                        , ( x - 1, y )
                        , ( x + 1, y )
                        , ( x - 1, y - 1 )
                        , ( x - 1, y + 1 )
                        , ( x + 1, y + 1 )
                        , ( x + 1, y - 1 )
                        ]


checkTile : String -> Point -> Tile -> Tile
checkTile guessLetter location tile =
    { tile | match = (guessLetter == tile.letter) }


type Step
    = Step { point : Point, nextSteps : List Step }


getAllPaths : Board -> String -> List Step
getAllPaths board word =
    let
        makeStep : String -> Point -> Maybe Step
        makeStep currentWord point =
            if checkTileAtLocation (String.left 1 currentWord) point then
                Just (Step { point = point, nextSteps = List.filterMap (makeStep (nextWord currentWord)) (getNeighbors board point) })
            else
                Nothing

        tileIsMatch : String -> Tile -> Bool
        tileIsMatch letter tile =
            letter == tile.letter

        nextWord : String -> String
        nextWord currentWord =
            String.dropLeft 1 currentWord

        checkTileAtLocation : String -> Point -> Bool
        checkTileAtLocation currentWord point =
            case Dict.get point board of
                Just tile ->
                    tileIsMatch currentWord tile

                Nothing ->
                    False
    in
        List.filterMap (makeStep word) <| Dict.keys board


getFlatPaths : String -> List Step -> List (List Point)
getFlatPaths string steps =
    let
        getAllPoints : List (List Point) -> Step -> List (List Point)
        getAllPoints pathSoFar step =
            let
                nextPathSoFar : Point -> List (List Point)
                nextPathSoFar point =
                    List.map (List.append [ point ]) pathSoFar

                -- For each step, append to the previous steps.
            in
                case step of
                    Step info ->
                        if List.length info.nextSteps > 0 then
                            List.concatMap (getAllPoints (nextPathSoFar info.point)) info.nextSteps
                        else
                            nextPathSoFar info.point

        -- List.append [ info.point ] ((List.concatMap getAllPoints) info.nextSteps)
        completeMatchesOnly : List Point -> Bool
        completeMatchesOnly pointsList =
            -- True
            (String.length string) == (Set.size (Set.fromList pointsList))
    in
        List.filter completeMatchesOnly <| List.concatMap (getAllPoints [ [] ]) steps


getHighlightedPoints : List (List Point) -> Set Point
getHighlightedPoints paths =
    Set.fromList (List.concat paths)


setMatches : Board -> String -> Board
setMatches board string =
    let
        highlightedPoints : Set Point
        highlightedPoints =
            getHighlightedPoints <| getFlatPaths string (getAllPaths board string)

        updateMatch : Point -> Tile -> Tile
        updateMatch point tile =
            { tile | match = (isMatch point) }

        isMatch : Point -> Bool
        isMatch point =
            Set.member point highlightedPoints
    in
        -- Set.map highlightedPoints
        if (Set.size highlightedPoints > 0) || (String.length string == 0) then
            Dict.map updateMatch board
        else
            board



-- VIEW


view : Model -> Html Msg
view model =
    let
        makeRow row =
            div [] (List.map makeTile row)

        makeTile tile =
            span
                [ classList
                    [ ( "letter", True )
                    , ( "letter--highlighted"
                      , tile.match
                      )
                    ]
                , onClick (AddLetter tile.letter)
                ]
                [ text tile.letter ]

        makeTemp point =
            span [ classList [ ( "letter", True ) ] ] [ text (toString point) ]
    in
        div []
            [ h2 [] [ text <| toString model.score ]
            , div [ class "boardContainer" ] (List.map makeTile <| Dict.values model.board)
              -- , div [ class "boardContainer" ] (List.map makeTemp <| Dict.keys model.board)
            , div []
                [ input [ placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                , button [ onClick (LookupWord model.currentGuess) ] [ text "Check" ]
                ]
            , button [ onClick Shuffle ] [ text "Shuffle" ]
            , div [] [ text <| toString (getAllPaths model.board model.currentGuess) ]
            , div [] [ text <| toString (getFlatPaths model.currentGuess (getAllPaths model.board model.currentGuess)) ]
            ]
