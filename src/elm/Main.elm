module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)
import Random exposing (Generator)
import Task


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


type alias Model =
    { board : Board, score : Int, currentGuess : String }


type alias Tile =
    { letter : String, match : Bool }


type alias Point =
    ( Int, Int )


model : Model
model =
    { board = initBoard, score = 0, currentGuess = "" }


initBoard : Board
initBoard =
    let
        ( board, seed ) =
            Random.step boardGen <| Random.initialSeed 123
    in
        board



-- UPDATE


type Msg
    = NoOp
    | ScoreWord
    | UpdateGuess String
    | NewGame Board
    | Shuffle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ScoreWord ->
            ( { model
                | score = model.score + (String.length model.currentGuess)
                , currentGuess = ""
              }
            , Cmd.none
            )

        UpdateGuess guess ->
            let
                newDict =
                    Debug.log "UpdateGuess dict" <| setMatches model.board guess
            in
                ( { model
                    | currentGuess = guess
                    , board = newDict
                  }
                , Cmd.none
                )

        NewGame board ->
            ( { model | board = board, currentGuess = "" }, Cmd.none )

        Shuffle ->
            ( { model | currentGuess = "" }, Random.generate NewGame boardGen )


boardGen : Generator Board
boardGen =
    let
        tileDistribution : List ( String, Int )
        tileDistribution =
            [ ( "e", 12 )
            , ( "a", 9 )
            , ( "i", 9 )
            , ( "o", 8 )
            , ( "n", 6 )
            , ( "r", 6 )
            , ( "t", 6 )
            , ( "l", 6 )
            , ( "s", 6 )
            , ( "u", 6 )
            , ( "d", 4 )
            , ( "g", 3 )
            , ( "b", 2 )
            , ( "c", 2 )
            , ( "m", 2 )
            , ( "p", 2 )
            , ( "f", 2 )
            , ( "h", 2 )
            , ( "v", 2 )
            , ( "w", 2 )
            , ( "y", 2 )
            , ( "k", 1 )
            , ( "j", 1 )
            , ( "q", 1 )
            , ( "z", 1 )
            ]

        intList : Generator (List Int)
        intList =
            Random.list 36 (Random.int 0 (List.length allLettersList))

        allLettersList =
            let
                makeRepeatedList ( letter, number ) =
                    List.repeat number letter
            in
                List.concat <| List.map makeRepeatedList tileDistribution

        getIndex list number =
            Maybe.withDefault "a" (Array.get number (Array.fromList list))

        lettersList : List Int -> List String
        lettersList intList =
            List.map (getIndex allLettersList) intList
    in
        Random.map (getBoard << lettersList) intList


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


type alias Board =
    Dict Point Tile


getBoard : List String -> Board
getBoard board =
    let
        tileForLetter : String -> Tile
        tileForLetter letter =
            { letter = letter, match = False }

        encodeTile : Int -> Int -> String -> ( Point, Tile )
        encodeTile y x letter =
            ( ( x, y ), (tileForLetter letter) )

        encodeRows : Int -> String -> ( Point, Tile )
        encodeRows index letter =
            encodeTile (index % 6) (index // 6) letter
    in
        Dict.fromList <| (List.indexedMap encodeRows board)


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
        Dict.map updateMatch board



-- VIEW


view : Model -> Html Msg
view model =
    let
        makeRow row =
            div [] (List.map makeTile row)

        makeTile tile =
            span [ classList [ ( "letter", True ), ( "letter--highlighted", tile.match ) ] ] [ text tile.letter ]

        makeTemp point =
            span [ classList [ ( "letter", True ) ] ] [ text (toString point) ]
    in
        div []
            [ h2 [] [ text <| toString model.score ]
            , div [ class "boardContainer" ] (List.map makeTile <| Dict.values model.board)
              -- , div [ class "boardContainer" ] (List.map makeTemp <| Dict.keys model.board)
            , div []
                [ input [ placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                , button [ onClick ScoreWord ] [ text "Check" ]
                ]
            , button [ onClick Shuffle ] [ text "Shuffle" ]
            , div [] [ text <| toString (getAllPaths model.board model.currentGuess) ]
            , div [] [ text <| toString (getFlatPaths model.currentGuess (getAllPaths model.board model.currentGuess)) ]
            ]
