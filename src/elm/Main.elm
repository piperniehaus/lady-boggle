module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)
import Debug exposing (..)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { board : BoardDict, score : Int, currentGuess : String }


type alias Board =
    List Row


type alias Tile =
    { letter : String, match : Bool }


type alias Point =
    ( Int, Int )


type alias Row =
    List Tile


type alias StartingPoints =
    List Point


type alias Path =
    List Point


type alias Paths =
    List Path


type alias BoardDict =
    Dict Point Tile


model : Model
model =
    { board = board, score = 0, currentGuess = "" }


boardWidth : Int
boardWidth =
    3


board : BoardDict
board =
    let
        letters =
            [ [ "a", "b", "a" ], [ "a", "d", "k" ], [ "p", "w", "z" ] ]

        tilesForRow : List String -> Row
        tilesForRow row =
            List.map tileForLetter row

        tileForLetter : String -> Tile
        tileForLetter letter =
            { letter = letter, match = False }
    in
        getBoardDict <| List.map tilesForRow letters



-- UPDATE


type Msg
    = NoOp
    | ScoreWord
    | UpdateGuess String


firstLetter : String -> String
firstLetter string =
    String.slice 0 1 string


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        ScoreWord ->
            { model
                | score = model.score + (String.length model.currentGuess)
                , currentGuess = ""
            }

        UpdateGuess guess ->
            let
                checkTile : String -> Tile -> Tile
                checkTile guessLetter tile =
                    { tile | match = (guessLetter == tile.letter) }

                matchFirstLetter : Point -> Tile -> Tile
                matchFirstLetter point tile =
                    checkTile (firstLetter guess) tile

                findPath : List Point
                findPath =
                    log "stuff"
                        (List.concatMap
                            (\point -> (log "path" (explorePath model.board [ point ] guess)))
                         <|
                            Dict.keys firstLetterMatches
                        )

                firstLetterMatches : BoardDict
                firstLetterMatches =
                    Dict.filter isMatching <|
                        Dict.map matchFirstLetter model.board

                isMatching : Point -> Tile -> Bool
                isMatching _ tile =
                    tile.match

                neighborList : List Point
                neighborList =
                    List.concatMap getNeighbors <|
                        Dict.keys <|
                            Dict.filter isMatching <|
                                Dict.map matchFirstLetter model.board

                findNeighbors : Point -> Tile -> Tile
                findNeighbors point tile =
                    if List.member point neighborList then
                        { tile | match = True }
                    else
                        tile

                findMatches : Point -> Tile -> Tile
                findMatches point tile =
                    if List.member point (log "fn path" findPath) then
                        { tile | match = True }
                    else
                        tile

                newDict =
                    Dict.map findMatches board
            in
                { model
                    | currentGuess = guess
                    , board = newDict
                }


explorePath : BoardDict -> Path -> String -> Path
explorePath board path word =
    let
        lastPoint : Path -> Maybe Point
        lastPoint path =
            Array.get ((List.length path) - 1) (Array.fromList path)

        shortenedWord word =
            String.dropLeft 1 word

        travel : BoardDict -> String -> Path -> Path
        travel board word path =
            List.append (log "matching neighbors" (matchingNeighbors board (lastPoint path) (firstLetter word))) path
    in
        if String.length word > 0 then
            log "path" (explorePath board (travel board word path) <| shortenedWord word)
        else
            path


matchingNeighbors : BoardDict -> Maybe Point -> String -> List Point
matchingNeighbors board point letter =
    let
        isMatch : Point -> Point -> Tile -> Bool
        isMatch origin point tile =
            (List.member point <| getNeighbors origin) && (tile.letter == letter)
    in
        case point of
            Just value ->
                Dict.keys <| Dict.filter (isMatch value) board

            Nothing ->
                []


getNeighbors : Point -> List Point
getNeighbors ( x, y ) =
    Set.toList <|
        Set.remove ( x, y ) <|
            Set.fromList
                [ ( max (x - 1) 0, max (y - 1) 0 )
                , ( max (x - 1) 0, y )
                , ( max (x - 1) 0, min (y + 1) boardWidth )
                , ( x, max (y - 1) 0 )
                , ( x, min (y + 1) boardWidth )
                , ( min (x + 1) boardWidth, max (y - 1) 0 )
                , ( min (x + 1) boardWidth, y )
                , ( min (x + 1) boardWidth, min (y + 1) boardWidth )
                ]


getBoardDict : Board -> BoardDict
getBoardDict board =
    let
        encodeTile : Int -> Int -> Tile -> ( Point, Tile )
        encodeTile x y tile =
            ( ( x, y ), tile )

        encodeRow : Int -> Row -> List ( Point, Tile )
        encodeRow index row =
            List.indexedMap (encodeTile index) row
    in
        Dict.fromList <| List.concatMap (\n -> n) (List.indexedMap encodeRow board)



-- VIEW


view : Model -> Html Msg
view model =
    let
        makeRow row =
            div [] (List.map makeTile row)

        makeTile tile =
            span [ classList [ ( "letter", True ), ( "letter--highlighted", tile.match ) ] ] [ text tile.letter ]
    in
        div []
            [ h2 [] [ text <| toString model.score ]
            , div [ class "boardContainer" ] (List.map makeTile <| Dict.values model.board)
            , div []
                [ input [ placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                , button [ onClick ScoreWord ] [ text "Check" ]
                ]
            , div [] [ text <| toString model.board ]
            ]
