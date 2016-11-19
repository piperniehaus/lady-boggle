module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Set exposing (Set)


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
                firstLetter : String -> String
                firstLetter string =
                    String.slice 0 1 string

                matchFirstLetter : Point -> Tile -> Tile
                matchFirstLetter point tile =
                    checkTile (firstLetter guess) point tile

                isMatching : Point -> Tile -> Bool
                isMatching _ tile =
                    tile.match

                neighborList : List Point
                neighborList =
                    List.concatMap getNeighbors <|
                        Dict.keys <|
                            Dict.filter isMatching <|
                                Dict.map matchFirstLetter model.board

                mappingThingy : Point -> Tile -> Tile
                mappingThingy point tile =
                    if List.member point neighborList then
                        { tile | match = True }
                    else
                        tile

                newDict =
                    Dict.map mappingThingy model.board
            in
                { model
                    | currentGuess = guess
                    , board = newDict
                }



-- starting points are the places where the first letter matches


updateGuess : String -> String
updateGuess guess =
    -- guess - the first letter
    guess


type alias StartingPoints =
    List Point


toPath : Point -> Path
toPath point =
    [ point ]


matchingNeighbors : BoardDict -> Point -> String -> List Point
matchingNeighbors board point letter =
    let
        matchList ( x, y ) =
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

        isMatch : Point -> Tile -> Bool
        isMatch point tile =
            (List.member point matchList) && tile.letter == letter
    in
        Dict.keys <| Dict.filter isMatch board


explorePath : BoardDict -> Path -> String -> Path
explorePath board path word =
    let
        lastPoint : path -> point
        lastPoint path =
            List.drop 1 path

        shortenedWord word =
            String.dropLeft 1 word

        travel : BoardDict -> String -> Path -> Path
        travel board path word =
            List.append (matchingNeighbors board (lastPoint path) word) path
    in
        if String.length word > 0 then
            explorePath board (travel board path word) <| shortenedWord word
        else
            path


findPaths : StartingPoints -> Paths
findPaths startingPoints =
    -- iterate through starting points
    List.map (\point -> explorePath (toPath point) "remainingGuess") startingPoints


type alias Path =
    List Point


type alias Paths =
    List Path



--
-- updateGuess(guess)
--  startingPoints = List(tuple)
--  startingPoints.each (
-- path = explorePoint([point], remainingWord))
-- if path then update dictionary with things in path
--explorePoint(list, word)
-- if word is nil return list
-- else if no neighbords return nil
-- getNeighbors(tail(list)).filter(neighbor.letter == firstLetter word, explorePoint(list+neighbor, remainingWord))


getNeighbors : Point -> Tile -> Bool
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


type alias Entry =
    ( Point, Tile )


checkTile : String -> Point -> Tile -> Tile
checkTile guessLetter location tile =
    { tile | match = (guessLetter == tile.letter) }


type alias BoardDict =
    Dict Point Tile


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
