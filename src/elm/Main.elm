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


checkTile : String -> Point -> Tile -> Tile
checkTile guessLetter location tile =
    { tile | match = (guessLetter == tile.letter) }


type alias BoardDict =
    Dict Point Tile


getBoardDict : Board -> BoardDict
getBoardDict board =
    let
        encodeTile : Int -> Int -> Tile -> ( Point, Tile )
        encodeTile y x tile =
            ( ( x, y ), tile )

        encodeRow : Int -> Row -> List ( Point, Tile )
        encodeRow index row =
            List.indexedMap (encodeTile index) row
    in
        Dict.fromList <| List.concatMap (\n -> n) (List.indexedMap encodeRow board)


type Step
    = Step { point : Point, nextSteps : List Step }


getAllPaths : BoardDict -> String -> List Step
getAllPaths board word =
    let
        makeStep : String -> Point -> Maybe Step
        makeStep currentWord point =
            if checkTileAtLocation (String.left 1 currentWord) point then
                Just (Step { point = point, nextSteps = List.filterMap (makeStep (nextWord currentWord)) (getNeighbors point) })
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
        point : Step -> Point
        point step =
            case step of
                Step info ->
                    info.point

        getAllPoints : Step -> List Point
        getAllPoints step =
            case step of
                Step info ->
                    List.append [ info.point ] (List.concatMap getAllPoints info.nextSteps)

        completeMatchesOnly : List Point -> Bool
        completeMatchesOnly pointsList =
            (String.length string) == (List.length pointsList)
    in
        List.filter completeMatchesOnly <| List.map getAllPoints steps


getHighlightedPoints : List (List Point) -> Set Point
getHighlightedPoints paths =
    Set.fromList (List.concat paths)


setMatches : BoardDict -> String -> BoardDict
setMatches board string =
    let
        highlightedPoints : Set Point
        highlightedPoints =
            getHighlightedPoints <| getFlatPaths string (getAllPaths model.board string)

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
            , div [ class "boardContainer" ] (List.map makeTemp <| Dict.keys model.board)
            , div []
                [ input [ placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                , button [ onClick ScoreWord ] [ text "Check" ]
                ]
            , div [] [ text <| toString (getAllPaths model.board "aa") ]
            , div [] [ text <| toString (getFlatPaths "aa" (getAllPaths model.board "aa")) ]
            ]
