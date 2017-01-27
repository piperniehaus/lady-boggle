module Main exposing (..)

import Platform exposing (Task)
import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)
import Dom exposing (focus, Error)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { board : BoardDict, score : Int, currentGuess : String, foundWords : List String, hasMatch : Bool, guessed : Bool, correct : Bool, doFocus : Task Dom.Error () }


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
    { board = board
    , score = 0
    , currentGuess = ""
    , foundWords = []
    , hasMatch = False
    , guessed = False
    , correct = False
    , doFocus = Dom.focus "guess-input"
    }


boardWidth : Int
boardWidth =
    5


board : BoardDict
board =
    let
        letters =
            [ [ "l", "r", "e", "o", "s" ]
            , [ "e", "d", "i", "w", "f" ]
            , [ "j", "e", "m", "w", "e" ]
            , [ "a", "f", "l", "t", "r" ]
            , [ "o", "s", "a", "h", "h" ]
            ]

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
                | score =
                    if model.hasMatch && not (List.member model.currentGuess model.foundWords) then
                        model.score + (String.length model.currentGuess)
                    else
                        model.score
                , foundWords =
                    if model.hasMatch && not (List.member model.currentGuess model.foundWords) then
                        model.currentGuess :: model.foundWords
                    else
                        model.foundWords
                , currentGuess = ""
                , guessed = True
                , correct = model.hasMatch
            }

        UpdateGuess guess ->
            let
                checkTile : String -> Tile -> Tile
                checkTile guessLetter tile =
                    { tile | match = (guessLetter == tile.letter) }

                matchFirstLetter : Point -> Tile -> Tile
                matchFirstLetter point tile =
                    checkTile (firstLetter guess) tile

                findPaths : List Path
                findPaths =
                    List.filter (\path -> List.length path == String.length guess) <|
                        List.concat <|
                            (List.map
                                (\point -> (explorePath model.board [ point ] (shortenedWord guess)))
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

                findMatches : Point -> Tile -> Tile
                findMatches point tile =
                    if List.member point (Maybe.withDefault [] (List.head findPaths)) then
                        { tile | match = True }
                    else
                        tile

                newDict =
                    Dict.map findMatches board
            in
                { model
                    | currentGuess = guess
                    , board = newDict
                    , hasMatch = not <| Dict.isEmpty (Dict.filter (\key tile -> tile.match == True) newDict)
                    , guessed = False
                }


shortenedWord : String -> String
shortenedWord word =
    String.dropLeft 1 word


explorePath : BoardDict -> Path -> String -> List Path
explorePath board path word =
    let
        lastPoint : Path -> Maybe Point
        lastPoint path =
            Array.get 0 (Array.fromList path)

        travel : BoardDict -> String -> Path -> List Path
        travel board word path =
            List.map
                (\match ->
                    if (not <| List.member match path) then
                        List.append [ match ] path
                    else
                        []
                )
                (matchingNeighbors board (lastPoint path) (firstLetter word))
    in
        if String.length word > 0 then
            List.concatMap (\aPath -> (explorePath board aPath <| shortenedWord word)) (travel board word path)
        else
            [ path ]


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

        makeFoundWord word =
            div [ class "foundWord" ] [ text word ]
    in
        div []
            [ div [ classList [ ( "game", True ), ( "guessed", model.guessed ), ( "correct", model.correct ) ] ]
                [ div []
                    [ h2 [] [ text <| "Score: " ++ toString model.score ]
                    , div [ class "boardContainer" ] (List.map makeTile <| Dict.values model.board)
                    , div []
                        [ input [ Html.Attributes.id "guess-input", placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                        , button [ onClick ScoreWord ] [ text "Check" ]
                        ]
                    ]
                , div [ class "foundWordContainer" ] <| [ h2 [] [ text "Found Words" ] ] ++ (List.map makeFoundWord model.foundWords)
                ]
            , div [] [ text <| toString model.board ]
            ]
