module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList, class)
import Html.Events exposing (onClick, onInput)
import Dict exposing (Dict)


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


board : BoardDict
board =
    let
        letters =
            [ [ "a", "b", "e" ], [ "c", "d", "k" ], [ "p", "w", "z" ] ]

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
                newDict =
                    Dict.map (checkTile <| firstLetter guess) model.board
            in
                { model
                    | currentGuess = guess
                    , board = newDict
                }


checkWord : String -> BoardDict -> BoardDict
checkWord guess board =
    board



-- let


firstLetter : String -> String
firstLetter string =
    String.slice 0 1 string



--
-- in
-- Dict.map (checkTile "a") (getBoardDict board)


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



-- getBoardFromDict : Board -> BoardDict
-- getBoardFromDict board =
--   let
--     buildRow
-- getRowDict rowIndex row =
-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


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
