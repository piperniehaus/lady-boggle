module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value, classList)
import Html.Events exposing (onClick, onInput)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { board : Board, score : Int, currentGuess : String }


type alias Board =
    List Row


type alias Tile =
    { letter : String, match : Bool }


type alias Row =
    List Tile


model : Model
model =
    { board = board, score = 0, currentGuess = "" }


board : Board
board =
    let
        letters =
            [ [ "a", "b" ], [ "c", "d" ] ]

        tilesForRow : List String -> Row
        tilesForRow row =
            List.map tileForLetter row

        tileForLetter : String -> Tile
        tileForLetter letter =
            { letter = letter, match = False }
    in
        List.map tilesForRow letters



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
            { model
                | currentGuess = guess
                , board = checkWord guess model.board
            }


checkWord : String -> Board -> Board
checkWord guess board =
    let
        firstLetter : String -> String
        firstLetter string =
            String.slice 0 1 string

        checkRow : Row -> Row
        checkRow row =
            row

        -- checkForLetter : String -> Row -> Row
        -- checkForLetter remainingGuess row =
        --     List.map (checkTile (firstLetter remainingGuess))
        checkTile : String -> Tile -> Tile
        checkTile guessLetter tile =
            { tile | match = (guessLetter == tile.letter) }
    in
        List.map checkRow board



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
            , div [] (List.map makeRow model.board)
            , div []
                [ input [ placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                , button [ onClick ScoreWord ] [ text "Check" ]
                ]
            ]
