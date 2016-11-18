module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onClick, onInput)


-- APP


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { board : Board, score : Int, currentGuess : String }


type alias Board =
    List (List String)


model : Model
model =
    { board = board, score = 0, currentGuess = "" }


board : List (List String)
board =
    [ [ "a", "b" ], [ "c", "d" ] ]



-- UPDATE


type Msg
    = NoOp
    | CheckWord
    | UpdateGuess String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        CheckWord ->
            { model
                | score = model.score + (String.length model.currentGuess)
                , currentGuess = ""
            }

        UpdateGuess guess ->
            { model | currentGuess = guess }



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    let
        makeRow row =
            div [] (List.map makeLetter row)

        makeLetter letter =
            span [] [ text letter ]
    in
        div []
            [ h2 [] [ text <| toString model.score ]
            , div [] (List.map makeRow model.board)
            , div []
                [ input [ placeholder "Guess away!", onInput UpdateGuess, value model.currentGuess ] []
                , button [ onClick CheckWord ] [ text "Check" ]
                ]
            ]
