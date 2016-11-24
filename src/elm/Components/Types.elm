module Components.Types exposing (..)

import Dict exposing (Dict)


type alias Model =
    { board : Board, score : Int, currentGuess : String }


type alias Tile =
    { letter : String, match : Bool }


type alias Point =
    ( Int, Int )


type alias Board =
    Dict Point Tile
