module Components.BoardGenerator exposing (initBoard, boardGen)

import Random exposing (Generator)
import Components.Types exposing (..)
import Array
import Dict


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


initBoard : Board
initBoard =
    let
        ( board, seed ) =
            Random.step boardGen <| Random.initialSeed 123
    in
        board


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
