module TestCommon exposing (describeGraphNodesAndEdges, gameParser, initGame2By2, initSac2By2)

import Extensions.String exposing (..)
import Game exposing (..)
import Graph
import List.Extra exposing (..)
import StringsAndCoins exposing (..)


initGame2By2 : Game
initGame2By2 =
    Game.initGame { width = 2, height = 2 }


initSac2By2 : StringsAndCoinsGraph
initSac2By2 =
    toStringsAndCoinsGraph initGame2By2


describeGraphNodesAndEdges : StringsAndCoinsGraph -> String
describeGraphNodesAndEdges gr =
    Graph.toString (always Nothing) (\e -> Just (String.fromInt (List.length e))) gr


{-| Game parser takes the width and height of a game in squares, and
a string representation of a game, and returns the game.

Example input string for 2 by 2 game:

    O-O-O
    |   |
    O O O

    O O O

-}
gameParser : String -> Game
gameParser data =
    let
        splitData =
            regexSplit regexNewLine data |> removeEmptyStrings

        size =
            { width = ((List.head splitData |> Maybe.withDefault "?" |> String.trim |> String.length) - 1) // 2, height = (List.length splitData - 1) // 2 }

        game =
            initGame size

        moveIsPlayed : Line -> Bool
        moveIsPlayed (Line way x y) =
            let
                columnIndex =
                    (x - 1)
                        * 2
                        + (if way == Horizontal then
                            1

                           else
                            0
                          )

                rowIndex =
                    (y - 1)
                        * 2
                        + (if way == Horizontal then
                            0

                           else
                            1
                          )

                rowData =
                    List.Extra.getAt rowIndex splitData |> Maybe.withDefault ""

                character =
                    String.slice columnIndex (columnIndex + 1) rowData
            in
            character == "|" || character == "-"
    in
    unPlayedMoves game
        |> List.foldl
            (\move g ->
                if moveIsPlayed move then
                    addMove move g

                else
                    g
            )
            game
