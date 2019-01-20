module Player exposing (makeGreedyMove, makeRandomMove)

import Array
import Dict
import Game exposing (Game, Line(..), Way(..))
import List
import Random
import Tuple


getPossibleMoves : Game -> Array.Array Line
getPossibleMoves g =
    Array.fromList (Game.unPlayedMoves g)


makeRandomMove : Random.Seed -> Game -> Maybe Line
makeRandomMove seed g =
    let
        possibleMoves =
            getPossibleMoves g
    in
    if Array.isEmpty possibleMoves then
        Nothing

    else
        pickRandomFromList seed possibleMoves


pickRandomFromList : Random.Seed -> Array.Array a -> Maybe a
pickRandomFromList seed list =
    let
        count =
            Array.length list

        chosenIndex =
            Tuple.first (Random.step (Random.int 0 (count - 1)) seed)
    in
    Array.get chosenIndex list


moveCausesWin : Game -> Line -> Bool
moveCausesWin game move =
    let
        oldScore =
            Game.score game game.player

        newScore =
            Game.score (Game.play move game) game.player
    in
    newScore > oldScore


moveCausesLose : Game -> Line -> Bool
moveCausesLose game move =
    if moveCausesWin game move then
        False

    else
        let
            newGame =
                Game.play move game

            newGameWinningMoves =
                getPossibleMoves newGame |> Array.filter (moveCausesWin newGame)
        in
        not (Array.isEmpty newGameWinningMoves)


makeGreedyMove : Random.Seed -> Game -> Maybe Line
makeGreedyMove seed g =
    let
        possibleMoves =
            getPossibleMoves g

        winningMoves =
            possibleMoves |> Array.filter (moveCausesWin g)

        nonLosingMoves =
            possibleMoves |> Array.filter (not << moveCausesLose g)
    in
    if Array.isEmpty winningMoves then
        if Array.isEmpty nonLosingMoves then
            makeRandomMove seed g

        else
            pickRandomFromList seed nonLosingMoves

    else
        pickRandomFromList seed winningMoves
