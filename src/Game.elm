module Game exposing (Game, Line(..), Size, Square(..), Way(..), Win(..), addMove, initGame, isFinished, isPlayed, lineToGameKey, linesForSquare, moves, play, playMany, player, score, square, squaresForLine, unPlayedMoves, winner, wins)

import Dict.Any as AD
import Extensions.List exposing (cartesian)
import List exposing (append, map, map2, range)
import Maybe
import String exposing (fromInt)
import Tuple exposing (first)


type Way
    = Horizontal
    | Vertical


{-| Describes a line (aka move) in a game of dots and boxes, with a direction (Way) and
the coordinates which are X amd Y based and 1 - based.
-}
type Line
    = Line Way Int Int


type Win
    = PlayerA
    | PlayerB


type alias Size =
    { width : Int, height : Int }


type Square
    = Square { x : Int, y : Int }


type alias Game =
    { moves : AD.AnyDict ( Int, Int, Int ) Line Bool
    , wins : AD.AnyDict ( Int, Int ) Square Win
    , size : Size
    , player : Win
    }


isPlayed : Game -> Line -> Bool
isPlayed game line =
    AD.get line game.moves |> Maybe.withDefault False


player : Game -> Win
player g =
    g.player


lineToGameKey : Line -> ( Int, Int, Int )
lineToGameKey (Line w x y) =
    ( case w of
        Horizontal ->
            0

        Vertical ->
            1
    , x
    , y
    )


initLines : Size -> List Line
initLines size =
    append
        (cartesian (range 1 size.width) (range 1 (size.height + 1)) |> map (\( x, y ) -> Line Horizontal x y))
        (cartesian (range 1 (size.width + 1)) (range 1 size.height) |> map (\( x, y ) -> Line Vertical x y))


initGame : Size -> Game
initGame size =
    { moves =
        AD.fromList lineToGameKey <|
            map (\l -> ( l, False )) (initLines size)
    , size = size
    , wins = AD.empty (\(Square { x, y }) -> ( x, y ))
    , player = PlayerA
    }


moves : Game -> List ( Line, Bool )
moves game =
    AD.toList game.moves


unPlayedMoves : Game -> List Line
unPlayedMoves game =
    moves game
        |> List.filter (\( m, played ) -> not played)
        |> List.map Tuple.first


wins : Game -> List ( Int, Int, Win )
wins game =
    map (\( Square { x, y }, p ) -> ( x, y, p )) (AD.toList game.wins)


playMany : List Line -> Game -> Game
playMany moveList game =
    List.foldl play game moveList


{-| Adds a move without updating the current player or score. Useful for testing -
-}
addMove : Line -> Game -> Game
addMove line game =
    let
        newMoves =
            AD.update line (Maybe.map (always True)) game.moves
    in
    { game | moves = newMoves }


play : Line -> Game -> Game
play line game =
    let
        newMoves =
            AD.update line (Maybe.map (always True)) game.moves

        additionalWins =
            squaresForLine game.size line
                |> List.filter
                    (\(Square { x, y }) -> List.all (\line2 -> AD.get line2 newMoves |> Maybe.withDefault False) (linesForSquare x y))

        newWins =
            List.foldl (\win dict -> AD.insert win game.player dict) game.wins additionalWins

        newPlayer =
            if List.isEmpty additionalWins then
                otherPlayer game.player

            else
                game.player

        otherPlayer p =
            case p of
                PlayerA ->
                    PlayerB

                _ ->
                    PlayerA
    in
    { game | moves = newMoves, wins = newWins, player = newPlayer }


linesForSquare : Int -> Int -> List Line
linesForSquare x y =
    [ Line Horizontal x y, Line Vertical x y, Line Horizontal x (y + 1), Line Vertical (x + 1) y ]


square : Int -> Int -> Square
square x y =
    Square { x = x, y = y }


squaresForLine : Size -> Line -> List Square
squaresForLine s l =
    let
        all =
            case l of
                Line Horizontal x y ->
                    [ square x y, square x (y - 1) ]

                Line Vertical x y ->
                    [ square x y, square (x - 1) y ]
    in
    List.filter (\(Square { x, y }) -> x >= 1 && y >= 1 && x <= s.width && y <= s.height) all


score : Game -> Win -> Int
score game p =
    game.wins |> AD.values |> List.filter (\w -> w == p) |> List.length


isFinished : Game -> Bool
isFinished game =
    game.moves |> AD.values |> List.all identity


winner : Game -> Maybe Win
winner game =
    if isFinished game then
        let
            playerAWins =
                score game PlayerA

            playerBWins =
                score game PlayerB
        in
        if playerAWins > playerBWins then
            Just PlayerA

        else if playerBWins > playerAWins then
            Just PlayerB

        else
            Nothing

    else
        Nothing
