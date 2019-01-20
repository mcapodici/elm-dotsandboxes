module NimStringTests exposing (calculateNimberTest, capturableCoinsTest, checkTakeTest, loonyCoinsForGraphTest, mexTest, removeCapturableCoinsReductionTest)

import Expect
import Extensions.String exposing (regexNewLine, regexSplit, removeEmptyStrings)
import Game exposing (..)
import List.Extra
import NimString exposing (..)
import StringsAndCoins exposing (toStringsAndCoinsGraph)
import Test exposing (Test, describe, test)
import TestCommon exposing (describeGraphNodesAndEdges, gameParser, initGame2By2, initSac2By2)


calculateNimberTest : Test
calculateNimberTest =
    describe "calculateNimber"
        (calculateNimberTestScenarios
            |> List.map
                (\( description, expectedNimber, game ) ->
                    test description <|
                        \_ ->
                            let
                                dbg =
                                    Debug.log "Running test" description
                            in
                            Expect.equal (Just expectedNimber) (calculateNimber <| toStringsAndCoinsGraph <| game)
                )
        )


mexTest : Test
mexTest =
    describe "mex"
        [ test "mex of nothing is 0" <|
            \_ -> Expect.equal (Nimber 0) (mex [])
        , test "mex of loony is 0" <|
            \_ -> Expect.equal (Nimber 0) (mex [ Loony ])
        , test "mex of 1 is 0" <|
            \_ -> Expect.equal (Nimber 0) (mex [ Nimber 1 ])
        , test "mex of 0 is 1" <|
            \_ -> Expect.equal (Nimber 1) (mex [ Nimber 0 ])
        , test "mex of 1,0 is 2" <|
            \_ -> Expect.equal (Nimber 2) (mex [ Nimber 1, Nimber 0 ])
        , test "mex of a big bunch" <|
            \_ -> Expect.equal (Nimber 2) (mex [ Loony, Nimber 1, Nimber 0, Nimber 1, Nimber 4, Nimber 3, Loony ])
        ]


removeCapturableCoinsReductionTest : Test
removeCapturableCoinsReductionTest =
    describe
        "removeCapturableCoinsReduction"
        [ test "scenario 1" <|
            \_ ->
                Expect.equal "Graph [Node 0, Node 1, Node 3, Node 4] [Edge 1->0 (2)]"
                    (describeGraphNodesAndEdges <|
                        removeCapturableCoinsReduction
                            (toStringsAndCoinsGraph <| gameParser """
O O-O
    |
O-O-O
| | |
O-O-O
                        """)
                    )
        ]


checkTakeTest : Test
checkTakeTest =
    describe
        "checkTake"
        [ test "scenario 1" <|
            \_ ->
                Expect.equal CanTakeLoony
                    (checkTake 1
                        (toStringsAndCoinsGraph <| gameParser """
O-O-O
| | |
O O O
|   |
O-O-O
                        """)
                    )
        ]


loonyCoinsForGraphTest : Test
loonyCoinsForGraphTest =
    describe
        "loonyCoinsForGraph"
        [ test "scenario 1" <|
            \_ ->
                Expect.equal [ 1, 2 ]
                    (loonyCoinsForGraph
                        (toStringsAndCoinsGraph <| gameParser """
O-O-O
| | |
O O O
|   |
O-O-O
                        """)
                    )
        ]


capturableCoinsTest : Test
capturableCoinsTest =
    describe
        "capturableCoins"
        [ test "returns none for new game" <|
            \_ -> Expect.equal [] (capturableCoinsForGraph initSac2By2)
        , test "returns none for loop" <|
            \_ ->
                Expect.equal []
                    (capturableCoinsForGraph
                        (toStringsAndCoinsGraph
                            (initGame2By2
                                |> Game.playMany
                                    [ Line Horizontal 1 1
                                    , Line Horizontal 2 1
                                    , Line Horizontal 1 3
                                    , Line Horizontal 2 3
                                    , Line Vertical 1 1
                                    , Line Vertical 1 2
                                    , Line Vertical 3 1
                                    , Line Vertical 3 2
                                    ]
                            )
                        )
                    )
        , test "returns none for chain" <|
            \_ ->
                Expect.equal []
                    (capturableCoinsForGraph
                        (toStringsAndCoinsGraph
                            (initGame2By2
                                |> Game.playMany
                                    [ Line Vertical 2 1
                                    , Line Horizontal 1 3
                                    , Line Horizontal 2 3
                                    , Line Vertical 1 1
                                    , Line Vertical 1 2
                                    , Line Vertical 3 1
                                    , Line Vertical 3 2
                                    ]
                            )
                        )
                    )
        , test "finds capturable coin via edge move" <|
            \_ ->
                Expect.equal [ 1 ]
                    (capturableCoinsForGraph
                        (toStringsAndCoinsGraph
                            (initGame2By2
                                |> Game.playMany
                                    [ Line Horizontal 1 1
                                    , Line Horizontal 1 2
                                    , Line Vertical 2 1
                                    ]
                            )
                        )
                    )
        , test "finds capturable coins via double cross" <|
            \_ ->
                Expect.equal [ 1, 2 ]
                    (capturableCoinsForGraph
                        (toStringsAndCoinsGraph
                            (initGame2By2
                                |> Game.playMany
                                    [ Line Horizontal 1 1
                                    , Line Horizontal 2 1
                                    , Line Horizontal 1 2
                                    , Line Horizontal 2 2
                                    , Line Vertical 1 1
                                    , Line Vertical 3 1
                                    ]
                            )
                        )
                    )
        ]


calculateNimberTestScenarios : List ( String, Nimber, Game )
calculateNimberTestScenarios =
    regexSplit "===+" calculateNimberTestScenariosString
        |> removeEmptyStrings
        |> List.map
            (\scenarioString ->
                case regexSplit regexNewLine scenarioString |> removeEmptyStrings of
                    description :: nimberAsString :: gameStringLines ->
                        let
                            nimber =
                                String.toInt nimberAsString |> Maybe.map Nimber |> Maybe.withDefault Loony
                        in
                        ( description, nimber, gameParser (String.join "\n" gameStringLines) )

                    _ ->
                        -- BS scenario designed to fail
                        ( "failed to parse this game", Loony, gameParser "" )
            )


calculateNimberTestScenariosString =
    """
No moves
0
O-O-O
| | |
O-O-O
| | |
O-O-O
====================
Last move
0
O O-O
| | |
O-O-O
| | |
O-O-O
====================
Figure 24 (1)
1
O O-O
  | |
O-O-O
| | |
O-O-O
====================
Figure 24 (2)
1
O O-O
|   
O-O-O
| | |
O-O-O
====================
Figure 24 (3)
0
O O-O
   
O-O-O
| | |
O-O-O
====================
Figure 24 (4)
1
O O O
   
O-O-O
| | |
O-O-O
====================
Figure 24 (5)
0
O O-O
| | |
O O-O
|   
O-O-O
====================
Figure 24 (6)
2
O O-O
| | |
O O-O
|   
O-O O
====================
Figure 24 (7)
2
O O-O
| | |
O O-O
|   
O O-O
====================
Figure 24 (8)
1
O O-O
  | |
O O-O
|   
O-O O
====================
Figure 24 (9)
1
O O-O
| | |
O O-O
|   
O O O
====================
Figure 24 (10)
1
O O-O
| | |
O O-O
    
O O-O
====================
Figure 24 (11)
0
O O-O
| | |
O O-O
    
O O O
====================
Figure 24 (12)
0
O O-O
  | |
O O-O
|   
O O O
====================
Figure 24 (13)
0
O O O
| | |
O O O
|   |
O-O-O
====================
Figure 24 (14)
1
O O O
  | |
O O O
|   |
O-O-O
====================
Figure 24 (15)
2
O O O
| | |
O O O
    |
O-O-O
====================
Figure 24 (16)
2
O O O
  | 
O O O
|   |
O-O-O
====================
Figure 24 (17)
3
O O O
  | |
O O O
    |
O-O-O
====================
Figure 24 (18)
3
O O O
  | |
O O O
|    
O-O-O
====================
Figure 24 (19)
1
O O O
| | |
O O O
    |
O O-O
====================
Figure 24 (20)
1
O O O
| | |
O O O
     
O-O-O
====================
Figure 24 (21)
0
O-O-O
|   |
O O O
|   |
O-O-O
====================
Figure 24 (22)
1
O O-O
|   |
O O O
|   |
O-O-O
====================
Figure 24 (23)
0
O O-O
    |
O O O
|   |
O-O-O
====================
Figure 24 (24)
2
O O-O
|   
O O O
|   |
O-O-O
====================
Figure 24 (25)
2
O O-O
|   |
O O O
|   
O-O-O
====================
Figure 24 (26)
3
O O O
    |
O O O
|   |
O-O-O
====================
Figure 24 (27)
1
O O-O
    |
O O O
|    
O-O-O
====================
Figure 24 (28)
0
O O-O
|   |
O O O
     
O-O-O
====================
Figure 25
7
O-O-O O O O
|       |
O O-O-O-O O
|
O-O-O-O-O-O
      |
O O-O O O O
      |
O o o o o o
"""
