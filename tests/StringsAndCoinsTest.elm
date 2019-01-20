module StringsAndCoinsTest exposing (mainTest)

import Array
import Expect exposing (Expectation)
import Extensions.Graph exposing (nodeLabelsForEdge)
import Game exposing (Game, Line(..), Square(..), Way(..))
import Graph exposing (Edge, Node)
import Maybe
import StringsAndCoins exposing (..)
import Test exposing (Test, describe, test)
import TestCommon exposing (describeGraphNodesAndEdges, initGame2By2, initSac2By2)


testForEach : String -> List a -> (a -> Expectation) -> Test
testForEach desc list runner =
    describe desc (List.map (\item -> test (Debug.toString item) (\_ -> runner item)) list)


mainTest : Test
mainTest =
    describe "The StringsAndCoins module"
        [ toStringsAndCoinsGraphTests
        , testSplitSacGraph
        ]


testSplitSacGraph : Test
testSplitSacGraph =
    let
        testBasedOnGame : Game -> String
        testBasedOnGame game =
            let
                graph : StringsAndCoinsGraph
                graph =
                    toStringsAndCoinsGraph game

                splitGraphs : List StringsAndCoinsGraph
                splitGraphs =
                    splitSacGraph graph
            in
            String.join "," (List.map describeGraphNodesAndEdges splitGraphs)
    in
    describe "splitSacGraph splits a graph"
        [ test "that is in one piece" <|
            \_ ->
                Expect.equal
                    [ initSac2By2 ]
                    (splitSacGraph initSac2By2)
        , test "that is in two pieces" <|
            \_ ->
                let
                    game : Game
                    game =
                        initGame2By2
                            |> Game.playMany
                                [ Line Vertical 2 1
                                , Line Vertical 2 2
                                ]
                in
                Expect.equal
                    (String.join ","
                        [ "Graph [Node 0, Node 1, Node 3] [Edge 3->1 (1), Edge 3->0 (2), Edge 1->3 (1), Edge 1->0 (2)]"
                        , "Graph [Node 0, Node 2, Node 4] [Edge 4->2 (1), Edge 4->0 (2), Edge 2->4 (1), Edge 2->0 (2)]"
                        ]
                    )
                    (testBasedOnGame game)
        , test "that is in some odd pieces" <|
            \_ ->
                let
                    game : Game
                    game =
                        initGame2By2
                            |> Game.playMany
                                [ Line Vertical 2 1
                                , Line Vertical 2 2
                                , Line Horizontal 2 1
                                , Line Horizontal 2 2
                                ]
                in
                Expect.equal
                    (String.join ","
                        [ "Graph [Node 0, Node 1, Node 3] [Edge 3->1 (1), Edge 3->0 (2), Edge 1->3 (1), Edge 1->0 (2)]"
                        , "Graph [Node 0, Node 2] [Edge 2->0 (1)]"
                        , "Graph [Node 0, Node 4] [Edge 4->0 (2)]"
                        ]
                    )
                    (testBasedOnGame game)
        , test "that is in four pieces" <|
            \_ ->
                let
                    game : Game
                    game =
                        initGame2By2
                            |> Game.playMany
                                [ Line Vertical 2 1
                                , Line Vertical 2 2
                                , Line Horizontal 1 2
                                , Line Horizontal 2 2
                                ]
                in
                Expect.equal
                    (String.join ","
                        [ "Graph [Node 0, Node 1] [Edge 1->0 (2)]"
                        , "Graph [Node 0, Node 2] [Edge 2->0 (2)]"
                        , "Graph [Node 0, Node 3] [Edge 3->0 (2)]"
                        , "Graph [Node 0, Node 4] [Edge 4->0 (2)]"
                        ]
                    )
                    (testBasedOnGame game)
        ]


toStringsAndCoinsGraphTests : Test
toStringsAndCoinsGraphTests =
    describe "toStringsAndCoinsGraph produces a graph"
        [ test "with the correct nodes and edges" <|
            \_ ->
                let
                    gr =
                        initSac2By2
                in
                Expect.equal
                    (String.join " "
                        [ "Graph"
                        , "[Node 0, Node 1, Node 2, Node 3, Node 4]"
                        , "[Edge 4->3 (1), Edge 4->2 (1), Edge 4->0 (2),"
                        , "Edge 3->4 (1), Edge 3->1 (1), Edge 3->0 (2),"
                        , "Edge 2->4 (1), Edge 2->1 (1), Edge 2->0 (2),"
                        , "Edge 1->3 (1), Edge 1->2 (1), Edge 1->0 (2)]"
                        ]
                    )
                    (describeGraphNodesAndEdges gr)
        , test "with the correct moves associated to it's edges" <|
            \_ ->
                let
                    gr =
                        initSac2By2

                    edges =
                        Graph.edges gr

                    testValue =
                        edges |> List.map (\e -> describeEdge e ++ "->" ++ wrapInParens (describeLines e.label)) |> String.join "; "
                in
                Expect.equal
                    (String.join " "
                        [ "(4,3)->(V 2 2); (4,2)->(H 2 2); (4,0)->(H 2 3, V 3 2);"
                        , "(3,4)->(V 2 2); (3,1)->(H 1 2); (3,0)->(V 1 2, H 1 3);"
                        , "(2,4)->(H 2 2); (2,1)->(V 2 1); (2,0)->(H 2 1, V 3 1);"
                        , "(1,3)->(H 1 2); (1,2)->(V 2 1); (1,0)->(V 1 1, H 1 1)"
                        ]
                    )
                    testValue
        , test "with the correct squares associated to it's nodes" <|
            \_ ->
                let
                    gr =
                        initSac2By2

                    nodes =
                        Graph.nodes gr

                    testValue =
                        nodes |> List.map (\n -> describeNode n ++ "->" ++ wrapInParens (describeSquareOrGround n.label)) |> String.join "; "
                in
                Expect.equal
                    "0->(G); 1->(S 1,1); 2->(S 2,1); 3->(S 1,2); 4->(S 2,2)"
                    testValue
        ]


describeSquareOrGround : SquareOrGround -> String
describeSquareOrGround sog =
    case sog of
        IsGround ->
            "G"

        IsSquare (Square sq) ->
            "S " ++ String.fromInt sq.x ++ "," ++ String.fromInt sq.y


describeNode : Node a -> String
describeNode n =
    String.fromInt n.id


describeEdge : Edge a -> String
describeEdge e =
    wrapInParens (String.fromInt e.from ++ "," ++ String.fromInt e.to)


wrapInParens : String -> String
wrapInParens s =
    "(" ++ s ++ ")"


describeLines : List Line -> String
describeLines lines =
    String.join ", " (List.map describeLine lines)


describeLine : Line -> String
describeLine (Line way x y) =
    let
        wayText =
            case way of
                Horizontal ->
                    "H"

                Vertical ->
                    "V"
    in
    wayText ++ " " ++ String.fromInt x ++ " " ++ String.fromInt y
