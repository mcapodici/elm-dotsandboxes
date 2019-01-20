module GraphTest exposing (removeEdgeTest)

import Expect
import Extensions.Graph exposing (removeEdge)
import Graph
import Test exposing (Test, describe, test)
import TestCommon exposing (..)



-- outGoingNodesTest : Test
-- outGoingNodesTest =
--     describe "outGoingNodesTest"
--         [ test "" <|
--             \_ ->
--                 let
--                     graph =
--                         Graph.fromNodeLabelsAndEdgePairs [ 'a', 'b', 'c', 'd' ] [ ( 0, 1 ), ( 1, 0 ), ( 2, 0 ), ( 0, 3 ) ]
--                 in
--                 Expect.equal [ 1, 3 ] (outGoingNodes 0 graph)
--         ]


removeEdgeTest : Test
removeEdgeTest =
    describe "removeEdgeTest"
        [ test "" <|
            \_ ->
                let
                    graph =
                        Graph.fromNodeLabelsAndEdgePairs [ 'a', 'b', 'c', 'd' ] [ ( 0, 1 ), ( 1, 0 ), ( 2, 0 ), ( 0, 3 ) ]
                in
                Expect.equal
                    (Graph.fromNodeLabelsAndEdgePairs [ 'a', 'b', 'c', 'd' ] [ ( 1, 0 ), ( 2, 0 ), ( 0, 3 ) ])
                    (removeEdge 0 1 graph)
        ]
