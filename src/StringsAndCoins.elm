module StringsAndCoins exposing (SquareOrGround(..), StringsAndCoinsGraph, allSquares, connectedSquaresForMove, sacToString, splitSacGraph, squareToGraphNode, toStringsAndCoinsGraph)

import Extensions.Functions exposing (uncurry)
import Extensions.List exposing (cartesian, zipWithIndexWith)
import Game exposing (Game, Line, Square(..))
import Graph exposing (Edge, Graph, Node, NodeId)
import List.Extra exposing (groupWhile, zip)


type alias StringsAndCoinsGraph =
    Graph SquareOrGround (List Line)


sacToString : StringsAndCoinsGraph -> String
sacToString =
    Graph.toString (\n -> Nothing) (\e -> Nothing)


type SquareOrGround
    = IsSquare Square
    | IsGround


{-| Gets all of the squares of the game, in order left to right then top to bottom
so that the order is in line with squareToGraphNode
-}
allSquares : Game -> List SquareOrGround
allSquares g =
    IsGround
        :: (cartesian (List.range 1 g.size.height) (List.range 1 g.size.width)
                |> List.map (\( y, x ) -> IsSquare <| Game.square x y)
           )


{-| Converts a Dots & Boxes game into a graph for the strings and coins version of the same
game.
-}
toStringsAndCoinsGraph : Game -> StringsAndCoinsGraph
toStringsAndCoinsGraph game =
    let
        thisSquareToGraphNode : SquareOrGround -> NodeId
        thisSquareToGraphNode =
            squareToGraphNode game

        thisConnectedSquaresForMove : Line -> ( SquareOrGround, SquareOrGround )
        thisConnectedSquaresForMove =
            connectedSquaresForMove game

        moveToEdge : Line -> Edge Line
        moveToEdge move =
            thisConnectedSquaresForMove move
                |> (\( sq1, sq2 ) ->
                        { from = thisSquareToGraphNode sq1
                        , to = thisSquareToGraphNode sq2
                        , label = move
                        }
                   )

        -- Edges that only hold a single move, to be later grouped and combined.
        singleMoveEdges : List (Edge Line)
        singleMoveEdges =
            Game.unPlayedMoves game
                |> List.concatMap
                    (\m ->
                        let
                            edge =
                                moveToEdge m

                            otherEdges =
                                if edge.to == 0 then
                                    []

                                else
                                    [ reverseEdge edge ]
                        in
                        edge :: otherEdges
                    )

        squares : List SquareOrGround
        squares =
            allSquares game

        -- Create the nodes from the list of squares, labelling in the same order
        -- as the list
        nodes : List (Node SquareOrGround)
        nodes =
            zipWithIndexWith Node squares

        -- Construct multi move edges. The reason is that the graph library chosen
        -- doesn't allow duplicate edges, so we need to handle duplication in the
        -- label by storing multiple moves there.
        edges : List (Edge (List Line))
        edges =
            globalGrouper
                compareEdgesByWhatTheyConnect
                singleMoveEdges
                |> List.map (uncurry combineSingleMoveEdgesIntoMultiMoveEdges)
    in
    Graph.fromNodesAndEdges nodes edges


combineSingleMoveEdgesIntoMultiMoveEdges : Edge Line -> List (Edge Line) -> Edge (List Line)
combineSingleMoveEdgesIntoMultiMoveEdges conn conns =
    Edge conn.from conn.to (List.map (\c -> c.label) (conn :: conns))


{-| Compares two edges by their from and to nodes, ignoring the label
-}
compareEdgesByWhatTheyConnect : Edge a -> Edge a -> Order
compareEdgesByWhatTheyConnect edge1 edge2 =
    compare ( edge1.from, edge1.to ) ( edge2.from, edge2.to )


{-| This will group items in a list where the items to be grouped may not be
adjacent in the original list. It therefore works like the grouping you expect
in an SQL statement, where, for example grouping by first on:

[(1,true), (2, false), (1, true)]

Would create 2 groups, one for items whose first value is 1 and one for the item
whose first value is 2.

-}
globalGrouper : (a -> a -> Order) -> List a -> List ( a, List a )
globalGrouper f list =
    groupWhile (\a b -> f a b == EQ) (List.sortWith f list)


reverseEdge : Edge a -> Edge a
reverseEdge e =
    { from = e.to, to = e.from, label = e.label }


{-| Maps a square to the index of the node within the graph representation
-}
squareToGraphNode : Game -> SquareOrGround -> NodeId
squareToGraphNode g sog =
    case sog of
        IsGround ->
            0

        IsSquare (Square { x, y }) ->
            (y - 1) * g.size.width + x


{-| Represents the node in the graph for a line between a square and the
edge of the board. For example the non-square node for the move in the
dots and boxes game shown below next to the dollar symbol, where 0,1,2,3
are the regular nodes for squares:

o-o-o
|1|2|$
o-o-o
|3|4|
o-o-o

-}
groundNode : Square
groundNode =
    Game.square -1 -1


connectedSquaresForMove : Game -> Line -> ( SquareOrGround, SquareOrGround )
connectedSquaresForMove g m =
    let
        sfl =
            Game.squaresForLine g.size m
    in
    case sfl of
        -- Should be impossible!
        [] ->
            ( IsGround, IsGround )

        -- If there is 1 square for line, then the other 'node' is the ground
        [ s1 ] ->
            ( IsSquare s1, IsGround )

        s1 :: (s2 :: _) ->
            ( IsSquare s1, IsSquare s2 )


{-| Gets the squares connected to a given square in the game, given the
current state of the game (i.e. the moves already played)
-}
connectedSquares : Game -> Square -> List SquareOrGround
connectedSquares g (Square { x, y }) =
    let
        moves =
            Game.linesForSquare x y

        playableMoves =
            List.filter (Game.isPlayed g) moves

        unfilteredConnectedSquares =
            List.concatMap
                (connectedSquaresForMove g
                    >> (\( s1, s2 ) -> [ s1, s2 ])
                )
                playableMoves
    in
    List.filter (\s -> s /= IsSquare (Square { x = x, y = y })) unfilteredConnectedSquares


splitSacGraph : StringsAndCoinsGraph -> List StringsAndCoinsGraph
splitSacGraph g =
    case recurseStep g of
        Just ( subGraph, remainder ) ->
            subGraph :: splitSacGraph remainder

        Nothing ->
            []


recurseStep : StringsAndCoinsGraph -> Maybe ( StringsAndCoinsGraph, StringsAndCoinsGraph )
recurseStep g =
    let
        chooseExploreNode : Maybe Int
        chooseExploreNode =
            nonGroundNodesForGraph g |> List.map (\n -> n.id) |> List.head
    in
    case chooseExploreNode of
        Nothing ->
            Nothing

        Just nodeId ->
            let
                nodes : List Int
                nodes =
                    Tuple.first <|
                        Graph.guidedDfs
                            Graph.alongOutgoingEdges
                            (Graph.onDiscovery (\ctx acc -> ctx.node.id :: acc))
                            [ nodeId ]
                            []
                            g

                subGraph =
                    Graph.inducedSubgraph nodes g

                remainingGraph =
                    List.foldl Graph.remove g (List.filter (\n -> n /= 0) nodes)
            in
            Just ( subGraph, remainingGraph )


graphHasNoSquares : StringsAndCoinsGraph -> Bool
graphHasNoSquares g =
    List.length (nonGroundNodesForGraph g) == 0


nonGroundNodesForGraph : StringsAndCoinsGraph -> List (Node SquareOrGround)
nonGroundNodesForGraph g =
    onlyNonGroundNodes (Graph.nodes g)


onlyNonGroundNodes : List (Node SquareOrGround) -> List (Node SquareOrGround)
onlyNonGroundNodes =
    List.filter (\n -> n.label /= IsGround)


