module NimString exposing (Nimber(..), TakeableResult(..), applyReductionsUntilComplete, calculateNimber, capturableCoinsForGraph, checkTake, loonyCoinsForGraph, mex, removeCapturableCoinsReduction)

import Bitwise exposing (xor)
import Extensions.Graph exposing (removeEdge, removeMany)
import Extensions.List exposing (removeNothings)
import Game exposing (Line)
import Graph exposing (Edge, NodeContext, NodeId)
import IntDict
import List
import List.Extra exposing (unique, zip)
import Maybe.Extra exposing (combine)
import StringsAndCoins exposing (..)


type Nimber
    = Loony
    | Nimber Int


{-| Calculates the number of an arbitrary strings and coins graph. Returns
a Maybe, with Nothing being returned if the calculation was too hard too
complete.
-}
calculateNimber : StringsAndCoinsGraph -> Maybe Nimber
calculateNimber graph =
    let
        subGraphs =
            splitSacGraph graph

        subNimbers =
            List.map (calculateConnectedSubNimbers >> calculateConnectedNimber) subGraphs
    in
    List.foldl (Maybe.map2 xorNimber) (Just (Nimber 0)) subNimbers


calculateSubnimbers : StringsAndCoinsGraph -> Maybe CalculateConnectedSubNimbersResult
calculateSubnimbers graph =
    let
        subGraphs =
            splitSacGraph graph

        subGraphSubNimbers =
            List.map calculateConnectedSubNimbers subGraphs

        subGraphNimbers =
            List.map calculateConnectedNimber subGraphSubNimbers
    in
    Nothing



-- split graph
-- get sns for each subgraph
-- get n for each subgraph
-- etc,


{-| Xor function generalised to Nimbers, where if either or both of the inputs
are Loony the result is Loony, otherwise it is a normal xor
-}
xorNimber : Nimber -> Nimber -> Nimber
xorNimber a b =
    case ( a, b ) of
        ( Nimber x, Nimber y ) ->
            Nimber (xor x y)

        _ ->
            Loony


describeGraphNodesAndEdges : StringsAndCoinsGraph -> String
describeGraphNodesAndEdges gr =
    Graph.toString (always Nothing) (\e -> Just (String.fromInt (List.length e))) gr



-- TODO: I would like to make SAC graph edges an "at least one" type of list
-- and wrap SAC in a new type and for it to have it's own module with associated
-- operations and tests


playMoveOnSac : Edge (List Line) -> StringsAndCoinsGraph -> StringsAndCoinsGraph
playMoveOnSac edge graph =
    Graph.mapEdges
        (\edgeToInspect ->
            if edgeToInspect == edge.label then
                Debug.log "droped " <| List.drop 1 edgeToInspect

            else
                edgeToInspect
        )
        graph


type NimberWithEdge
    = NimberWithEdge Nimber (Edge Line)


type CalculateConnectedSubNimbersResult
    = CalculateConnectedSubNimbersResultLoony
    | CalculateConnectedSubNimbersResult (List Nimber)


calculateConnectedSubNimbers : StringsAndCoinsGraph -> Maybe CalculateConnectedSubNimbersResult
calculateConnectedSubNimbers graph =
    let
        reducedGraph =
            applyReductionsUntilComplete 100 graph
    in
    if not (List.isEmpty (loonyCoinsForGraph reducedGraph)) then
        Just CalculateConnectedSubNimbersResultLoony

    else
        -- Next step is to make every move, calculate the subgraph nimber (using calc unconnected)
        -- and then apply MEX
        let
            edges =
                Graph.edges reducedGraph |> List.filter (\e -> e.from > e.to)

            subNimbers : List (Maybe Nimber)
            subNimbers =
                List.concatMap
                    (\edge ->
                        let
                            numberOfMoves =
                                List.length edge.label

                            subGraph =
                                playMoveOnSac edge reducedGraph
                        in
                        case numberOfMoves of
                            0 ->
                                []

                            _ ->
                                List.repeat numberOfMoves (calculateNimber subGraph)
                    )
                    edges

            -- TODO: Problem with above is (i) edges includes moves both ways 1->2 and 2->1
            -- also we need to complete playMoveOnSac so that it updates the edges in both
            -- directions for a move.
        in
        combine subNimbers |> Maybe.map CalculateConnectedSubNimbersResult


{-| Calculates the number of a strings and coins graph that is connected. If
your graph might be split, then best to use calculateNimber in order to get
better performance.
-}
calculateConnectedNimber : Maybe CalculateConnectedSubNimbersResult -> Maybe Nimber
calculateConnectedNimber connectedSubNimbers =
    case connectedSubNimbers of
        Just CalculateConnectedSubNimbersResultLoony ->
            Just Loony

        Just (CalculateConnectedSubNimbersResult subNimbers) ->
            Just (mex subNimbers)

        Nothing ->
            Nothing


mex : List Nimber -> Nimber
mex nimbers =
    let
        numbers =
            List.filterMap
                (\nimber ->
                    case nimber of
                        Loony ->
                            Nothing

                        Nimber n ->
                            Just n
                )
                nimbers

        -- A fussy mex function that requires a sorted unqiue
        -- list as it's input, and uses tail recursion
        mex_ : Int -> List Int -> Int
        mex_ next list =
            case list of
                [] ->
                    next

                x :: xs ->
                    if next == x then
                        -- The mex can't be 'next' so keep searching
                        mex_ (next + 1) xs

                    else
                        -- Next is not in the list, so it is the mex
                        next
    in
    numbers |> unique |> List.sort |> mex_ 0 |> Nimber


capturableCoinsForGraph : StringsAndCoinsGraph -> List NodeId
capturableCoinsForGraph graph =
    Debug.log "capturableCoinsForGraph" (List.filter (\nodeId -> checkTake nodeId graph == CanTake) (Graph.nodes graph |> List.map (\node -> node.id)))


loonyCoinsForGraph : StringsAndCoinsGraph -> List NodeId
loonyCoinsForGraph graph =
    List.filter (\nodeId -> checkTake nodeId graph == CanTakeLoony) (Graph.nodes graph |> List.map (\node -> node.id))


{-| The nimber of a position is unchanged when capturable coins (but not loony ones) are
taken.
-}
removeCapturableCoinsReduction : StringsAndCoinsGraph -> StringsAndCoinsGraph
removeCapturableCoinsReduction graph =
    removeMany (capturableCoinsForGraph graph) graph


{-| A list of all the possible graph reductions that doesn't affect the nimber
-}
reductions : List (StringsAndCoinsGraph -> StringsAndCoinsGraph)
reductions =
    [ removeCapturableCoinsReduction ]


{-| Apply all possible graph reductions repeatedly until the reductions have
no effect, then stop.
-}
applyReductionsUntilComplete : Int -> StringsAndCoinsGraph -> StringsAndCoinsGraph
applyReductionsUntilComplete depth graph =
    let
        reducedGraph =
            List.foldl (\reduction g -> reduction g) graph reductions
    in
    if depth == 0 || List.length (Graph.nodes graph) == List.length (Graph.nodes reducedGraph) then
        reducedGraph

    else
        let
            x =
                Debug.log "reductions" ""
        in
        applyReductionsUntilComplete (depth - 1) reducedGraph


type TakeableResult
    = CanTake
    | CanTakeLoony
    | CannotTake


{-| Gets the connected coins. Will return the same coin multiple times if there
are multiple moves connecting it, for example a corner coin has two connections
to ground
-}
sacConnectedCoins : NodeId -> StringsAndCoinsGraph -> List NodeId
sacConnectedCoins nodeId graph =
    Graph.get nodeId graph
        |> Maybe.map
            (\nodeContext ->
                IntDict.toList nodeContext.outgoing |> List.concatMap (\( connectedNodeId, moves ) -> List.repeat (List.length moves) connectedNodeId)
            )
        |> Maybe.withDefault []


checkTake : NodeId -> StringsAndCoinsGraph -> TakeableResult
checkTake nodeId graph =
    case Debug.log "sc1" (sacConnectedCoins nodeId graph) of
        -- Either this is the ground node (unexpected!) or an already won coin
        [] ->
            CannotTake

        -- This is takable, we now need to decide if it is a no-brainer or a loony
        -- position
        [ nodeId2 ] ->
            let
                dbg =
                    Debug.log "nodeId2" nodeId2
            in
            case Debug.log "sc2" (sacConnectedCoins nodeId2 graph) of
                -- This means we are on the ground node, since this is the only node
                -- you can traverse to without being able to traverse back.
                -- (See Figure 21 (a) of DABGSCP)
                [] ->
                    CanTake

                -- This means we have 2 coins connected like this O-O
                -- (See Figure 21 (b) of DABGSCP)
                [ _ ] ->
                    CanTake

                -- This means we have a O-O-? and we need to traverse more to figure
                -- it out
                [ a, b ] ->
                    let
                        nodeId3 =
                            Debug.log "nodeId3" <|
                                if a == nodeId then
                                    b

                                else
                                    a
                    in
                    case
                        Debug.log "sc3" (sacConnectedCoins nodeId3 graph)
                    of
                        -- This means O-O-, which is loony because you can either take the coin
                        -- or leave a hard hearted handout
                        -- (See Figure 21 (e) of DABGSCP)
                        [] ->
                            CanTakeLoony

                        -- This means O-O-O, which is not loony because you are forced to make both
                        -- moves
                        -- (See Figure 21 (c) of DABGSCP)
                        [ _ ] ->
                            CanTake

                        -- This means O-O-O-, which is loony because hte middle string double crosses,
                        -- the first string takes.
                        -- (See Figure 21 (f) of DABGSCP)
                        _ ->
                            CanTakeLoony

                -- This means we have 2 coins connected like this O-O= where = is >= 2
                -- strings
                -- (See Figure 21 (d) of DABGSCP)
                _ ->
                    CanTake

        -- There are more than one connected coins, so ther is not takable now
        _ ->
            CannotTake
