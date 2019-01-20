module Extensions.Graph exposing (nodeLabelsForEdge, removeEdge, removeMany)

import Graph exposing (..)
import IntDict
import Maybe exposing (..)


{-| Get the 2 labels for the nodes connected to an edge in a graph
-}
nodeLabelsForEdge : Graph n e -> Edge e -> Maybe ( n, n )
nodeLabelsForEdge graph edge =
    Maybe.map2 Tuple.pair
        (get
            edge.from
            graph
            |> Maybe.map (\n -> n.node.label)
        )
        (get
            edge.to
            graph
            |> Maybe.map (\n -> n.node.label)
        )


removeMany : List NodeId -> Graph n e -> Graph n e
removeMany list graph =
    List.foldl Graph.remove graph list


removeEdge : NodeId -> NodeId -> Graph n e -> Graph n e
removeEdge from to =
    Graph.update from (Maybe.map (\nc -> { nc | outgoing = IntDict.remove to nc.outgoing }))
