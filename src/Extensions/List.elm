module Extensions.List exposing (cartesian, removeNothings, zipWithIndex, zipWithIndexWith)

import List exposing (..)
import List.Extra exposing (zip)


{-| Cartesian product of two lists, using the first list as the outer loop.

    cartesian [1,2] ["a","b","c"]

    -- result: [(1,"a"),(1,"b"),(1,"c"),(2,"a"),(2,"b"),(2,"c")]

-}
cartesian : List a -> List b -> List ( a, b )
cartesian xs ys =
    List.concatMap
        (\x -> List.map (\y -> ( x, y )) ys)
        xs


{-| Turns a list into a list of tuples where the first value is an index ranging from 0 to
the last index of the list, and the second value is the original list value.

    zipWithIndex ["a","b","c"]

    -- result: [(0,"a"),(1,"b"),(2,"c")]

-}
zipWithIndex : List a -> List ( Int, a )
zipWithIndex items =
    zip (List.range 0 (List.length items - 1)) items


{-| Turns a list into a list of items constructed based on the original item and an index
ranging from 0 to the last index of the list.

    zipWithIndexWith (+) [1000,2000,3000]

    -- result: [1000,2001,3002]

-}
zipWithIndexWith : (Int -> a -> b) -> List a -> List b
zipWithIndexWith f items =
    List.map (\( idx, item ) -> f idx item) (zipWithIndex items)


{-| Removes Nothing values from a list of maybes
-}
removeNothings : List (Maybe a) -> List a
removeNothings val =
    case val of
        [] ->
            []

        maybe :: rest ->
            case maybe of
                Just x ->
                    x :: removeNothings rest

                Nothing ->
                    removeNothings rest
