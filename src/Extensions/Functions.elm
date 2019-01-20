module Extensions.Functions exposing (curry, uncurry)

{-| Change how arguments are passed to a function.
This splits paired arguments into two separate arguments.
-}


curry : (( a, b ) -> c) -> a -> b -> c
curry f a b =
    f ( a, b )


{-| Change how arguments are passed to a function.
This combines two arguments into a single pair.
-}
uncurry : (a -> b -> c) -> ( a, b ) -> c
uncurry f ( a, b ) =
    f a b
