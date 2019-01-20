module Svg.Animation.Types exposing (LineAnimations)

import Animation
import Dict.Any as AnyDict exposing (AnyDict)
import Game exposing (Line(..))


{-| Animation styles to apply to each line displayed in dots and boxes. The animtion
style final state is the "played" state, so it is just left in that state to show that
the move has been made.
-}
type alias LineAnimations =
    AnyDict ( Int, Int, Int ) Line Animation.State
