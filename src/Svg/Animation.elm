module Svg.Animation exposing (animate, calcMovesWithStyle, initialLineAnimations, startPlayedAnimationForLine, subscriptions)

import Animation
import Color
import Dict.Any as AnyDict exposing (AnyDict)
import Game exposing (Line(..))
import Model exposing (Model)
import Msg exposing (Msg(..))
import Svg exposing (LineDisplayInfo)
import Svg.Animation.Types exposing (LineAnimations)


{-| Takes an animation message (i.e. the latest 'time') and updates all of the
animations.
-}
animate : Animation.Msg -> LineAnimations -> LineAnimations
animate time styles =
    AnyDict.map (\_ s -> Animation.update time s) styles


{-| The initial LineAnimations when the game is started (which is empty)
-}
initialLineAnimations : LineAnimations
initialLineAnimations =
    AnyDict.empty Game.lineToGameKey


startPlayedAnimationForLine : Line -> LineAnimations -> LineAnimations
startPlayedAnimationForLine line styles =
    let
        newStyle =
            playedMoveAnimation line styles
    in
    AnyDict.insert line newStyle styles


playedMoveAnimation : Line -> LineAnimations -> Animation.State
playedMoveAnimation line styles =
    Animation.interrupt
        [ Animation.set [ Animation.stroke <| colorToAnimationColor Color.gray ]
        , Animation.toWith (Animation.easing { duration = 500, ease = identity }) [ Animation.stroke <| colorToAnimationColor Color.black ]
        ]
    <|
        Maybe.withDefault
            (Animation.style [])
            (AnyDict.get line styles)


colorToAnimationColor : Color.Color -> Animation.Color
colorToAnimationColor c =
    let
        parts =
            Color.toRgba c
    in
    { red = round (parts.red * 255), blue = round (parts.blue * 255), green = round (parts.green * 255), alpha = parts.alpha }


unplayedStokeStyle : Animation.Property
unplayedStokeStyle =
    Animation.stroke <| colorToAnimationColor Color.white


calcMovesWithStyle : List ( Line, Bool ) -> LineAnimations -> List LineDisplayInfo
calcMovesWithStyle listOfMoves styles =
    List.map
        (\( line, played ) ->
            let
                initialStylesForLine =
                    Animation.style [ unplayedStokeStyle ]

                maybeCurrentStylesForLine =
                    AnyDict.get line styles
            in
            { line = line
            , played = played
            , style = Animation.render <| Maybe.withDefault initialStylesForLine maybeCurrentStylesForLine
            }
        )
        listOfMoves


subscriptions : Model -> Sub Msg
subscriptions model =
    Animation.subscription Animate (AnyDict.values model.styles)
