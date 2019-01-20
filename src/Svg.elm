module Svg exposing (LineDisplayInfo, gameToSvg)

import Animation
import Color
import Dict.Any as AnyDict exposing (AnyDict)
import Extensions.List exposing (cartesian)
import Game exposing (Game, Line(..), Way(..), Win(..), moves, play, player, wins)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List exposing (concatMap, map, range, take)
import Msg exposing (..)
import Style
import TypedSvg exposing (circle, line, svg, text_)
import TypedSvg.Attributes exposing (alignmentBaseline, cx, cy, fill, fontSize, height, r, shapeRendering, stroke, strokeWidth, style, textAnchor, viewBox, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), Fill(..), ShapeRendering(..), px)


type alias LineDisplayInfo =
    { line : Line
    , played : Bool
    , style : List (Html.Attribute Msg)
    }


gameToSvg : Bool -> Game -> List LineDisplayInfo -> Html Msg
gameToSvg allowPlayByClick game moves =
    svg
        [ viewBox 0 0 (100 * (toFloat game.size.width + 2)) (100 * (toFloat game.size.height + 2))
        , style "width:100%"
        ]
    <|
        gameToSvgLines allowPlayByClick moves
            ++ makeDots game.size.width game.size.height
            ++ gameToSvgWins game


makeDots : Int -> Int -> List (Html Msg)
makeDots width height =
    cartesian (range 1 <| width + 1) (range 1 <| height + 1)
        |> map
            (\( x, y ) ->
                circle
                    [ cx (px (toFloat x * 100))
                    , cy (px (toFloat y * 100))
                    , r (px Style.gameStyles.dotRadius)
                    , fill <| Fill Color.black
                    ]
                    []
            )


gameToSvgWins : Game -> List (Html Msg)
gameToSvgWins game =
    wins game
        |> concatMap
            (\( posX, posY, player ) ->
                [ circle
                    [ cx (px (toFloat posX * 100 + 50))
                    , cy (px (toFloat posY * 100 + 50))
                    , r (px 25)
                    , fill <|
                        Fill <|
                            case player of
                                PlayerA ->
                                    Style.colourScheme.playerAColor

                                PlayerB ->
                                    Style.colourScheme.playerBColor
                    ]
                    []
                , text_
                    {--Hack: can't user AlignmentCentral as Edge doesn't support it (https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/12052362/)
                    therefore just position the text a bit higher to make it look vertically centered --}
                    [ x (px (toFloat posX * 100 + 50))
                    , y (px (toFloat posY * 100 + 60))
                    , textAnchor AnchorMiddle
                    , fontSize (px 30)

                    --, alignmentBaseline AlignmentCentral
                    ]
                    [ text <|
                        case player of
                            PlayerA ->
                                "A"

                            PlayerB ->
                                "B"
                    ]
                ]
            )


singleSvgLine : Bool -> LineDisplayInfo -> Html Msg
singleSvgLine allowPlayByClick lineDisplayInfo =
    let
        (Line w x y) =
            lineDisplayInfo.line

        originX =
            toFloat x * 100

        originY =
            toFloat y * 100

        ( plusX, plusY ) =
            case w of
                Horizontal ->
                    ( 100.0, 0.0 )

                Vertical ->
                    ( 0.0, 100.0 )
    in
    line
        (lineDisplayInfo.style
            ++ [ x1 (px <| originX)
               , y1 (px <| originY)
               , x2 (px <| originX + plusX)
               , y2 (px <| originY + plusY)
               , strokeWidth (px Style.gameStyles.lineWidth)
               , onClick <|
                    if not allowPlayByClick || lineDisplayInfo.played then
                        NoOp

                    else
                        MakeMove lineDisplayInfo.line
               ]
        )
        []


gameToSvgLines : Bool -> List LineDisplayInfo -> List (Html Msg)
gameToSvgLines allowPlayByClick lineDisplayInfos =
    let
        playedLines =
            List.filter (\i -> i.played) lineDisplayInfos

        unPlayedLines =
            List.filter (\i -> not i.played) lineDisplayInfos
    in
    map (singleSvgLine allowPlayByClick) unPlayedLines
        ++ map (singleSvgLine allowPlayByClick) playedLines
