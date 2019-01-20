module Main exposing (main)

import Animation as A
import Basics exposing (toFloat)
import Browser exposing (Document)
import Color
import Dict.Any as AD
import Element as El
import Element.Background as ElBg
import Element.Border as ElBorder
import Element.Font as ElFont
import Game exposing (Game, Line(..), Way(..), Win(..), moves, play, player, wins)
import GameDialog exposing (GameDialogModel)
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List exposing (map, range, take)
import Model exposing (Model)
import Msg exposing (..)
import Platform
import Player
import Process
import Random exposing (Seed)
import Style exposing (toElmUiColor)
import Svg exposing (LineDisplayInfo, gameToSvg)
import Svg.Animation exposing (..)
import Svg.Animation.Types exposing (LineAnimations)
import Task


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> NoOp
        , onUrlChange = \_ -> NoOp
        }


init : Model
init =
    { game = Game.initGame { width = 3, height = 3 }
    , computerIsThinking = False
    , styles = Svg.Animation.initialLineAnimations
    , gameDialog = GameDialog.initGameDialogModel
    }


subscriptions : Model -> Sub Msg
subscriptions =
    Svg.Animation.subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MakeMove line ->
            let
                updatedGame =
                    play line model.game

                computerTurn =
                    playerIsComputer { model | game = updatedGame }
            in
            ( { model
                | game = updatedGame
                , computerIsThinking = computerTurn
                , styles = Svg.Animation.startPlayedAnimationForLine line model.styles
              }
            , if computerTurn then
                delay 200 MakeComputerMoveInit

              else
                Cmd.none
            )

        MakeComputerMoveInit ->
            ( model, computerMoveMsg )

        MakeComputerMove seed ->
            case Player.makeGreedyMove seed model.game of
                Just line ->
                    let
                        ( newModel, cmd ) =
                            update (MakeMove line) model
                    in
                    ( newModel, cmd )

                Nothing ->
                    ( { model | computerIsThinking = False }, Cmd.none )

        Animate time ->
            ( { model
                | styles = Svg.Animation.animate time model.styles
              }
            , Cmd.none
            )

        GameDialogMessage m ->
            let
                ( subModel, subCmd ) =
                    GameDialog.update m model.gameDialog

                modelWithUpdatedSubmodel =
                    { model | gameDialog = subModel }

                ( finalModel, additionalCmd ) =
                    checkForGameStart m modelWithUpdatedSubmodel
            in
            ( finalModel
            , Cmd.batch
                [ Cmd.map GameDialogMessage subCmd
                , additionalCmd
                ]
            )


checkForGameStart : GameDialog.Msg -> Model -> ( Model, Cmd Msg )
checkForGameStart msg model =
    case msg of
        GameDialog.Start ->
            let
                wh =
                    case model.gameDialog.sizeChoice of
                        GameDialog.ThreeByThree ->
                            3

                        GameDialog.FourByFour ->
                            4

                        GameDialog.FiveByFive ->
                            5

                newModel =
                    { model | game = Game.initGame { width = wh, height = wh } }
            in
            ( newModel
            , if playerIsComputer newModel then
                delay 200 MakeComputerMoveInit

              else
                Cmd.none
            )

        _ ->
            ( model, Cmd.none )


computerMoveMsg : Cmd Msg
computerMoveMsg =
    Random.generate (\v -> MakeComputerMove (Random.initialSeed v)) (Random.int Random.minInt Random.maxInt)


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


view : Model -> Document Msg
view model =
    let
        currentPlayerText =
            if model.gameDialog.isShown then
                ""

            else if Game.isFinished model.game then
                case Game.winner model.game of
                    Just PlayerA ->
                        "Game finished, winner is A"

                    Just PlayerB ->
                        "Game finished, winner is B"

                    Nothing ->
                        "Game finished, it's a draw"

            else
                (case model.game.player of
                    PlayerA ->
                        "A"

                    _ ->
                        "B"
                )
                    ++ " to play"

        currentScoreText =
            if model.gameDialog.isShown then
                ""

            else
                (Game.score model.game PlayerA |> String.fromInt)
                    ++ " - "
                    ++ (Game.score model.game PlayerB |> String.fromInt)

        computerThinkingText =
            if model.computerIsThinking then
                "Thinking ..."

            else
                ""

        movesWithStyle =
            Svg.Animation.calcMovesWithStyle (moves model.game) model.styles
    in
    { title = "Dots And Boxes"
    , body =
        [ El.layout [ El.height El.fill, El.width El.fill ]
            (El.column [ El.height <| El.fill, El.width <| El.fill, El.spaceEvenly ]
                [ El.el
                    Style.headerStyles.headerStyle
                    (El.row
                        [ El.height El.fill, El.width El.fill ]
                        [ El.el Style.headerStyles.titleTextStyle (El.text Style.name)
                        , El.el Style.headerStyles.seperatorStyle El.none
                        , showIfText (El.text >> El.el Style.headerStyles.currentPlayerTextStyle) currentScoreText
                        , showIfText (El.text >> El.el Style.headerStyles.currentPlayerTextStyle) currentPlayerText
                        ]
                    )
                , if model.gameDialog.isShown then
                    El.column
                        Style.dialogStyles.dialogContainerStyle
                        [ El.map GameDialogMessage <| GameDialog.view model.gameDialog ]

                  else
                    El.el
                        [ El.width <| El.minimum 200 (El.maximum 600 El.fill), El.centerX ]
                        (El.html <| gameToSvg (not <| playerIsComputer model) model.game movesWithStyle)
                , El.el
                    [ El.height <| El.px 100, El.height El.fill, El.padding 5 ]
                    El.none
                ]
            )
        ]
    }


showIfText : (String -> El.Element Msg) -> String -> El.Element Msg
showIfText f s =
    if s == "" then
        El.none

    else
        f s


playerIsComputer : Model -> Bool
playerIsComputer model =
    let
        player =
            model.game.player
    in
    (player == PlayerA && model.gameDialog.playerOneIsComputer)
        || (player == PlayerB && model.gameDialog.playerTwoIsComputer)
