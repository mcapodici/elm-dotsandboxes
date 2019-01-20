module GameDialog exposing (GameDialogModel, Msg(..), SizeChoice(..), initGameDialogModel, update, view)

import Color
import Element as El
import Element.Background as ElBackground
import Element.Border as ElBorder
import Element.Font as ElFont
import Element.Input as ElInput
import Game
import Html exposing (Html)
import Style exposing (dialogStyles, toElmUiColor)


type Msg
    = Start
    | ChooseGameSize SizeChoice
    | ChoosePlayerOneIsComputer Bool
    | ChoosePlayerTwoIsComputer Bool


type SizeChoice
    = ThreeByThree
    | FourByFour
    | FiveByFive


type alias GameDialogModel =
    { isShown : Bool
    , playerOneIsComputer : Bool
    , playerTwoIsComputer : Bool
    , sizeChoice : SizeChoice
    }


initGameDialogModel : GameDialogModel
initGameDialogModel =
    { isShown = True
    , playerOneIsComputer = False
    , playerTwoIsComputer = True
    , sizeChoice = FourByFour
    }


update : Msg -> GameDialogModel -> ( GameDialogModel, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( { model | isShown = False }, Cmd.none )

        ChooseGameSize choice ->
            ( { model | sizeChoice = choice }, Cmd.none )

        ChoosePlayerOneIsComputer choice ->
            ( { model | playerOneIsComputer = choice }, Cmd.none )

        ChoosePlayerTwoIsComputer choice ->
            ( { model | playerTwoIsComputer = choice }, Cmd.none )


view : GameDialogModel -> El.Element Msg
view model =
    El.column
        ([ El.height <| El.fill
         , El.width <| El.fill
         ]
            ++ dialogStyles.dialogStyle
        )
        [ El.el
            [ El.centerY, El.centerX, ElFont.bold ]
            (El.text <| Style.name ++ ": New Game")
        , El.el
            [ El.centerY, El.centerX ]
            (ElInput.radio [ El.spacing 15 ]
                { onChange = ChooseGameSize
                , selected = Just model.sizeChoice
                , label = ElInput.labelAbove dialogStyles.inputLabelStyle (El.text "Size:")
                , options =
                    [ ElInput.option ThreeByThree (El.text "3 by 3")
                    , ElInput.option FourByFour (El.text "4 by 4")
                    , ElInput.option FiveByFive (El.text "5 by 5")
                    ]
                }
            )
        , El.el
            [ El.centerY, El.centerX ]
            (ElInput.radioRow [ El.spacing 15 ]
                { onChange = ChoosePlayerOneIsComputer
                , selected = Just model.playerOneIsComputer
                , label = ElInput.labelAbove dialogStyles.inputLabelStyle (El.text "Player 1:")
                , options =
                    [ ElInput.option True (El.text "Computer")
                    , ElInput.option False (El.text "Human")
                    ]
                }
            )
        , El.el
            [ El.centerY, El.centerX ]
            (ElInput.radioRow [ El.spacing 15 ]
                { onChange = ChoosePlayerTwoIsComputer
                , selected = Just model.playerTwoIsComputer
                , label = ElInput.labelAbove dialogStyles.inputLabelStyle (El.text "Player 2:")
                , options =
                    [ ElInput.option True (El.text "Computer")
                    , ElInput.option False (El.text "Human")
                    ]
                }
            )
        , ElInput.button
            ([ El.centerY
             , El.centerX
             ]
                ++ dialogStyles.buttonStyle
            )
            { onPress = Just Start
            , label = El.el [ ElFont.bold ] (El.text "Start")
            }
        ]
