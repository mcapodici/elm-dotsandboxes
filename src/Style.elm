module Style exposing (colourScheme, dialogStyles, gameStyles, headerStyles, name, toElmUiColor)

import Color
import Element as El
import Element.Background as ElBackground
import Element.Border as ElBorder
import Element.Font as ElFont


{-| Colour scheme to use as basis for choosing other colours. From <https://www.canva.com/learn/website-color-schemes/>
-}
modernCleanColourScheme =
    { tangerine = Color.rgb255 0xE3 0x72 0x72
    , teal = Color.rgb255 0x07 0x88 0x9B
    , powder = Color.rgb255 0x66 0xB9 0xBF
    , tan = Color.rgb255 0xEE 0xAA 0x7B
    }


{-| Colour scheme to be used in this application
-}
colourScheme =
    { topBackground = modernCleanColourScheme.powder
    , topBorder = modernCleanColourScheme.teal
    , startDialogBackground = modernCleanColourScheme.powder
    , headerFont = Color.white
    , playerAColor = modernCleanColourScheme.teal
    , playerBColor = modernCleanColourScheme.tangerine
    }


toElmUiColor : Color.Color -> El.Color
toElmUiColor =
    Color.toRgba >> El.fromRgb


zeroEach =
    { bottom = 0, top = 0, left = 0, right = 0 }


name =
    "Dots & Boxes"


headerStyles =
    { headerStyle =
        [ El.height <| El.px 50
        , El.width <| El.fill
        , ElBackground.color (toElmUiColor colourScheme.topBackground)
        , El.padding 5
        , ElBorder.solid
        , ElBorder.widthEach { zeroEach | bottom = 1 }
        , ElBorder.color (toElmUiColor colourScheme.topBorder)
        , ElFont.color (toElmUiColor colourScheme.headerFont)
        ]
    , titleTextStyle =
        [ El.centerY
        , El.paddingEach { zeroEach | left = 10, right = 30 }
        , ElFont.bold
        ]
    , currentPlayerTextStyle =
        [ El.centerY
        , El.paddingEach { zeroEach | left = 30 }
        ]
    , seperatorStyle =
        [ El.width (El.px 2)
        , El.height (El.px 30)
        , ElBackground.color <| toElmUiColor colourScheme.headerFont
        ]
    }


dialogStyles =
    { dialogStyle =
        [ El.spacing 25
        , El.padding 25
        , ElBorder.rounded 5
        , ElBorder.color (toElmUiColor colourScheme.startDialogBackground)
        , ElBorder.width 3
        ]
    , dialogContainerStyle =
        [ El.paddingEach { zeroEach | top = 50 }
        , El.centerX
        ]
    , inputLabelStyle =
        [ El.padding 15
        , El.centerX
        , ElFont.bold
        ]
    , buttonStyle =
        [ ElBorder.color (toElmUiColor colourScheme.startDialogBackground)
        , ElBorder.width 2
        , ElBorder.solid
        , El.padding 8
        , ElBorder.rounded 6
        , ElBackground.color (toElmUiColor colourScheme.startDialogBackground)
        ]
    }


gameStyles =
    { dotRadius = 8
    , lineWidth = 15
    }
