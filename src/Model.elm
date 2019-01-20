module Model exposing (Model)

import Game exposing (Game)
import GameDialog exposing (GameDialogModel)
import Svg.Animation.Types exposing (LineAnimations)


type alias Model =
    { game : Game
    , computerIsThinking : Bool
    , styles : LineAnimations
    , gameDialog : GameDialogModel
    }
