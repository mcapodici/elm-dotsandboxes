module Msg exposing (Msg(..))

import Animation as A
import Dict exposing (Dict(..))
import Game exposing (..)
import GameDialog
import Random exposing (Seed)


type Msg
    = NoOp
    | MakeMove Line
    | MakeComputerMoveInit
    | MakeComputerMove Seed
    | Animate A.Msg
    | GameDialogMessage GameDialog.Msg
