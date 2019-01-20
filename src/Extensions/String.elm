module Extensions.String exposing (regexNewLine, regexSplit, removeEmptyStrings)

import Regex


{-| Used to split a string. Assumes regex input is valid, if it is not then nothing is matched
and the original string is returned unsplit
-}
regexSplit : String -> String -> List String
regexSplit regexString input =
    Regex.split (Regex.fromString regexString |> Maybe.withDefault Regex.never) input


removeEmptyStrings : List String -> List String
removeEmptyStrings =
    List.filter (String.isEmpty >> not)


regexNewLine : String
regexNewLine =
    "(\u{000D}\n|\u{000D}|\n)"
