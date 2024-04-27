module Util exposing (intToEnglish, intToEnglishOrdinal)

{-| Util module for converting integers to natural language equivalents.

@docs intToEnglish, intToEnglishOrdinal

-}

import Array exposing (Array)


{-| Convert an integer into its English representation, e.g.

  - `10 -> "ten"`
  - `100 -> "one hundred"`

**Note that this does not support numbers over 999, since the package does not
need it.**

-}
intToEnglish : Int -> String
intToEnglish n =
    let
        lessThan100ToEnglish : Int -> String
        lessThan100ToEnglish i =
            if i < 20 then
                getEnglish onesAndTeens i

            else
                ( i // 10, remainderBy 10 i )
                    |> (\( d1, d2 ) ->
                            if d2 == 0 then
                                getEnglish tens d1

                            else
                                getEnglish tens d1 ++ " " ++ getEnglish onesAndTeens d2
                       )
    in
    if n < 100 then
        lessThan100ToEnglish n

    else
        ( n // 100, remainderBy 100 n )
            |> (\( d1, d23 ) ->
                    if d23 == 0 then
                        getEnglish onesAndTeens d1 ++ " hundred"

                    else
                        getEnglish onesAndTeens d1 ++ " hundred " ++ lessThan100ToEnglish d23
               )


{-| Convert an integer into its English ordinal representation, e.g.

  - `10 -> "tenth"`
  - `100 -> "one hundredth"`

**Note that this does not support numbers over 999, since the package does not
need it.**

-}
intToEnglishOrdinal : Int -> String
intToEnglishOrdinal n =
    let
        lessThan100ToEnglishOrdinal : Int -> String
        lessThan100ToEnglishOrdinal i =
            if i < 20 then
                getEnglish onesAndTeensOrdinal i

            else
                ( i // 10, remainderBy 10 i )
                    |> (\( d1, d2 ) ->
                            if d2 == 0 then
                                getEnglish tensOrdinal d1

                            else
                                getEnglish tens d1 ++ " " ++ getEnglish onesAndTeensOrdinal d2
                       )
    in
    if n < 100 then
        lessThan100ToEnglishOrdinal n

    else
        ( n // 100, remainderBy 100 n )
            |> (\( d1, d23 ) ->
                    if d23 == 0 then
                        getEnglish onesAndTeens d1 ++ " hundredth"

                    else
                        getEnglish onesAndTeens d1 ++ " hundred " ++ lessThan100ToEnglishOrdinal d23
               )


{-| Ordinal numbers below 20, at the appropriate index.
-}
onesAndTeensOrdinal : Array String
onesAndTeensOrdinal =
    [ "" -- Padding
    , "first"
    , "second"
    , "third"
    , "fourth"
    , "fifth"
    , "sixth"
    , "seventh"
    , "eighth"
    , "ninth"
    , "tenth"
    , "eleventh"
    , "twelfth"
    , "thirteenth"
    , "fourteenth"
    , "fifteenth"
    , "sixteenth"
    , "seventeenth"
    , "eighteenth"
    , "nineteenth"
    ]
        |> Array.fromList


{-| Ordinal numbers for 10s, from "twentieth" to "ninetieth", at the appropriate
index divided by 10, e.g. at index 3 is "thirtieth".
-}
tensOrdinal : Array String
tensOrdinal =
    [ "" -- Padding
    , "" -- Padding
    , "twentieth"
    , "thirtieth"
    , "fortieth"
    , "fiftieth"
    , "sixtieth"
    , "seventieth"
    , "eightieth"
    , "ninetieth"
    ]
        |> Array.fromList


{-| Given an array of strings and an index checked to be within bounds, get the
string at that index.
-}
getEnglish : Array String -> Int -> String
getEnglish a n =
    Maybe.withDefault "" <| Array.get n a


{-| Numbers below 20, at the appropriate index.
-}
onesAndTeens : Array String
onesAndTeens =
    [ "" -- Padding
    , "one"
    , "two"
    , "three"
    , "four"
    , "five"
    , "six"
    , "seven"
    , "eight"
    , "nine"
    , "ten"
    , "eleven"
    , "twelve"
    , "thirteen"
    , "fourteen"
    , "fifteen"
    , "sixteen"
    , "seventeen"
    , "eighteen"
    , "nineteen"
    ]
        |> Array.fromList


{-| Ordinal numbers for 10s, from "twenty" to "ninety", at the appropriate
index divided by 10, e.g. at index 3 is "thirty".
-}
tens : Array String
tens =
    [ "" -- Padding
    , "" -- Padding
    , "twenty"
    , "thirty"
    , "forty"
    , "fifty"
    , "sixty"
    , "seventy"
    , "eighty"
    , "ninety"
    ]
        |> Array.fromList
