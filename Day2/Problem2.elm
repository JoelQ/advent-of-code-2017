module Day2.Problem2 exposing (main)

import Html exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (HowMany(..), regex)


-- Sub-squares whose largest numbers are the squares of even numbers are
-- lopsided (there is no center)
--
-- Sub-squares whose largest numbers are the squares of odd numbers are
-- perfectly centered around 1
--
-- Given a sub-square whose largest number is the the square of the odd number n
--
-- All numbers exist on the edge of some sub-square whose largest number is the
-- square of an odd number
--
-- The distance from any corner (max distance) is n - 1
--
-- To travel from the corner of the square to the center, you must travel
-- (n - 1 / 2) squares horizontally and (n - 1 / 2) squares vertically
--
-- The shortest distance from the edge and the center is (n - 1) / 2
--
-- The center of an edge is (n - 1) / 2 from the nearest corners


parseNumber : String -> Int
parseNumber =
    Result.withDefault 0 << String.toInt


solve : Int -> Int
solve position =
    let
        maxDistance =
            position
                |> toFloat
                |> sqrt
                |> ceiling
                |> (\n -> n - 1)

        inwardSteps =
            maxDistance // 2

        outwardSteps =
            (position - maxDistance + 1) % maxDistance
    in
        outwardSteps + inwardSteps



-- THE ELM ARCHITECTURE


viewSolution : String -> Html a
viewSolution string =
    div [] [ text <| toString <| solve <| parseNumber string ]


view : String -> Html String
view string =
    div []
        [ input [ onInput identity ] []
        , viewSolution string
        ]


main : Program Never String String
main =
    Html.beginnerProgram
        { model = ""
        , update = (\newString _ -> newString)
        , view = view
        }
