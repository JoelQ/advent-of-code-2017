module Day1.Problem1 exposing (main)

import Html exposing (..)
import Html.Events exposing (onInput)


stringToNumbers : String -> List Int
stringToNumbers string =
    string
        |> String.split ""
        |> List.filterMap (Result.toMaybe << String.toInt)


rotateListBy : Int -> List a -> List a
rotateListBy n list =
    (List.drop n list) ++ (List.take n list)


numberIfMatch : Int -> Int -> Int
numberIfMatch x y =
    if x == y then
        x
    else
        0


solve : List Int -> Int
solve list =
    List.map2 numberIfMatch (list) (rotateListBy 1 list)
        |> List.sum



-- THE ELM ARCHITECTURE


viewSolution : String -> Html a
viewSolution string =
    div [] [ text <| toString <| solve <| stringToNumbers string ]


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
