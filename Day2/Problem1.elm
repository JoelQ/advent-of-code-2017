module Day2.Problem1 exposing (main)

import Html exposing (..)
import Html.Events exposing (onInput)
import Regex exposing (HowMany(..), regex)


type alias Table =
    List (List Int)


parseRow : String -> List Int
parseRow string =
    string
        |> Regex.split All (regex "\\s+")
        |> List.filterMap (Result.toMaybe << String.toInt)


parseTable : String -> Table
parseTable string =
    string
        |> String.split "\n"
        |> List.map parseRow


diffRow : List Int -> Maybe Int
diffRow list =
    Maybe.map2 (-) (List.maximum list) (List.minimum list)


solve : Table -> Int
solve table =
    table
        |> List.filterMap diffRow
        |> List.sum



-- THE ELM ARCHITECTURE


viewSolution : String -> Html a
viewSolution string =
    div [] [ text <| toString <| solve <| parseTable string ]


view : String -> Html String
view string =
    div []
        [ textarea [ onInput identity ] []
        , viewSolution string
        ]


main : Program Never String String
main =
    Html.beginnerProgram
        { model = ""
        , update = (\newString _ -> newString)
        , view = view
        }
