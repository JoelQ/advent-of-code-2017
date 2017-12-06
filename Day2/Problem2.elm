module Day2.Problem2 exposing (main)

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


dividesCleanly : Int -> Int -> Maybe Int
dividesCleanly x y =
    if x % y == 0 then
        Just (x // y)
    else
        Nothing


divisor : Int -> List Int -> Maybe Int
divisor x posibleDivisors =
    posibleDivisors
        |> List.filterMap (dividesCleanly x)
        |> List.head


diffRow : List Int -> Maybe Int
diffRow list =
    case List.sort list |> List.reverse of
        [] ->
            Nothing

        x :: xs ->
            case divisor x xs of
                Nothing ->
                    diffRow xs

                Just quotient ->
                    Just quotient


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
