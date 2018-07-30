{-
    Find the shortest path from Heathrow airport to London.

    This exercise comes from <Learn You an Elm>, Chapter "Functionally solving
    problems",

    https://learnyouanelm.github.io/pages/10-functionally-solving-problems.html
-}
module HeatLondon exposing(..)
import Dict exposing (Dict)
import List exposing (foldl)


roads : List (String, Int)
roads = [
    ("Start-A1", 50),
    ("Start-B1", 10),
    ("A1-B1", 30),
    ("A1-A2", 5),
    ("B1-B2", 90),
    ("A2-B2", 20),
    ("A2-A3", 40),
    ("B2-B3", 2),
    ("A3-B3", 25),
    ("A3-End", 10),
    ("B3-End", 8)
    ]


type alias EndPoint = (String, Int)
type alias RoadGraph = Dict String (List EndPoint)


buildGraph : List (String, Int) -> RoadGraph
buildGraph list =
    let
        accumulator : EndPoint -> RoadGraph -> RoadGraph
        accumulator x acc =
            let
                (roadString, distance) = x
                points = String.split "-" roadString

                update : String -> String -> Int -> RoadGraph -> RoadGraph
                update startPoint endPoint distance dict =
                    let
                        append : Maybe (List EndPoint) -> Maybe (List EndPoint)
                        append list =
                            case list of
                                Nothing -> Just [(endPoint, distance)]
                                Just xs -> Just ((endPoint, distance) :: xs)
                    in
                        Dict.update startPoint append dict

            in
                case points of
                    x::y::ys -> update y x distance <| update x y distance acc
                    _ -> acc

    in
        foldl accumulator Dict.empty roads


type alias EndPointPath = (List String, Int)
type alias ShortestRoadPath = Dict String EndPointPath



output : String
output =
    toString <| buildGraph roads
