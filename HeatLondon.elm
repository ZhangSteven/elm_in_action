{-
    Find the shortest path from Heathrow airport to London.

    This exercise comes from <Learn You an Elm>, Chapter "Functionally solving
    problems",

    https://learnyouanelm.github.io/pages/10-functionally-solving-problems.html

    The code below tries to find a general solution which finds a Shortest
    path between two points in a graph. But the "updatePath" and
    "findShortestPath" are not completed yet.
-}
module HeatLondon exposing(..)
import Dict exposing (Dict)
import List exposing (foldl)


type alias Distance = Int
roads : List (String, Distance)
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


{-
    Build a graph for the connections between points. Each point is a key
    in the RoadGraph dictionary whose value is the list of points directly
    reachable from that point, e.g., "A1":

    "A1" : [("A2", 5), ("B1", 30), ("Start", 50)]
-}
type alias EndPoint = (String, Distance)
type alias RoadGraph = Dict String (List EndPoint)

buildGraph : List (String, Distance) -> RoadGraph
buildGraph list =
    let
        accumulator : EndPoint -> RoadGraph -> RoadGraph
        accumulator x acc =
            let
                (roadString, distance) = x
                points = String.split "-" roadString

                update : String -> String -> Distance -> RoadGraph -> RoadGraph
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


{-
    Find the shortest path from point A to B.

    Suppose we want to find shortest path from A1 to B3, here is how it works:

    Starting from A1, list the direct reachable points and the distance,

        Points reachable    Shortest Path (point tried: A1)
        A1                  ([A1], 0)
        Start               ([A1, Start], 50)
        B1                  ([A1, B1], 30)
        A2                  ([A1, A2], 5)

    In the next round, from the newly added points (Start, B1 and A2), add
    new destinations reachable and update existing path if a shorter path is
    found, as below,

        Points reachable    Shortest Path (points tried: A1, Start, B1, A2)
        A1                  ([A1], 0)
        Start               ([A1, B1, Start], 40)   -- updated by B1
        B1                  ([A1, B1], 30)
        A2                  ([A1, A2], 5)
        B2                  ([A1, A2, B2], 25)  -- updated by B1, then A2
        A3                  ([A1, A2, A3], 45)  -- updated by A2

    In the next round, from the newly added points (B2, A3), repeat the process,

        Points reachable    Shortest Path (tried A1, Start, B1, A2, B2, A3)
        A1                  ([A1], 0)
        Start               ([A1, B1, Start], 40)   -- updated by B1
        B1                  ([A1, B1], 30)
        A2                  ([A1, A2], 5)
        B2                  ([A1, A2, B2], 25)  -- updated by B1, then A2
        A3                  ([A1, A2, A3], 45)  -- updated by A2
        B3                  ([A1, A2, B2, B3], 27)  -- updated by B2
        A4                  ([A1, A2, A3, A4], 55)  -- updated by A3

    So far we have found a relatively path to B3, namely

    A1, A2, B2, B3, 27 (The path via A3: A1, A2, A3, B3, 70 is already
    eliminated by the above process).

    But the above path may not be the shortest path, because if the Distance
    between B2 and B3 is 33 instead of 2, then A1-A2-B2-B3 with distance 68
    will still beat A1-A2-A3-B3 (70). However, in this case, the path A1-A2
    -A3-End-B3 (63) will be even shorter. Therefore, we need to exhaust the
    roadGraph to make sure we have the final shortest path.

    So the above iteration needs to go on, until all points in the dictionary
    have been tried.
-}
type alias RoadPath = (List String, Distance)
type alias RoadPathRecord = Dict String RoadPath

updatePath : List String -> RoadGraph -> RoadPathRecord -> RoadPathRecord
updatePath endPoints roadGraph roadPaths =
    let
        -- Find new keys in Dict B compared to Dict A
        newKeys : Dict comparable v -> Dict comparable v -> List comparable
        newKeys dictA dictB =
            Dict.keys <| Dict.diff dictB dictA

        newRoadPath = updatePath endPoints roadGraph roadPath
        newEndPoints = newKeys newRoadPath roadPath

    in
        if newEndPoints == [] then
            newRoadPath
        else
            updatePath newEndPoints roadGraph newRoadPath


findShortestPath : String -> String -> RoadGraph -> Maybe RoadPath
findShortestPath pointA pointB roadGraph =
    let



    in
        Dict.get pointB <| buildPath [pointA] roadGraph
                            <| Dict.fromList [(pointA, ([pointA], 0))]



output : String
output =
    toString <| buildGraph roads
