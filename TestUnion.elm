{-
    Union in Elm
-}
module TestUnion exposing(..)


{-
    Define a Union type, a simple case.

    In the simple case, we can think of a union like 'PictureSize' as a group
    of symbols.

    The names of the type and symbols must have their first letter capitalized.

    Actually, the boolean type is a union.

    type Bool =
        True
        | False
-}
type PictureSize =
    Small
    | Medium
    | Large


showSize : PictureSize -> String
showSize s =
    case s of
        Small -> "small"
        Medium -> "medium"
        Large -> "large"


{-
    Once defined, the names of the type and symbols will occupy the name space.

    We cannot use those names to define other things. The below won't compile.

    Small = (+) 1   -- define a new function
-}


{-
    Define Union with a function

    Say we would like to define users in a chatroom, there are two possibilities

    an anonymous user
    a user with a name
-}
type User =
    Anonymous
    | Named String  -- define 'Named' as a function of type : String -> User


-- create a few users
u1 = Named "Isaac"
u2 = Anonymous
u3 = Named "Chloe"
u4 = Anonymous


showUser : User -> String
showUser user =
    case user of
        Anonymous -> "anonymous user"
        Named name -> "user " ++ name   -- destructure the Union data, name
                                        -- represents the String part


{-
    Union to group data of different structure together

    Suppose we are creating a dashboard with three different types of
    widgets. One shows recent log data, one shows time plots, and one
    shows scatter plots. So how do we represent a widget?

    First, find out a data type to represent each type of widget.
    Second, use a function name to tag each type of widget in a Union.
    Third, create a general widget handler using a 'case' structure
-}
type alias LogsInfo =
    { logs : List String }

type alias TimeInfo =
    { events : List (String, Float)
    , yAxis : String
    }

type alias ScatterInfo =
    { points : List (Float, Float)
    , xAxis : String
    , yAxis : String
    }

type Widget =
    Logs LogsInfo
    | TimePlot TimeInfo
    | ScatterPlot ScatterInfo

{-
    Final event handler 'view'

    viewLogs, viewTimePlot, viewScatterPlot are very specialized function
    to handle each type of widget. We use a Union type to group their
    data together so that we can use a single handler view.

view : Widget -> Html Msg
view widget =
    case widget of
        Logs info -> viewLogs info
        TimePlot info -> viewTimePlot info
        ScatterPlot info -> viewScatterPlot info

-}



{-
    Define Union recursively

    A linked list is a recursive structure, a node at the beginning (head) and
    the rest (another linked list).
-}
type IntList =
    Empty
    | Node Int IntList  -- Node : Int -> IntList -> IntList


-- create a few nodes
n1 = Empty  -- n1 : IntList
n2 = Node 10 n1     -- n2 : IntList
n3 = Node 20 n2
n4 = Node 30 n3

-- show a list
showList : IntList -> String
showList list =
    case list of
        Empty -> "empty"
        Node value remaining ->
            toString value ++ " " ++ showList remaining



output : String
output =
    -- showSize Small
    -- toString <| Small
    -- toString <| List.map showUser [u1, u2, u3, u4]
    showList n4
