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


{-
    Define a binary tree

    A binary tree is either an empty tree or it's an element that contains
    some value and two sub trees.

    To insert an value into a binary tree, we do:

    1. If it's an empty tree, then create the first node with the value a,
        i.e., the root, and two sub trees (both empty).

    2. If the tree is not empty, compare the value to the value contained in
        the root, then
            if it is bigger, go to the right subtree and repeat (1)
            if it is smaller, go to the left subtree and repeat (1)

    This part of code comes from "Learn you an Elm",

    https://learnyouanelm.github.io/pages/08-making-our-own-types-and-typeclasses.html
-}
type Tree a =
    EmptyTree
    | TreeNode a (Tree a) (Tree a)

singleton : a -> Tree a
singleton x =
    TreeNode x EmptyTree EmptyTree

treeInsert : comparable -> Tree comparable -> Tree comparable
treeInsert x tree =
    case tree of
        EmptyTree -> singleton x
        TreeNode value left right ->
            if x == value then
                tree    -- no change
            else if x > value then
                TreeNode value left (treeInsert x right)
            else
                TreeNode value (treeInsert x left) right


-- Test whether a value is an element in the tree
treeElem : comparable -> Tree comparable -> Bool
treeElem x tree =
    case tree of
        EmptyTree -> False
        TreeNode value left right ->
            if x == value then
                True
            else if x > value then
                treeElem x right
            else
                treeElem x left


treeFromList : List comparable -> Tree comparable
treeFromList =
    List.foldl treeInsert EmptyTree



output : String
output =
    -- showSize Small
    -- toString <| Small
    -- toString <| List.map showUser [u1, u2, u3, u4]
    -- showList n4

    -- test binary tree
    let
        tree = treeFromList [5,3,7,1,4,6,8]
    in
        toString tree
        -- toString <| treeElem 10 tree
        -- toString <| treeElem 8 tree
