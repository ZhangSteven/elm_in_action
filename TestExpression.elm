{-
    Some basics in Elm.
-}
module TestExpression exposing(..)

{-
    In Elm, everything is a expression. A function is also an expression
    that evalutes to a value. There is no 'return' statement in Elm.

    The below imperative style of programming is not allowed:

    add x =
        a = 5
        a + x

    Instead, we do it in a declarative way as below:
-}
add : Int -> Int
add x =
    let
        a = 5
    in
        a + x   -- the expression's value is the function's return value


{-
    Int is bounded, which means it has a minimum and maximum value, between
    -2147483648 and 2147483647
-}
change : Int -> Int
change n =
    -- 'If' is also an expression, it has two branches - 'then' and 'else',
    -- one of them will be evaluted as the result of 'if'.
    if n % 2 == 0
    then
        n // 2      -- since n is of type Int, must do integer division
    else
        3 * n + 1


output : String
output =
    toString (
        add 5       -- 10
        +
        change 12   -- 6
    )
