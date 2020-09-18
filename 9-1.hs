import Text.Printf

myExp:: Float -> Float
myExp x = taylor 1 1 1 where
    -- taylor:: Float -> Float -> Float -> Float
    taylor acc n mem = let mem' = mem * x / n
                           acc' = acc + mem'
                       in if acc' == acc
                          then acc
                          else taylor acc' (n+1) mem'

main = putStr $ foldMap (\x -> let x' = printf "%3.f" x in "myExp " ++ x' ++ "   -   exp " ++ x' ++ " = " ++ show (myExp x - exp x) ++ "\n") [1, 5, 10, 15, 20, 25, -1, -5, -10, -15, -20, -25]

-- В случае отрицательного аргумента можно считать 1 / myExp (abs x)