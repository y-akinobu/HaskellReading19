import Data.List

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x * y):ys
        foldingFunction (x:y:ys) "+" = (x + y):ys
        foldingFunction (x:y:ys) "-" = (y - x):ys
        foldingFunction (x:y:ys) "/" = (y + x):ys
        foldingFunction (x:y:ys) "^" = (y ** x):ys
        foldingFunction (x:xs) "ln" = log x:xs
        foldingFunction xs "sum" = [sum xs]
        foldingFunction xs numberString = read numberString:xs

data Ope = Add | Sub | Mul | Div deriving (Eq, Ord)

instance Show Ope where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

changeRPN :: String -> String
changeRPN = concat . intersperse " " . reverse . (\(s, opes) -> reverse(map show opes) ++ s) . head . foldl foldingFunction ([], []) . words
  where foldingFunction (s, []) "/" = (s, [Div])
        foldingFunction (s, []) "*" = (s, [Mul])
        foldingFunction (s, []) "+" = (s, [Add])
        foldingFunction (s, []) "-" = (s, [Sub])
        foldingFunction (s, []) "^" = (s, [Pow])
        foldingFunction (s, opes) 
        foldingFunction (s, opes) num = (num:s, opes)

calc :: String -> Float
calc = solveRPN . changeRPN