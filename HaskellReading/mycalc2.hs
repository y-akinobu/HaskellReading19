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

trance :: String -> [String]
trance = words

-- *Main> trance "1 + 2"
-- ["1","+","2"]

data Ope = Add | Sub | Mul | Div | Pow deriving (Eq, Ord)
-- 演算子の優先順位が低い <= => 高い

instance Show Ope where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Pow = "^"

-- 入力文字列("1 + 2") -> (出力文字列のリスト["2", "1"], 演算子のスタック[Add])
-- trance4 :: String -> ([String], [Ope])
-- trance4 = foldl foldingFunction ([], []) . words
--   where foldingFunction (s, []) "+" = (s, [Add])
--         foldingFunction (s, opes) num = (num: s, opes)

-- 入力文字列("1 + 2") -> 出力文字列("1 2 +")
trance8 :: String -> String
trance8 = concat . intersperse " " . reverse . (\(s, opes) -> reverse (map show opes) ++ s) . foldl foldingFunction ([], []) . words
  where foldingFunction (s, []) "*" = (s, [Mul])
        foldingFunction (s, []) "+" = (s, [Add])
        foldingFunction (s, ope:opes) "*" = if ope < Mul then (s, Mul:ope:opes) else foldingFunction ((show ope):s, opes)　"*"
        foldingFunction (s, ope:opes) "+" = if ope < Add then (s, Add:ope:opes) else foldingFunction ((show ope):s, opes) "+"
        foldingFunction (s, opes) num = (num: s, opes)