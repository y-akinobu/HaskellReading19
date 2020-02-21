import Control.Monad
import Data.Char

main = forever $ do
  purStr "Give me some input: "
  | <- getLine
  putStrLn $ map tpUpper |