main = do
  contects <- getContents
  putStr(shortLinesOnly contects)

shortLinesOnly :: String -> String
shortLinesOnly input = 
  let allLine = lines input
    shortLines = filter(\line -> length line < 10) allLines
    result = utlines shortLines
  in result