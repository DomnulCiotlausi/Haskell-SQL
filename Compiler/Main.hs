import Interpreter
import Data.List.Split

mySplit :: String -> [String]
mySplit [] = []
mySplit xs = splitOn "," xs

main = do
  inputA <- readFile "A.csv"
  inputB <- readFile "B.csv"
  let tokensA = alexScanTokens inputA
  let tokensB = alexScanTokens inputB
  let outA = parseCalc tokensA
  let outB = parseCalc tokensB
  putStr $ show outA
  putStr "\n"
  putStr $ show outB
  putStr "\n"
