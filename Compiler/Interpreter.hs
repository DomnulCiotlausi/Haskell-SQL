import Tokens
import Grammar
import Data.List
import Data.List.Split
import Control.Applicative
import System.Environment

--Data types
type Location = Int
data Table = T Header Content
type Header = [String]
type Content = [[String]]

--MAIN
main = do
  path <- getArgs
  program <- readFile $ path!!0
  let parsed = parseCalc $ alexScanTokens program
  if program == "\n" || program == ""
    then error "Error. Empty program."
  else do
    let ok1 = check1 parsed
    if not ok1 then
      error "Error. Duplicate values."
    else do
      let ok3 = check3 parsed
      if not ok3 then
        error "Error. Bounded variables present in the values"
      else do interpret parsed

--INTERPRETING
--State and out monad for our table
newtype M a = StOut (Table -> (a, Table))

instance Functor M where
  fmap f e = StOut (\t -> let (a, t1) = (unStOut e) t in
                          let (b, t2) = (f a, t1) in
                          (b, t2))

instance Applicative M where
  pure f = StOut (\t -> (f, t))
  e <*> f = StOut (\t -> let (a, t1) = (unStOut e) t in
                         let (b, t2) = (unStOut f) t1 in
                         (a b, t2))

instance Monad M where
  return x = StOut (\t -> (x, t))
  e >>= f = StOut (\t -> let (a, t1) = (unStOut e) t in
                         let (b, t2) = unStOut (f a) t1 in
                         (b, t2))

unStOut (StOut f) = f

--Interpret a line
interpret :: Line -> IO()
interpret (Apply values expr) = do
  let solvedValues = interpretValues values
  tables <- createTables expr
  if tables == []
    then myPrint[]
  else
    let solvedExpression = interpretExpression expr tables
    in myPrint$ mySort solvedValues solvedExpression
interpret (Multiple l1 l2) = do
  interpret l1
  interpret l2

--Create tables for each relation
createTables :: Exp -> IO [String]
createTables (And expr1 expr2) = do
  t1 <- createTables expr1
  t2 <- createTables expr2
  return (t1++t2)
createTables (Equals _ _) = return []
createTables (Exists _ expr2) = createTables expr2
createTables (Relation filePath _) = do
  content <- readFile (filePath ++ ".csv")
  return [content]

--Interpret a value
interpretValues :: Values -> Header
interpretValues (Var s) = [s]
interpretValues (Comma s v) = [s] ++ interpretValues v

--Interpret an expression
interpretExpression :: Exp -> [String] -> Table
interpretExpression expr s = table where
  (_, table) = (unStOut (interpretExp expr s)) (T [] [[]])
    where
      interpretExp :: Exp -> [String] -> M [String]
      interpretExp (And expr1 expr2) s = do
        s1 <- interpretExp expr1 s
        s2 <- interpretExp expr2 s1
        return s2
      interpretExp (Equals x (Var y)) s = do
        filterVariables x y
        return s
      interpretExp (Exists (Var x) expr) s = do
        interpretExp expr s
        deleteVariables x
        return s
      interpretExp (Relation _ values) (s:ss) = do
        let table = [mySplit x | x <- lines s]
        constructTable (T (interpretValues values) table)
        return ss

--Monadic functions
constructTable :: Table -> M ()
constructTable table = StOut (\t -> ((), mergeTable t table))

filterVariables :: String -> String -> M ()
filterVariables x y = StOut (\t -> ((), filterTable t x y))

deleteVariables :: String -> M ()
deleteVariables x = StOut (\t -> ((), deleteColumn t x))

--Helper functions for the monadic functions
filterTable :: Table -> String -> String -> Table
filterTable (T header content) x y = T header [a | a <- content, a!!(position x header) == a!!(position y header)]

deleteColumn :: Table -> String -> Table
deleteColumn (T header content) x = let c = position x header in
  T (delete x header) [take c y ++ drop (c+1) y | y <- content]

-- Merge 2 tables
mergeTable :: Table -> Table -> Table
mergeTable (T header1 content1) (T header2 content2) = (T header3 content3)
  where
    header3 = header1 ++ [h | h <- header2, not $ elem h header1]
    common = [h | h <- header2, elem h header1]
    pos1 = [position x header1 | x <- common]
    pos2 = [position x header2 | x <- common]
    content3 = [x ++  removeDuplicates y pos2 | x <- content1, y <- content2, equal (convert x) (convert y) pos1 pos2]
      where
        --Convert the type of data to be compatible with the equal function
        convert :: [String] -> [String]
        convert [] = []
        convert (x:xs) = (mySplit (removeSpaces2 x)) ++ convert xs
          where
            removeSpaces2 [] = []
            removeSpaces2 (' ':a) = removeSpaces2 a
            removeSpaces2 (a:b) = a : removeSpaces2 b

-- Return the position of a tuple in a list of tuples
position :: String -> Header -> Int
position n [] = 0
position n (x:xs) = if x==n then 0 else (position n xs) + 1

--Check if all the elements from 2 columns are equal
equal :: [String] -> [String] -> [Int] -> [Int] -> Bool
equal _ _ [] [] = True
equal x y (p1:pos1) (p2:pos2) = if x!!p1 == "\n" || y!!p2 == "\n" || x!!p1 == "" || y!!p2 == "" then False
                                else if x!!p1 == y!!p2 then equal x y pos1 pos2
                                     else False


--Remove duplicates from content
removeDuplicates :: Eq a => [a] -> [Int] -> [a]
removeDuplicates xs [] = xs
removeDuplicates xs (y:ys) = removeDuplicates (delete (xs!!y) xs) [y-1 | y <- ys]

--Get the content from a table
getContent :: Table -> Content
getContent (T _ x) = x

--Get the header from a table
getHeader :: Table -> Header
getHeader (T h c) = h

--Split on the , character
mySplit :: String -> [String]
mySplit [] = []
mySplit xs = splitOn "," xs

--Print the output in stdout
myPrint:: [String] -> IO ()
myPrint[] = return ()
myPrint(x:xs) = do
  putStrLn $ removeSpaces x
  myPrint xs

-- Format the output
removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (',':' ':a) = ", " ++ removeSpaces (a)
removeSpaces (',':a) = ", " ++ removeSpaces (a)
removeSpaces (a:b) = a : removeSpaces b

-- Print the header and contents of a table
printTable :: Table -> IO()
printTable (T header content) = putStrLn $ show $ header:content

--Sort in lexicographical order the contents from a table
mySort :: [String] -> Table -> [String]
mySort values (T header content) = let ordered = [order values header c | c <- content]
  in sort [intercalate "," x | x <- ordered]
    where
      order :: [String] -> [String] -> [String] -> [String]
      order [] _ _ = []
      order (x:xs) header content = [content!!(position x header)] ++ order xs header content

--ERROR CHECKING
--Functions for checking errors
check1 :: Line -> Bool
check1 (Apply values expr) = checkDuplicateValues (interpretValues values)
check1 (Multiple l1 l2) = check1 l1 && check1 l2

check2 :: Line -> Bool
check2 (Apply values expr) = compareLists (removeElements (getBoundedVariables expr) (interpretValues values)) (removeElements (getBoundedVariables expr) (getUnboundedVariables expr))
check2 (Multiple l1 l2) = check2 l1 && check2 l2

check3 :: Line -> Bool
check3 (Apply values expr) = differentLists (interpretValues values) (getBoundedVariables expr)
check3 (Multiple l1 l2) = check3 l1 && check3 l2

--Remove xs list from the ys list
removeElements :: [String] -> [String] -> [String]
removeElements [] ys = ys
removeElements (x:xs) ys = removeElements xs (filter (/=x) ys)

--Compare if 2 lists have the same elements
compareLists :: [String] -> [String] -> Bool
compareLists x y = sort x == sort y

--Check if 2 lists have different elements
differentLists :: [String] -> [String] -> Bool
differentLists [] _ = True
differentLists (x:xs) ys = not (elem x ys) && differentLists xs ys

--Check values
checkDuplicateValues :: Header -> Bool
checkDuplicateValues v = checkDuplicates v
  where
    checkDuplicates :: Header -> Bool
    checkDuplicates [] = True
    checkDuplicates (x:xs) = if elem x xs then False else checkDuplicates xs

--Get the unbounded variables
getUnboundedVariables :: Exp -> [String]
getUnboundedVariables (And expr1 expr2) = getUnboundedVariables expr1 ++ getUnboundedVariables expr2
getUnboundedVariables (Relation _ values) = interpretValues values
getUnboundedVariables (Equals x (Var y)) = [x, y]
getUnboundedVariables (Exists _ expr) = getUnboundedVariables expr

--Get the bounded variables
getBoundedVariables :: Exp -> [String]
getBoundedVariables (And expr1 expr2) = getBoundedVariables expr1 ++ getBoundedVariables expr2
getBoundedVariables (Relation _ _) = []
getBoundedVariables (Equals _ _) = []
getBoundedVariables (Exists x expr) = interpretValues x ++ getBoundedVariables expr
