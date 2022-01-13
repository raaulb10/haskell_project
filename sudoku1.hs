{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Data.List (sortBy)
type Pos = (Int, Int)
type Cell = (Pos, Int)
type Sudoku = [Cell]

--returneaza numerele deja folosite in coloana curent
itemsInCol :: Sudoku -> Int -> [Int]
itemsInCol sudo n = [ i | ((x,y), i) <- sudo, y==n ]

--returneaza numerele deja folosite in randul curent
itemsInRow :: Sudoku -> Int -> [Int]
itemsInRow sudo n = [ i | ((x,y), i) <- sudo, x==n ]

--returneaza indexul blocului curent
indexofBlock :: Pos -> Int
indexofBlock (x,y) = x - x `mod` 3 + y `div` 3

--returneaza pozitiile dintr-un bloc
positionofBlock :: Int -> [Pos]
positionofBlock n
   | n `notElem` [0..8] = error ("Pozitie imposibila: " ++ show n)
   | otherwise = [ (x,y) | x <- [0..8], y <- [0..8], n == x - indexofBlock (x, y)]

--returneaza numerele deja folosite in bloc
numsUsedinBlock :: Sudoku -> Int -> [Int]
numsUsedinBlock sud n = [ i | ((x,y), i) <- sud, (j,k) <- positionofBlock n, (x,y) == (j,k)]

--verifica unicitatea elementelor din lista
isElementUnique :: Eq a => [a] -> Bool
isElementUnique [] = True
isElementUnique (x:xs)
  | x `elem` xs = False
  | otherwise = isElementUnique xs

--verificam validitatea unui sudoku input, daca tabla e de 9*9, toate elementele intre 1 si 8 si fara repetitii intre linii, coloane si blocuri
isInputValid :: Sudoku -> Bool
isInputValid sud = and [and [ x `elem` [0..8] && y `elem` [0..8] && z `elem` [1..9] | ((x,y), z) <- sud ] , and [ isElementUnique a | a <- [itemsInCol sud i | i <- [0..8] ]] , and [ isElementUnique a | a <- [itemsInRow sud i | i <- [0..8] ]] , and [ isElementUnique a | a <- [numsUsedinBlock sud i | i <- [0..8] ]]]

--verificam daca sudoku este plin
isFull :: Sudoku -> Bool
isFull sud = isElementUnique [ (x,y) | ((x,y), z) <- sud ] && length [ (x,y) | ((x,y), z) <- sud ] == 81

--verificam daca este finalizata rezolvarea (sa fie valid si sa fie plin)
isSudokuSolved :: Sudoku -> Bool
isSudokuSolved sud = isInputValid sud && isFull sud

--verificam daca pozitia este goala
isPositionEmpty :: Sudoku -> Pos -> Bool
isPositionEmpty sud (x,y) = (x,y) `notElem` [ (j,k) | ((j,k),l) <- sud ]

--returneaza toate pozitile goale din sudoku
emptyPositions :: Sudoku -> [Pos]
emptyPositions sud = [ (x,y) | x <- [0..8], y <- [0..8], isPositionEmpty sud (x,y) ]

--returneaza o lista de numere valide de pus in pozitia curenta
validNumsinPosition :: Sudoku -> Pos -> [Int]
validNumsinPosition sud (x,y)
   | isPositionEmpty sud (x,y) = [ i | i <- [1..9], i `notElem` itemsInRow sud x, i `notElem` itemsInCol sud y, i `notElem` numsUsedinBlock sud (indexofBlock (x,y)) ]
   | otherwise = []

--returneaza o lista care contine toate pozitiile goale si posibilele lor valori
validNumsinEmptyPositions :: Sudoku -> [(Pos, [Int])]
validNumsinEmptyPositions sud = [ ((x,y), validNumsinPosition sud (x,y)) | x <- [0..8], y <- [0..8], isPositionEmpty sud (x,y)]

--verificam daca un sudoku are solutie ( pozitie goala si valoare posibila)
isSolvable :: [(Pos, [Int])] -> Bool
isSolvable [] = False
isSolvable a = and [ not (null l) | ((j,k),l) <- a ]

--returneaza pozitiile goale pe care putem pune o singura valoare
emptyPositionswithUniqueVal :: [(Pos, [Int])] -> [(Pos, Int)]
emptyPositionswithUniqueVal a = [ ((j,k),head l) | ((j,k),l) <- a, length l == 1 ]

--inserare valoare
insertInto :: Sudoku -> Pos -> Int -> Sudoku
insertInto sud (x,y) n
   | isPositionEmpty sud (x,y) = ((x,y),n):sud
   | otherwise = error ("insertInto: position " ++ show (x,y) ++ " is not blank")


solutionstep :: Sudoku -> [Sudoku]
solutionstep sud
   | isSudokuSolved sud = [sud]
   | isSolvable (validNumsinEmptyPositions sud) && not (null (emptyPositionswithUniqueVal (validNumsinEmptyPositions sud))) = [ uncurry (insertInto sud) (head (emptyPositionswithUniqueVal (validNumsinEmptyPositions sud))) ]
   | isSolvable (validNumsinEmptyPositions sud) && null (emptyPositionswithUniqueVal (validNumsinEmptyPositions sud)) = [ insertInto sud (head (emptyPositions sud)) x | x <- validNumsinPosition sud (head (emptyPositions sud)) ]
   | not (isSolvable (validNumsinEmptyPositions sud)) = []
-- sudokusolved
-- verificam pozitiile cu goale ce pot avea doar o valoare
-- generam o lista de sudoku cu toate valorile valide


sudokuSolver :: Sudoku -> [Sudoku]
sudokuSolver sud
    | not (isInputValid sud) = error "solve: improper sudoku"
    | otherwise =
     until done f l
       where
         l = return sud
         f (x:xs) = f xs ++ solutionstep x
         f [] = []
         done m = all isSudokuSolved m && all isInputValid m


input :: Sudoku
input = [ ((0,1),2),((0,3),5),((0,5),1),((0,7),9),
          ((1,0),8),((1,3),2),((1,5),3),((1,8),6),
          ((2,1),3),((2,4),6),((2,7),7),
          ((3,2),1),((3,6),6),
          ((4,0),5),((4,1),4),((4,7),1),((4,8),9),
          ((5,2),2),((5,6),7),
          ((6,1),9),((6,4),3),((6,7),8),
          ((7,0),2),((7,3),8),((7,5),4),((7,8),7),
          ((8,1),1),((8,3),9),((8,5),7),((8,7),6)]


--ordonare sudoku
matrixcmp :: (Ord a, Ord b1) => ((a, b1), b2) -> ((a, b1), b3) -> Ordering
matrixcmp a b =
   if fst (fst a) == fst (fst b) then
      compare (snd (fst a)) (snd (fst b))
   else
      compare (fst (fst a)) (fst (fst b))




sudoku2 :: [((Int, Int), Int)]
sudoku2 = sortBy matrixcmp (head(sudokuSolver input)++input)

sortSudoku:: Sudoku
sortSudoku = sortBy matrixcmp (head(sudokuSolver input))

--print nice '''''''to continue''''''
printNice :: Show a1 => [(a2, a1)] -> IO ()
printNice  [] = print( )
printNice (x:xs) =do
 print (snd x)
 printNice xs




main :: IO ()
main = do
      --print sudoku2
      --printNice sortSudoku
      let puzzle_input = sortSudoku
      print puzzle_input