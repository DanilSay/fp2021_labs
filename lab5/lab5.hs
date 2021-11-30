-- Лабораторна робота №5
-- студента групи КН-32/1
-- Борисюка Даніла
-- Варіант №5

-- Мета: ознайомитись з модульною органiзацiєю програм та засобами введення-виведення. Набути досвiду компiляцiї Haskell-програм.

--Визначити довжину послiдовностi тотожних елементiв списку, напр.:
--"aaabbcaadddd"⇒ [(’a’,3), (’b’,2), (’c’,1), (’a’,2), (’d’,4)].

import Data.Text.Internal.Read
import System.Directory.Internal.Prelude (getArgs)

funk_main :: [Char] -> [(Char, Int)]
funk_main [] = []
funk_main ctr = funk1 ctr [] '`' 0

--а)
funk1 :: [Char] -> [(Char, Int)] -> Char -> Int -> [(Char, Int)]
funk1 str newStr var amount
    | var == '`' = funk1 (tail str) newStr (head str) (amount + 1)
    | length str == 0 = newStr ++ [(var, amount)]
    | var == head str = funk1 (tail str) newStr var (amount + 1)
    | otherwise       =  funk1 (tail str) (newStr ++ [(var, amount)]) (head str) 1

funk1_2 :: String -> [(Char,Int)]
funk1_2 [] = []
funk1_2 (x:y:ls) = (y,digitToInt x) : funk1_2 ls

--Знайти найбiльший спiльний дiльник двох чисел.

funk2_1 :: Integer -> Integer -> Integer
funk2_1 0 b = b
funk2_1 a b = (cost_mod a b)

cost_mod :: Integer -> Integer -> Integer
cost_mod a b =
    if a > b
        then helper a b
        else helper b a
helper a b = if a > b
    then helper (a - b) b
    else a


main :: IO ()
main = do
  --а) введення даних з клавiатури
  arg1 <- getArgs
  --б) введення даних з файлу
  contents <- readFile "/Users/DanilSay/Documents/GitHub/fp2021_labs/lab5/file.txt"
  let line1 = funk1_2 contents
  --в) виведення результатів на екран
  print (funk1_2 (head arg1))
  --г) виведення результатів у файл
  writeFile "result.txt" (concatMap show (funk1_2 (head arg1)))

-- Висновок: ознайомився з модульною органiзацiєю програм та засобами введення-виведення. Набути досвiду компiляцiї Haskell-програм.
