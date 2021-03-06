-- Лабораторна робота №3
-- студента групи КН-32/1
-- Борисюка Даніла
-- Варіант №5

-- Мета: Набути досвiду визначення та використання функцiй вищого порядку.

--Визначити довжину послiдовностi тотожних елементiв списку, напр.:
--"aaabbcaadddd"⇒ [(’a’,3), (’b’,2), (’c’,1), (’a’,2), (’d’,4)].

--Main> funk_main "addaa"
--[('a',1),('d',2),('a',2)]
--Main> funk_main "addaadds"
--[('a',1),('d',2),('a',2),('d',2),('s',1)]
import Data.Text.Internal.Read

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

--б)
funk1_2 :: String -> [(Char,Int)]
funk1_2 [] = []
funk1_2 (x:y:ls) = (y,digitToInt x) : funk1_2 ls


--Знайти найбiльший спiльний дiльник двох чисел.
--а)
--Main> funk2_1 0 3
--3
--Main> funk2_1 1 3
--1
--Main> funk2_1 15 10
--5
--Main> funk2_1 15 6
--3
--Main> funk2_1 15 30
--15

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

--б)
-- gcd 12 8
-- 4
-- Висновок: набути досвiду визначення та використання функцiй вищого порядку.
