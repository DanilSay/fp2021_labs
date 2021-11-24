-- Лабораторна робота №3
-- студента групи КН-32/1
-- Борисюка Даніла
-- Варіант №5

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму зiставлення зi зразком i роботи з кортежами та списками.

--
import System.Environment (getArgs)
import Data.Char
import Data.List


--Знайти найбiльший спiльний дiльник двох чисел.
--a)
gcd' :: Int -> Int -> Int
gcd' 0 y = abs y
gcd' x 0 = abs x
gcd' x y = case divisors a b of
    [] -> -1
    (x:y) -> x
  where a = abs x
        b = abs y
        
divisors :: Int -> Int -> [Int]
divisors x y = filter (\ z -> ((mod x z) + (mod y z)) == 0)  [a, b..1]
  where a = min x y
        b = if (a - 1) < 0 then 0 else (a - 1)

--b)
-- gcd 12 8
--4
-- Висновок: набув досвiду визначення рекурсивних функцiй, використання механiзму зiставлення зi зразком i роботи з кортежами та списками.
