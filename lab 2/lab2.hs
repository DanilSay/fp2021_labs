-- Лабораторна робота №2
-- студента групи КН-32/1
-- Борисюка Даніла
-- Варіант №5

-- Мета: Набути досвiду визначення рекурсивних функцiй, використання механiзму зiставлення зi зразком i роботи з кортежами та списками.

-- Завдання 1. Визначте вказанi функцiї в кожному з завдань: а) без застосування, б) з застосуванням вбудованих функцiй.
-- Чи є список палiндромом?

--а)
module Main where

 main = do
    print (isPalindrome ([1,2,1]))

 isPalindrome []  = True
 isPalindrome [_] = True
 isPalindrome xs  = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

--б)
module Main where

 main = do
    print (isPalindrome ([1,2,1,2]))

 isPalindrome :: (Eq a) => [a] -> Bool
 isPalindrome xs = xs == (reverse xs)

-- Завдання 2. Об’єднання зi змiшуванням двох спискiв довжиною n1 та n2. Вихiдний список має довжину 2 ∗ n, де n = min(n1, n2). Наприклад "abcde"та "123"перетворюються на "a1b2c3"

--а)
module Main where

 main = do
    print (merge [1,2,1,2] [1,2,3,4])

 merge xs ys = concatMap (\(x,y) -> [x,y]) (zip xs ys)

--б)
module Main where

 main = do
    print (merge [1,2,1,2] [1,2,3,4])

 merge [] ys = ys
 merge (x:xs) ys = x:merge ys xs

-- Висновок: набув досвiду визначення рекурсивних функцiй, використання механiзму зiставлення зi зразком i роботи з кортежами та списками.
