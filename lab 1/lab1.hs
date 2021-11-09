-- Лабораторна робота №1
-- студента групи КН-32/1
-- Борисюка Даніла
-- Варіант №5

-- Мета: Ознайомитись з основними типами мови. Ознайомитись зi структурою та функцiями Glasgow Haskell Compiller. Набути навичок роботи з iнтерпретатором ghci та визначення найпростiших функцiй.

-- Завдання 1. Наведiть приклади виразiв вказаного типу ([Char], [Double], [(Bool, Integer)]). Кожен список має мiстити кiлька елементiв.
-- (['a'], 0.5, [(True, 10)])
-- (['b'], 1.5, [(False, 15)])
-- (['c'], 2.5, [(True, 20)])

-- Завдання 2. Визначте два варiанти вказаних далi функцiй, де аргументи будуть представленi а) як один кортеж, б) без використання кортежiв чи спискiв.
-- Функцiя визначає, чи належить кругу дана точка. Точка задається координатами, круг – координатами центра та радiусом.

module Main where

 main = do
    --print (f1((0 :: Float),(0 :: Float),(2 :: Float),(1 :: Float),(1 :: Float)))
    --print (f2 0.0 0.0 2.0 1.0 1.0)
    --print (f1((0 :: Float),(0 :: Float),(2 :: Float),(2 :: Float),(2 :: Float)))
    print (f2 0.0 0.0 2.0 2.0 2.0)
    
 f1 :: (Float, Float, Float, Float, Float) -> Bool
 f1 (x, y, r, x1, y1) = sqrt((x1-x)*(x1-x)+(y1-y)*(y1-y)) <= r

 f2 :: Float -> Float -> Float -> Float -> Float -> Bool
 f2 x y r x1 y1 = sqrt((x1-x)*(x1-x)+(y1-y)*(y1-y)) <= r

-- Висновок: ознайомилися з основними типами мови Haskell, отримали навички роботи з інтерпритатором та навчились працювати з найпростішими функціями.