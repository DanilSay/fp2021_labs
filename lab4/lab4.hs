-- Лабораторна робота №4
-- студента групи КН-32/1
-- Борисюка Даніла
-- Варіант №5

-- Мета: Ознайомитись з системою типiв та класiв типiв. Набути досвiду визначення нових типiв та класiв типiв i їх використання.

--Фiгури на площинi. Використовуються такi фiгури, як коло (центр та ра-
--дiус), прямокутник (координати лiвої верхньої та правої нижньої точок), три-
--кутник (координати вершин) та мiтка — label (координати лiвої нижньої точки,
--шрифт та рядок). Доступнi шрифти — Consolas, Lucida Console та Source Code
--Pro. Визначне функцiї для:
-- отримання прямокутникiв, що охоплюють кожну фiгуру iз заданого списку;

data Figures
    = Circle (Integer, Integer) Integer
  | Triangle (Integer, Integer) (Integer, Integer) (Integer, Integer)
  | RectangleOfCircle (Integer, Integer) (Integer, Integer)
  | RectangleOfTriangle (Integer, Integer) (Integer, Integer)
  | Label (Integer, Integer) Fonts String
  deriving (Eq, Show)
  
  
data Fonts = Consolas
    | SourceCodePro
    | LucidaConsole
    deriving (Eq, Show)

getRectangles :: [Figures] -> [Figures]
getRectangles [] = []
getRectangles ((Circle (x1, y1) n) : fs) = Circle (x1, y1) n : getRectangles fs
getRectangles ((Triangle (x1, y1) (x2, y2) (x3, y3)) : fs) = Triangle (x1, y1) (x2, y2) (x3, y3) : getRectangles fs
getRectangles ((RectangleOfCircle (x1, y1) (x2, y2)) : fs) = RectangleOfCircle (x1, y1) (x2, y2) : getRectangles fs
getRectangles ((RectangleOfTriangle (x1, y1) (x2, y2)) : fs) = RectangleOfTriangle (x1, y1) (x2, y2) : getRectangles fs
getRectangles ((Label (x1, y1) fnt str) : fs) = Label (x1, y1) fnt str : getRectangles fs


--getRectangles [(Circle (1, 4) 10),(RectangleOfCircle (2, 2) (3, 3)), (RectangleOfTriangle (2, 2) (3, 3)), (Triangle (1, 2) (3, 4) (5,7)), (Label (3, 3) Consolas "labslabs")]
--[Circle (1,4) 10,RectangleOfCircle (2,2) (3,3),RectangleOfTriangle (2,2) (3,3),Triangle (1,2) (3,4) (5,7),Label (3,3) Consolas "labslabs"]
-- Висновок: ознайомився з системою типiв та класiв типiв. Набув досвiду визначення нових типiв та класiв типiв i їх використання.
