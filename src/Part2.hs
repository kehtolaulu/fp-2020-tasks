module Part2 where

import Part2.Types
------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 x = 0 <= colorPartToInt x && colorPartToInt x <= 255

colorPartToInt :: ColorPart -> Int
colorPartToInt (Red x) = x
colorPartToInt (Green x) = x
colorPartToInt (Blue x) = x

------------------------------------------------------------
-- PROBLEM #8
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 (Color r g b) (Red dr)   = Color (r + dr) g b
prob8 (Color r g b) (Green dg) = Color r (g + dg) b
prob8 (Color r g b) (Blue db)  = Color r g (b + db)

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 = colorPartToInt

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color = if length maximums == 1 then Just maxValue else Nothing
  where
    colors = decompose color
    maxValue = maxBy colorPartToInt colors
    maximums = filter (== colorPartToInt maxValue) (map colorPartToInt colors)

maxBy :: Ord a => (t -> a) -> [t] -> t
maxBy f (x:xs) = iter x xs
  where
    iter acc [] = acc
    iter acc (x:xs) = if f acc < f x then iter x xs else iter acc xs

decompose :: Color -> [ColorPart]
decompose (Color r g b) = [Red r, Green g, Blue b]

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 = sum

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 (Tree leftTree value rightTree)
    = allCorrect [
        fmap (all (< value)) leftTree
      , fmap (all (> value)) rightTree
      , fmap prob12 leftTree
      , fmap prob12 rightTree
      ]
  where
    allCorrect [] = True
    allCorrect (Nothing:xs) = allCorrect xs
    allCorrect ((Just b):xs) = b && allCorrect xs

instance Foldable Tree where
  foldMap f (Tree (Just leftTree) value (Just rightTree)) = foldMap f leftTree `mappend` f value `mappend` foldMap f rightTree
  foldMap f (Tree (Just leftTree) value Nothing) = foldMap f leftTree `mappend` f value
  foldMap f (Tree Nothing value (Just rightTree)) = f value `mappend` foldMap f rightTree
  foldMap f (Tree Nothing value Nothing) = f value

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 tree = result
  where
    (_, Just result) = helper 1 (Just tree)
    helper no Nothing = (no, Nothing)
    helper no (Just (Tree l () r)) = (li + 1, Just (Tree l' li r'))
      where
        (ri, r') = helper no r
        (li, l') = helper ri l


------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 (Tree a b (Just (Tree c d e))) = Tree (Just $ Tree a b c) d e

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 (Tree (Just (Tree a b c)) d e) = Tree a b (Just $ Tree c d e)

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
