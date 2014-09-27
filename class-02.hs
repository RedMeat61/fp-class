-- 1.1
-- Написать функцию, которая разбивает промежуток времени в секундах на часы, минуты и секунды.
-- Результат возвращать в виде кортежа из трёх элементов. Реализовать также обратное преобразование.
sec2hms :: Int -> (Int, Int, Int)
sec2hms s = ((s `div` 3600), (s `div` 60 `mod` 60), (s `mod` 60))

hms2sec :: (Int, Int, Int) -> Int
hms2sec (h, m, s) = h * 3600 + m * 60 + s

-- Реализовать с помощью hms2sec (здесь параметры заданы по отдельности)
hms2sec' :: Int -> Int -> Int -> Int
hms2sec' h m s = hms2sec (h, m, s)

-- должно быть True
test1 = and $ map (\x -> x == hms2sec (sec2hms x)) [1,10..10000]

-- 1.2
-- Написать функции, вычисляющие
-- а) длину отрезка по координатам его концов;
-- б) периметр и площадь треугольника по координатам вершин.

type Point = (Double, Double)

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x2-x1)^2+(y2-y1)^2)

triangle :: Point -> Point -> Point  -> (Double, Double)
triangle (x1, y1) (x2, y2) (x3, y3) = (p, s)
  where
    ab = distance (x1, y1) (x2, y2)
    bc = distance (x2, y2) (x3, y3)
    ca = distance (x3, y3) (x1, y1)
    p = ab + bc + ca
    s = sqrt(p/2*(p/2-ab)*(p/2-bc)*(p/2-ca))

-- Во всех следующих заданиях использование стандартных функций обработки списков не допускается.
-- Все решения должны реализовываться рекурсивными функциями.

-- 2.1
-- Определить рекурсивную функцию, определяющую количество чётных элементов списка
nEven :: Integral a => [a] -> Int
nEven [] = 0
nEven (x:xs) 
	| mod x 2 == 0 = 1 + nEven(xs) 
	| otherwise = nEven(xs)

-- 2.2
-- Увеличить все элементы заданного списка в два раза.
-- Указание: в решении может понадобиться операция конструирования списка:
-- > 1 : [2,3,4]
--   [1,2,3,4]
doubleElems :: Num a => [a] -> [a]
doubleElems [] = []
doubleElems (x:xs) = 2*x:doubleElems (xs)

-- 2.3
-- Дан список целых чисел. Сформировать новый список, содержащий только нечетные элементы исходного.
fltOdd :: Integral a => [a] -> [a]
fltOdd [] = []
fltOdd (x:xs) 
	| x `mod` 2 == 1 = x:fltOdd (xs)
	| otherwise = fltOdd (xs)

-- 2.4
-- Написать следующие функции обработки списков:
-- а) удалить все отрицательные элементы;
delSubZero :: Integral a => [a] -> [a]
delSubZero [] = []
delSubZero (x:xs) 
	| x < 0 = delSubZero xs
	| otherwise = x:delSubZero xs

-- б) увеличить элементы с чётными значениями в два раза;
doubleEven :: Integral a => [a] -> [a]
doubleEven [] = [] 
doubleEven (x:xs)
	| x `mod` 2 == 0 = 2*x:doubleEven xs
	| otherwise = x:doubleEven xs
-- в) переставить местами чётные и нечётные по порядку следования элементы
--    (для списков нечётной длины отбрасывать последний элемент).
head1 (x:xs) = x
tail1 (x:xs) = xs
sortEvenUneven :: Integral a => [a] -> [a]
sortEvenUneven [] = []
sortEvenUneven [_] = []
sortEvenUneven (x:xs) =  head1 xs : x : sortEvenUneven (tail1 xs)

-- 2.5 
-- Даны два списка целых чисел. Сформировать список, каждый элемент которого равен сумме
-- соответствующих   элементов исходных списков. Предусмотреть ситуацию списков разной длины.
combine_plus :: [Integer] -> [Integer] -> [Integer]
combine_plus [] ys = ys
combine_plus xs [] = xs
combine_plus (x:xs) (y:ys) = (x+y):(combine_plus xs ys)

-- 2.6
-- Даны два списка. Сформировать новый список, содержащий пары из соответствующих элементов
-- исходных списков. Хвост более длинного списка отбросить.
zip' :: [a] -> [b] -> [(a, b)]
zip' [ ] _ = [ ]
zip' _ [ ] = [ ]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
-- 2.7
-- Написать функции, которые по заданному n возвращают список, состоящий из n первых натуральных чисел
-- а) в порядке убывания;
nDescending :: (Num a, Enum a) => a -> [a]
nDescending n = [1..n]
-- б) в порядке возрастания.
nAscending :: (Num a, Enum a, Eq a) => a -> [a]
nAscending 0 = []
nAscending n = n:(nAscending (n-1))
-- 2.8
-- Дан элемент типа a и список [a]. Вставить между всеми элементами списка заданный элемент.
insertEverywhere :: a -> [a] -> [a]
insertEverywhere a [] = []
insertEverywhere a [x] = [x]
insertEverywhere a (x:y:xs) = x : a : (insertEverywhere a (y:xs))
-- 2.9
-- Написать функцию, которая разбивает список на два подсписка: элементы из начала списка,
-- совпадающие с первым элементом, и все остальные элементы, например:
-- [1,1,1,2,3,1] -> ([1,1,1], [2,3,1]).
numberOfFirstAndOther (x:xs) = (first x (x:xs), other x (x:xs))
	where
		first y [] = []
		first y (x:xs) 
			|y == x = x:first x xs
			|otherwise = []
		other y [] = []
		other y (x:xs) 
			|y == x = other x xs
			|otherwise = (x:xs)
			
--3
-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают, и напишите их
-- рекурсивные реализации (если вы можете предложить несколько вариантов, реализуйте все):
-- а) [a] -> Int -> a
index :: [a] -> Int -> a
index (x:xs) i 
	|i == 0 = x
	|i >= 0 = index xs (i-1)
-- б) Eq a => [a] -> a -> Bool
find :: Eq a => [a] -> a -> Bool
find [] _ = False
find (x:xs) a
	|x==a = True
	|x/=a = find xs a
	
-- в) [a] -> Int -> [a]
subList :: [a] -> Int -> [a]
subList [] _ = []
subList _ 0 = []
subList (x:xs) n = x:subList xs (n-1)

multiList :: [a] -> Int -> [a]
multiList [] _ = []
multiList _ 0 = []
multiList x n = x++multiList x (n-1)
-- г) a -> Int -> [a]
createList :: a -> Int -> [a]
createList a 0 = []
createList a 1 = [a]
createList a n = [a]++createList a (n-1)
-- д) [a] -> [a] -> [a]
concatenatio :: [a] -> [a] -> [a]
concatenatio a b = a++b

merge :: [a] -> [a] -> [a]
merge [] [] = []
merge a [] = a
merge [] a = a
merge (x:xs) (y:ys) = x:y:merge xs ys 
-- е) Eq a => [a] -> [[a]]
listOfList :: Eq a => [a] -> [[a]]
listOfList [] = []
listOfList (x:xs) = addList x (listOfList xs)
	where
		addList x [] = [[x]]
		addList x (e:es)
			|find e x = (x:e):es
			|otherwise = e:(addList x es)
	

	
-- ж) [a] -> [(Int, a)]
enumerateList :: [a] -> [(Int, a)]
enumerateList [] = []
enumerateList xs = enumerater xs 0
  where
    enumerater :: [a] -> Int -> [(Int, a)]
    enumerater [] n = []
    enumerater (x:xs) n = (n, x) : enumerater xs (n+1)
-- з) Eq a => [a] -> [a]
listOfUnique :: Eq a => [a] -> [a]
listOfUnique [] = []
listOfUnique (x:xs)
	|find xs x = listOfUnique xs
	|otherwise = x:(listOfUnique xs)
