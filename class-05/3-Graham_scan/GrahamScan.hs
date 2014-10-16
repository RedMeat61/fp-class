
{-# LANGUAGE EmptyDataDecls #-}

module GrahamScan where
import Data.List (sortBy, minimumBy, delete)
-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point Double Double
	deriving(Show, Eq, Ord)

{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = Le|Ri|Li
	deriving(Show, Eq, Ord)

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [b, c, d] и [c, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

split n k xs = res
	where
		(res,_,_,_) = foldl(\(ws, wn, wk, i) x -> if i < n then (if i < (n-k) then (ws,wn++[x],[],i+1) else (ws, wn++[x], wk++[x], i+1)) else ((ws ++ [wn]), wk++[x],  (drop (k-(n-k)) wk) ++[x] , k+1))([],[],[],0) xs 

space (Point x1 y1) (Point x2 y2) (Point x3 y3) = 0.5*((x2-x1)*(y3-y1) - (y2-y1)*(x3-x1))
p2d [a,b,c] 
	|space a b c > 0 = Le
	|space a b c < 0 = Ri
	|otherwise = Li
directions :: [Point] -> [Direction]
directions xs = map p2d (split 3 2 xs)

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

graham_scan :: [Point] -> [Point]
graham_scan = undefined

{-
  5. Приведите несколько примеров работы функции graham_scan.
-}
compareBL (Point ax ay) (Point bx by) 
	| ay > by = GT
	| ay < by = LT
	| ay == by = compare ax bx
findP0 pts = minimumBy compareBL pts
