{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

import System.Environment
import Data.List
import System.IO
import System.Random
import System.Directory
import Data.Char
import Data.String


{-
c1rec fn n gen = if n > 0 then do
	appendFile "temp.txt" 
	
c5rec l s i gen = if i < l then do
		appendFile "temp.txt" $ map(\x -> chr x) $ take s $ randomRs (32,126) (mkStdGen gen)
		appendFile "temp.txt" "\n"
		c5rec l s (i+1) (gen+i)
	else
		appendFile "temp.txt" $ map(\x -> chr x) $ take s $ randomRs (32,126) (mkStdGen gen)
		-}
		
c1 (fn:n:[]) = do
	gen1 <- newStdGen
	gen2 <- newStdGen
	writeFile fn $ unlines $ map show $ zip (xy gen1) (xy gen2)
	where
		xy gen = take (read n) $ (randomRs (-100, 100) gen :: [Int])

c2 (fn:[]) = do
	contents <- readFile fn
	putStr $ show $ foldl foldFunc (0,0,0,0) $ map (\x -> read x :: (Int, Int)) $ lines $ contents
	where
		quart (x, y) 
			| ((x >= 0) && (y >= 0)) = 1
			| ((x < 0) && (y >= 0)) = 2
			| ((x < 0) && (y < 0)) = 3
			| ((x >= 0) && (y < 0)) = 4
		foldFunc (q1,q2,q3,q4) x = case quart x of 
			1 -> (q1+1,q2,q3,q4)
			2 -> (q1,q2+1,q3,q4)
			3 -> (q1,q2,q3+1,q4)
			4 -> (q1,q2,q3,q4+1)
c3 (fn:[]) = do
	contents <- readFile fn
	putStr $ show $ foldl1 foldFunc $ map (\x -> read x :: (Int, Int)) $ lines $ contents
	where
		dist (x1, y1) (x2, y2) = ((x1-x2)^2 + (y1-y2)^2)
		foldFunc acc x = if dist (0, 0) acc < dist (0, 0) x then x else acc
main = do
	args <- getArgs
	case (head args) of
		"1" -> c1 (tail args)
		"2" -> c2 (tail args)
		"3" -> c3 (tail args)