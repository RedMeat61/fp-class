{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны передаваться с помощью параметров
  командной строки.
-}

import System.Environment
import Data.List
import System.IO
import System.Directory
import Data.Char
import Data.String
import System.Random

c1 (x:xs) = do
	withFile x ReadMode procF1 

procF1 handle = do
	contents <- hGetContents handle
	putStr (show (length $ lines contents))	

{-fn str b/e-}
c2 (fn:str:bn:[]) = do
	if bn == "e" then
		appendFile fn str
	else do 
		contents <- readFile fn
		writeFile "temp.txt" $ unlines $ str : (lines contents)
		renameFile "temp.txt" fn
	

c3 (fn:xs) = do
	contents <- readFile fn
	putStr(map toUpper contents) 

c4 (f1:f2:xs) = do
	co1 <- readFile f1
	co2 <- readFile f2
	writeFile "02_4.txt" $ unlines $ foldl(\acc x -> acc ++ [(fst x)] ++ [(snd x)])[] $ zip (lines co1) (lines co2)

c5rec l s i gen = if i < l then do
		appendFile "temp.txt" $ map(\x -> chr x) $ take s $ randomRs (32,126) (mkStdGen gen)
		appendFile "temp.txt" "\n"
		c5rec l s (i+1) (gen+i)
	else
		appendFile "temp.txt" $ map(\x -> chr x) $ take s $ randomRs (32,126) (mkStdGen gen)
		
		
c5 (fn:l:s:gen:xs) = do
	c5rec (read l) (read s - 1) 1 (read gen) 
	renameFile "temp.txt" fn
main = do
  args <- getArgs
  case (head args) of
	"1" -> c1 (tail args)
  	"2" -> c2 (tail args)
	"3" -> c3 (tail args)
	"4" -> c4 (tail args)
	"5" -> c5 (tail args)
 {- [n_str, text, fname] <- getArgs
  createFile (read n_str) text fname
-}