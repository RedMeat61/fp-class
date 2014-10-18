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


{-
createFile :: Int -> String -> FilePath -> IO ()
createFile n s fname = writeFile "file.txt" (unwords(replicate n s))
-}

{-
nLine i = do
     line <- getLine 
     if null line
        then putStr "i"
        else (nLine (i+1))
-}	
c1 (x:xs) = do
	withFile x ReadMode procF1 

procF1 handle = do
	contents <- hGetContents handle
	putStr (show (length $ lines contents))	

c2 (x:xs) = do
	writeFile x "666"
procF2 handle = do
	contents <- hGetContents handle
	putStr (show (length $ lines contents))	

main = do
  args <- getArgs
  case (head args) of
	"1" -> c1 (tail args)
  	"2" -> c2 (tail args)
 {- [n_str, text, fname] <- getArgs
  createFile (read n_str) text fname
-}
