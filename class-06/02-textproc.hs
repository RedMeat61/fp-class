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
processFile handle = nLine 0
    

nLine i = do
     line <- getLine 
     if null line
        then putStr "i"
        else (nLine (i+1))
	
first (x:xs) = do
	withFile x ReadMode processFile 
	nLine 0


main = do
  args <- getArgs
  first (tail args)
 {- [n_str, text, fname] <- getArgs
  createFile (read n_str) text fname
-}
