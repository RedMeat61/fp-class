{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

-}
import Data.List.Split
import System.Environment
import Data.List
import System.IO

import System.Directory
import Data.Char
import Data.String


c1 fn (kf:gf:[]) = do
	contents <- readFile fn
	let sa = foldl (\ (acc, i) (fio:age:k:g) -> (acc + (read age),i+1))(0,0) $ filter(\(fio:age:k:g) -> ((k == kf) && (g == [gf]))) $ map (endBy ";" ) $ lines $ contents
	putStr $  show $ (fst sa) / (snd sa)
	
		

main = do
	fn:args <- getArgs
	case (head args) of
		"1" -> c1 fn (tail args)
		--"2" -> c2 (tail args)
		--"3" -> c3 (tail args)

