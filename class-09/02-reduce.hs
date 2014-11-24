import System.Environment

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce x 
	|x `mod` 3 == 0 = 0
	|odd x = x^2
	|otherwise = x^3

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF 0 x = x
reduceNF n x = reduceNF (n-1) (fmap reduce x)

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList [] = []
toList xs = fmap (\(x,y)->(x^2+y^2)) xs

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe [] = Nothing
toMaybe xs =  fmap (sum . toList) (Just xs)

toEither :: Integral a => [(a, a)]  -> Either String a
toMaybe [] = Left "[]"
toMaybe xs =  fmap (sum . toList) (Right xs)

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO = undefined

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из 
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs = undefined

readData :: FilePath -> IO [(Int, Int)]
readData = undefined

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  undefined
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
