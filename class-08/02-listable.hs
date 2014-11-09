{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}
{-
   Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}
class Listable a where
   toList :: a -> [a]
   fromList :: [a] -> a

{-
  Объявите экземпляры класса типов Listable для следующих типов:
  1) String - строка разбивается по пробелам на список слов.
  2) Integer - любое целое число разбивается на список цифр.
-}
instance Listable String where
	toList x = words x
	fromList x = unwords x 
	
instance Listable Integer where
	toList x = []++(reverse(pise x))
		where 
			pise x = if (x >= 10) then ([(x `mod` 10)] ++ pise (x `div` 10))
				else x
	fromList x = p x
		where 
			p x = head x + 10*(p x)