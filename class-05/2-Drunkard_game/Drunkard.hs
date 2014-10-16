{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Clubs | Diamonds | Hearts
	deriving (Show, Eq, Ord)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
	deriving (Show, Eq, Ord)

data Card = Card Value Suit
	deriving (Show, Eq, Ord)
-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card v1 s1) (Card v2 s2) = s1==s2

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}
cardValue ::Card -> Value
cardValue (Card v1 s1)= v1

beats :: Card -> Card -> Ordering
c1 `beats` c2 = compare (cardValue c1) (cardValue c2) 

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры): 
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

game_round_store :: ([Card], [Card]) -> [Card] -> ([Card], [Card])
game_round_store ([],[]) ls = (ls,[])
game_round_store ((h1:t1),(h2:t2)) ls 
	|h1 `beats` h2 == LT = (t1,t2++ls++[h1,h2]) 
	|h1 `beats` h2 == GT = (t1++ls++[h1,h2],t2)
	|otherwise = game_round_store (t1,t2) (ls++[h1,h2])
 
game_round :: ([Card], [Card]) -> ([Card], [Card])
game_round (p1,p2) = game_round_store (p1,p2) []

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second
	deriving (Show, Eq)

game :: ([Card], [Card]) -> (Winner, Int)
game (p1,p2) = aux_game ((p1,p2), 0)
  where
    aux_game (([], p2), n) = (Second, n)
    aux_game ((p1, []), n) = (First, n)
    aux_game ((p1, p2), n) = aux_game $ (game_round (p1, p2), n+1)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

	               
p11 = [Card Eight Hearts,Card Seven Diamonds,Card King Hearts,Card Queen Spades,Card Eight Spades,Card Queen Diamonds,Card Nine Clubs,Card Ten Diamonds,Card Jack Hearts,Card Queen Hearts]


p12 = [Card Seven Clubs,Card Nine Diamonds,Card King Clubs,Card Ace Spades,Card Nine Spades,Card King Diamonds,Card Eight Diamonds,Card Jack Diamonds,Card Jack Clubs,Card King Spades]

test1 = game (p11,p12) == (Second,14)

p21 = [Card Ace Spades,Card Queen Hearts,Card Eight Hearts,Card Jack Clubs,Card King Diamonds,Card Nine Spades,Card Ten Diamonds,Card Seven Clubs,Card Queen Diamonds,Card Ten Hearts]
p22 = [Card King Hearts,Card Eight Diamonds,Card Seven Spades,Card Ten Clubs,Card Queen Spades,Card Nine Diamonds,Card Seven Diamonds,Card Seven Hearts ,Card Jack Diamonds, Card Nine Clubs]

test2 = game (p21,p22) == (First,8)

p31 = [Card Queen Hearts,Card Seven Hearts,Card Eight Clubs,Card King Spades,Card Eight Diamonds,Card Queen Diamonds,Card King Hearts,Card Nine Hearts,Card Ace Hearts,Card Ten Hearts]

p32 = [Card Jack Clubs,Card Seven Diamonds,Card Seven Clubs,Card King Spades,Card Seven Spades,Card Jack Diamonds,Card Jack Hearts,Card Nine Diamonds,Card Ace Diamonds,Card Nine Spades]

test3 = game (p31,p32) == (First,6)

p41 = [Card Queen Hearts,Card Seven Hearts,Card Eight Clubs,Card King Spades,Card Eight Diamonds,Card Queen Diamonds,Card King Hearts,Card Nine Hearts,Card Ace Hearts,Card Ten Hearts]	                

p42 = [Card Jack Clubs,Card Eight Spades,Card Seven Clubs,Card King Spades,Card Seven Spades,Card Jack Diamonds,Card Jack Hearts,Card Nine Diamonds,Card Ace Diamonds,Card Nine Spades]

test4 = game ( p41,p42) == (First,9)
	          
p51 = [Card Queen Clubs,Card Seven Hearts,Card Eight Clubs,Card King Spades,Card Eight Diamonds,Card Queen Diamonds,Card King Hearts,Card Nine Hearts,Card Ace Hearts,Card Ten Hearts]

p52 = [Card Queen Hearts,Card Seven Diamonds,Card Seven Clubs,Card King Spades,Card Seven Spades,Card Jack Diamonds,Card Jack Hearts,Card Nine Diamonds,Card Ace Diamonds,Card Nine Spades]					  						  
			  
test5 = game ( p51,p52) == (First,5)	

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}
