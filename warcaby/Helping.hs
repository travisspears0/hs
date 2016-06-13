module Helping where

import Data.Char
import Data.List.Split
import System.Random
import System.Process

--te funkcje powinny zwracac losowe wartosci ale cos im chyba nie bardzo wychodzi, nie dokonca wiem czemu :/
rnd n _min _max = fst (randomR (_min,_max) (mkStdGen n))
getRandomElement list n = list!!(rnd n 0 (length list - 1))

--dane a propo planszy sa zapisywane w 2 tablicach, ktore reprezentuja polozenie pionkow np [('a',1)...]
--testowe plansze
w:: [(Char,Int)]
b:: [(Char,Int)]
ww:: [(Char,Int)]
bb:: [(Char,Int)]
w = (zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3])
b = (zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6])

ww = (('f',5):(zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3]))
bb = removePaw (removePaw ((('c',4):('g',4):(zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6]))) 'f' 7) 'g' 6

--konwertuje zapis 1..15 do ('x',n)
getFieldByCode n = getFieldByCode' (ceiling(n)*2 - ((ceiling(n/4)-1) `mod` 2))
getFieldByCode' n
	| m == 0 = ('h',floor(fromIntegral(n)/8))
	| otherwise = (chr(ord 'a'-1+m),floor(fromIntegral(n)/8)+1)
	where m = n `mod` 8

--zwraca przeciwny kierunek
oppositeDirection dir
	| dir == UP = DOWN
	| otherwise = UP

--zwraca pozycje pionka o zadanych sgn num w tablicy 
indexOfPaw paws sgn num = indexOfPaw' paws sgn num 0
indexOfPaw' paws sgn num index
	| index >= length paws = -1
	| fst p == sgn && snd p == num = index
	| otherwise = indexOfPaw' paws sgn num (index+1)
	where p = paws!!index

--sprawdza, czy pole jest zajete
isFieldBusy [] sgn num = False
isFieldBusy (p:paws) sgn num
	| sgn < 'a' || sgn > 'h' || num > 8 || num < 1 = True
	| fst p == sgn && snd p == num = True
	| otherwise = isFieldBusy paws sgn num

--rusza pionkiem
movePaw paws opponentPaws currSgn currNum destSgn destNum
	| 	isFieldBusy (paws++opponentPaws) destSgn destNum == True || --Nothing--error --error "field is busy"
		indexOfPaw paws currSgn currNum == -1 = []
	| otherwise = movePaw' paws currSgn currNum destSgn destNum

movePaw' [] currSgn currNum destSgn destNum = []
movePaw' (p:paws) currSgn currNum destSgn destNum
	| fst p == currSgn && snd p == currNum = (destSgn,destNum):paws
	| otherwise = p:(movePaw' paws currSgn currNum destSgn destNum)

--usuwa pionek
removePaw (p:paws) sgn num
	| fst p == sgn && snd p == num = paws
	| otherwise = p:(removePaw paws sgn num)

data DIRECTION = UP | DOWN deriving(Show, Eq, Ord)

--pobiera tekstowa reprezentacje planszy
getGameString blacks whites = getGameString' blacks whites "" 'a' 1
getGameString' blacks whites board currSgn currNum
	| currNum > 8 = board
	| currSgn > 'h' = getGameString' blacks whites board 'a' (currNum+1)
	| isFieldBusy blacks currSgn currNum = getGameString' blacks whites (board++"b") (chr ((ord currSgn)+1)) currNum
	| isFieldBusy whites currSgn currNum = getGameString' blacks whites (board++"w") (chr ((ord currSgn)+1)) currNum
	| otherwise = getGameString' blacks whites (board++"_") (chr ((ord currSgn)+1)) currNum

--wypisuje plansze
printGame b w = putStr(printGame' b w)
printGame' b w = printGame'' (getGameString b w) "" '1'
printGame'' "" res n = "_abcdefgh_\n"++res++"_abcdefgh_\n"
printGame'' str res n = printGame'' (drop 8 str) (res++[n]++(take 8 str)++[n]++"\n") (chr ((ord n)+1))

--sprawdza czy dany pionek ma mozliwosc bicia
canPawBeat paws opponentPaws dir currSgn currNum
	| not (canPawMove (paws++opponentPaws) dir currSgn currNum 2) = False
	| dir==UP && (chr ((ord currSgn)+2))<='h' && (currNum-2)>=1 &&
		isFieldBusy opponentPaws (chr ((ord currSgn)+1)) (currNum-1) = True
	| dir==UP && (chr ((ord currSgn)-2))>='a' && (currNum-2)>=1 &&
		isFieldBusy opponentPaws (chr ((ord currSgn)-1)) (currNum-1) = True
	| dir==DOWN && (chr ((ord currSgn)+2))<='h' && (currNum+2)<=8 &&
		isFieldBusy opponentPaws (chr ((ord currSgn)+1)) (currNum+1) = True
	| dir==DOWN && (chr ((ord currSgn)-2))>='a' && (currNum+2)<=8 &&
		isFieldBusy opponentPaws (chr ((ord currSgn)-1)) (currNum+1) = True
	| otherwise = False

--pobiera liste pionkow zdolnych do bicia
getPawsBeats paws opponentPaws dir = getPawsBeats' paws opponentPaws dir [] paws
getPawsBeats' paws opponentPaws dir list [] = list
getPawsBeats' paws opponentPaws dir list (p:pawsCpy)
	| canPawBeat paws opponentPaws dir sgn num = getPawsBeats' paws opponentPaws dir (p:list) pawsCpy
	| otherwise = getPawsBeats' paws opponentPaws dir list pawsCpy
	where
		sgn = fst p
		num = snd p

--zwaraca krotke (ilosc_skokow,[(x1,n1),(x2,n2),...]) z lista ruchow wykrywajac od razu najdluzsza sekwencje czyli
--najwieksza ilosc bic
getPawBeats paws opponentPaws direction sgn num
	| direction == UP = getPawBeatsUp paws opponentPaws sgn num [] 0
	| direction == DOWN = getPawBeatsDown paws opponentPaws sgn num [] 0
--dla UP
getPawBeatsUp paws opponentPaws sgn num list n
	| onTheLeft && onTheRight && fst l >= fst r = l
	| onTheLeft && onTheRight && fst l < fst r = r
	| onTheLeft = getPawBeatsUp paws opponentPaws (chr ((ord sgn)-2)) (num-2) ((sgn,num):list) (n+1)
	| onTheRight = getPawBeatsUp paws opponentPaws (chr ((ord sgn)+2)) (num-2) ((sgn,num):list) (n+1)
	| otherwise = (n,reverse ((sgn,num):list))
	where
		onTheRight = (chr ((ord sgn)+2)) <= 'h' && (num-2) >=1 &&
			isFieldBusy opponentPaws (chr ((ord sgn)+1)) (num-1) &&
			not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)+2)) (num-2))
		onTheLeft = (chr ((ord sgn)-2)) >= 'a' && (num-2) >= 1 &&
			isFieldBusy opponentPaws (chr ((ord sgn)-1)) (num-1) &&
			not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)-2)) (num-2))
		l = getPawBeatsUp paws opponentPaws (chr ((ord sgn)-2)) (num-2) ((sgn,num):list) (n+1)
		r = getPawBeatsUp paws opponentPaws (chr ((ord sgn)+2)) (num-2) ((sgn,num):list) (n+1)
--dla DOWN
getPawBeatsDown paws opponentPaws sgn num list n
	| onTheLeft && onTheRight && fst l >= fst r = l
	| onTheLeft && onTheRight && fst l < fst r = r
	| onTheLeft = getPawBeatsDown paws opponentPaws (chr ((ord sgn)-2)) (num+2) ((sgn,num):list) (n+1)
	| onTheRight = getPawBeatsDown paws opponentPaws (chr ((ord sgn)+2)) (num+2) ((sgn,num):list) (n+1)
	| otherwise = (n,reverse ((sgn,num):list))
	where
		onTheRight = (chr ((ord sgn)+2)) <= 'h' && (num+2) >=1 &&
			isFieldBusy opponentPaws (chr ((ord sgn)+1)) (num+1) &&
			not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)+2)) (num+2))
		onTheLeft = (chr ((ord sgn)-2)) >= 'a' && (num+2) >= 1 &&
			isFieldBusy opponentPaws (chr ((ord sgn)-1)) (num+1) &&
			not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)-2)) (num+2))
		l = getPawBeatsDown paws opponentPaws (chr ((ord sgn)-2)) (num+2) ((sgn,num):list) (n+1)
		r = getPawBeatsDown paws opponentPaws (chr ((ord sgn)+2)) (num+2) ((sgn,num):list) (n+1)

--sprawdza czy dany pionek ma mozliwosc bicia
canPawMove paws dir sgn num step
	| 	dir == UP && (chr ((ord sgn)+step)) <= 'h' && (num-step) >=1 &&
		not (isFieldBusy paws (chr ((ord sgn)+step)) (num-step)) = True
	| 	dir == UP && (chr ((ord sgn)-step)) >= 'a' && (num-step) >=1 &&
		not (isFieldBusy paws (chr ((ord sgn)-step)) (num-step)) = True
	| 	dir == DOWN && (chr ((ord sgn)+step)) <= 'h' && (num+step) <= 8 &&
		not (isFieldBusy paws (chr ((ord sgn)+step)) (num+step)) = True
	| 	dir == DOWN && (chr ((ord sgn)-step)) >= 'a' && (num+step) <= 8 &&
		not (isFieldBusy paws (chr ((ord sgn)-step)) (num+step)) = True
	| otherwise = False

--sprawdza czy dany ruch jest dostepny
isMoveAvailable paws dir currSgn currNum destSgn destNum
	| not (canPawMove paws dir currSgn currNum 1) = False
	| dir==UP && (chr ((ord currSgn)+1))==destSgn && (currNum-1)==destNum=True
	| dir==UP && (chr ((ord currSgn)-1))==destSgn && (currNum-1)==destNum=True
	| dir==DOWN && (chr ((ord currSgn)+1))==destSgn && (currNum+1)==destNum=True
	| dir==DOWN && (chr ((ord currSgn)-1))==destSgn && (currNum+1)==destNum=True
	| otherwise = False

--sprawdza czy dane bicie jest dostepne
isBeatAvailable paws opponentPaws dir currSgn currNum destSgn destNum
	| not (canPawMove (paws++opponentPaws) dir currSgn currNum 2) = False
	| dir==UP && (chr ((ord currSgn)+2))==destSgn && (currNum-2)==destNum &&
		isFieldBusy opponentPaws (chr ((ord currSgn)+1)) (currNum-1) = True
	| dir==UP && (chr ((ord currSgn)-2))==destSgn && (currNum-2)==destNum &&
		isFieldBusy opponentPaws (chr ((ord currSgn)-1)) (currNum-1) = True
	| dir==DOWN && (chr ((ord currSgn)+2))==destSgn && (currNum+2)==destNum &&
		isFieldBusy opponentPaws (chr ((ord currSgn)+1)) (currNum+1) = True
	| dir==DOWN && (chr ((ord currSgn)-2))==destSgn && (currNum+2)==destNum &&
		isFieldBusy opponentPaws (chr ((ord currSgn)-1)) (currNum+1) = True
	| otherwise = False

--zwraca tablice pionkow zdolnych do ruchu
getPawsMoves paws opponentPaws dir = getPawsMoves' paws opponentPaws dir [] paws
getPawsMoves' paws opponentPaws dir list [] = list
getPawsMoves' paws opponentPaws dir list (p:pawsCpy)
	| canPawMove (paws++opponentPaws) dir sgn num 1 = getPawsMoves' paws opponentPaws dir (p:list) pawsCpy
	| otherwise = getPawsMoves' paws opponentPaws dir list pawsCpy
	where
		sgn = fst p
		num = snd p

--sprawdza dane bicie
checkBeat dir paws opponentPaws from [] = True
checkBeat dir paws opponentPaws from (to:toArr)
	| isBeatAvailable paws opponentPaws dir sgnFrom numFrom sgnTo numTo =
		checkBeat dir paws opponentPaws to toArr
	| otherwise = False
	where
		sgnFrom = fst (getFieldByCode (read from :: Float))
		numFrom = snd (getFieldByCode (read from :: Float))
		sgnTo = fst (getFieldByCode (read to :: Float))
		numTo = snd (getFieldByCode (read to :: Float))

--liczy roznice miedzy iloscia mozliwych bic dla gracza i przeciwnika
getGameValue paws opponentPaws dir = 
	(length $getPawsBeats paws opponentPaws dir)-(length $getPawsBeats opponentPaws paws (oppositeDirection dir))

--sprawdza ruch na prawo
getRightMove sgn num dir
	| dir==UP && c<='h' && (num-1)>=1=(c,(num-1))
	| dir==DOWN && c<='h' && (num+1)<=8=(c,(num+1))
	| otherwise = ('0',0)
	where
		c = (chr ((ord sgn)+1))

--sprawdza ruch na lewo
getLeftMove sgn num dir
	| dir==UP && c>='a' && (num-1)>=1=(c,(num-1))
	| dir==DOWN && c>='a' && (num+1)<=8=(c,(num+1))
	| otherwise = ('0',0)
	where
		c = (chr ((ord sgn)-1))

--zwraca krotke z najlepszy miejscem na ruch dla danych sgn num
getPawMove paws opponentPaws sgn num dir
	| l==('0',0) && r==('0',0) = ('0',0)
	| l==('0',0) = r
	| r==('0',0) = l
	| getGameValue (movePaw paws opponentPaws sgn num (fst l) (snd l)) opponentPaws dir > 
			getGameValue (movePaw paws opponentPaws sgn num (fst r) (snd r)) opponentPaws dir = l
	| otherwise = r
	where
		l = getLeftMove sgn num dir
		r = getRightMove sgn num dir

colors = ["white","black"]

data MODE = AIVSAI | AIVSMAN | MANVSAI | MANVSMAN deriving(Show,Ord,Eq)