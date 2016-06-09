{-
data Paw sign number color = Paw Char Int String deriving(Show)

getSign (Paw s n c) = s
getNumber (Paw s n c) = n
getColor (Paw s n c) = c
-}

import Data.Char
import Data.List.Split
import System.Random
import System.Process

rnd n _min _max = fst (randomR (_min,_max) (mkStdGen n))

w = (zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3])
b = (zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6])

d n = n :: Integer

getFieldByCode n = ceiling(n)*2 - ((ceiling(n/4)-1) `mod` 2)

getGameValue blacks whites color
	| color == 'b' = (length blacks) - (length whites)
	| color == 'w' = (length whites) - (length blacks)

indexOfPaw paws sgn num = indexOfPaw' paws sgn num 0
indexOfPaw' paws sgn num index
	| index >= length paws = -1
	| fst p == sgn && snd p == num = index
	| otherwise = indexOfPaw' paws sgn num (index+1)
	where p = paws!!index
{-
setSign [] currSgn currNum newSgn = []
setSign (p:paws) currSgn currNum newSgn
	| fst p == currSgn && snd p == currNum = (newSgn,currNum):paws
	| otherwise = p:(setSign paws currSgn currNum newSgn)

setNum [] currSgn currNum newNum = []
setNum (p:paws) currSgn currNum newNum
	| fst p == currSgn && snd p == currNum = (currSgn,newNum):paws
	| otherwise = p:(setNum paws currSgn currNum newNum)
-}
isFieldBusy [] sgn num = False
isFieldBusy (p:paws) sgn num
	| sgn < 'a' || sgn > 'h' || num > 8 || num < 1 = True
	| fst p == sgn && snd p == num = True
	| otherwise = isFieldBusy paws sgn num

movePaw paws opponentPaws currSgn currNum destSgn destNum
	| 	isFieldBusy (paws++opponentPaws) destSgn destNum == True || --Nothing--error --error "field is busy"
		indexOfPaw paws currSgn currNum == -1 = []
	| otherwise = movePaw' paws currSgn currNum destSgn destNum

movePaw' [] currSgn currNum destSgn destNum = []
movePaw' (p:paws) currSgn currNum destSgn destNum
	| fst p == currSgn && snd p == currNum = (destSgn,destNum):paws
	| otherwise = p:(movePaw' paws currSgn currNum destSgn destNum)

removePaw (p:paws) sgn num
	| fst p == sgn && snd p == num = paws
	| otherwise = p:(removePaw paws sgn num)

data DIRECTION = UP | DOWN deriving(Show, Eq, Ord)

getGameString blacks whites = getGameString' blacks whites "" 'a' 1
getGameString' blacks whites board currSgn currNum
	| currNum > 8 = board
	| currSgn > 'h' = getGameString' blacks whites board 'a' (currNum+1)
	| isFieldBusy blacks currSgn currNum = getGameString' blacks whites (board++"b") (chr ((ord currSgn)+1)) currNum
	| isFieldBusy whites currSgn currNum = getGameString' blacks whites (board++"w") (chr ((ord currSgn)+1)) currNum
	| otherwise = getGameString' blacks whites (board++"_") (chr ((ord currSgn)+1)) currNum

printGame b w = putStr(printGame' b w)
printGame' b w = printGame'' (getGameString b w) "" '1'
printGame'' "" res n = "_abcdefgh_\n"++res++"_abcdefgh_\n"
printGame'' str res n = printGame'' (drop 8 str) (res++[n]++(take 8 str)++[n]++"\n") (chr ((ord n)+1))

test 0 = []
test n
	| n == 4 = (chr(ord('a')+n),n):(test (n-1))
	| otherwise = (chr(ord('a')+n),n):(test (n-1))

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



canPawMove paws dir sgn num
	| 	dir == UP && (chr ((ord sgn)+1)) <= 'h' && (num-1) >=1 &&
		not (isFieldBusy paws (chr ((ord sgn)+1)) (num-1)) = True
	| 	dir == UP && (chr ((ord sgn)-1)) >= 'a' && (num-1) >=1 &&
		not (isFieldBusy paws (chr ((ord sgn)-1)) (num-1)) = True
	| 	dir == DOWN && (chr ((ord sgn)+1)) <= 'h' && (num+1) <= 8 &&
		not (isFieldBusy paws (chr ((ord sgn)+1)) (num+1)) = True
	| 	dir == DOWN && (chr ((ord sgn)-1)) >= 'a' && (num+1) <= 8 &&
		not (isFieldBusy paws (chr ((ord sgn)-1)) (num+1)) = True
	| otherwise = False

getPawsMoves paws opponentPaws dir = getPawsMoves' paws opponentPaws dir 0 []
getPawsMoves' paws opponentPaws dir currIndex list
	| currIndex >= length paws = list
	| canPawMove (paws++opponentPaws) dir sgn num = getPawsMoves' paws opponentPaws dir (currIndex+1) (currIndex:list)
	| otherwise = getPawsMoves' paws opponentPaws dir (currIndex+1) list
	where
		sgn = fst (paws!!currIndex)
		num = snd (paws!!currIndex)

--wyszukuje zbior ruchow rownowaznych o najwyzszym priorytecie czyli najpierw szuka bic
--a jak nie ma bic to szuka zywklych ruchow
{-}
aiMakeMove paws opponentPaws dir moveNumber
	| length b > 0 = aiBeat paws opponentPaws 
	| otherwise = 0
	where
		b = getPawsBeats paws opponentPaws dir
		m = getPawsMoves paws opponentPaws dir
-}
getRandomElement list n = list!!(rnd n 0 (length list - 1))
--x = RandomRIO (1,10)

aiBeat paws opponentPaws sgn num = 0
aiMove = 0
x=movePaw w b 'f' 3 'f' 5
{-}
ww = movePaw (movePaw w b 'f' 3 'f' 5) b 'c' 2 'f' 3
bb = movePaw (movePaw b w 'e' 6 'e' 4) w 'b' 7 'e' 6
-}
isGameOver blacks whites = length blacks == 0 && length whites == 0

makeMove paws opponentPaws = do
	move <- getLine
	system "clear"
	putStrLn("your move: " ++ move)
	printGame b w
	if move == "q" then return "ok" else makeMove paws opponentPaws









{-
smieci
------------------------------------------------------------------
spl delimiter str = spl' delimiter str [] []

spl' delimiter [] currentWord res = (res++reverse currentWord)
spl' delimiter (s:str) currentWord res
	| s == delimiter = spl' delimiter str [] (res++reverse currentWord)
	| otherwise = spl' delimiter str (s:currentWord) res

count [] n = n
count (s:str) n
	| s == ' ' = count str (n+1)
	| otherwise = count str n
-}