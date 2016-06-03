{-
data Paw sign number color = Paw Char Int String deriving(Show)

getSign (Paw s n c) = s
getNumber (Paw s n c) = n
getColor (Paw s n c) = c
-}

import Data.Char
import System.Random

rnd n _min _max = fst (randomR (_min,_max) (mkStdGen n))

w = zip (take 8 ['a'..]) $take 8 $cycle [1,2]
b = zip (take 8 ['a'..]) $take 8 $cycle [7,8]

getGameValue blacks whites color
	| color == 'b' = (length blacks) - (length whites)
	| color == 'w' = (length whites) - (length blacks)

indexOfPaw paws sgn num = indexOfPaw' paws sgn num 0
indexOfPaw' paws sgn num index
	| index >= length paws = -1
	| fst p == sgn && snd p == num = index
	| otherwise = indexOfPaw' paws sgn num (index+1)
	where p = paws!!index

setSign (p:paws) currSgn currNum newSgn
	| fst p == currSgn && snd p == currNum = (newSgn,currNum):paws
	| otherwise = p:(setSign paws currSgn currNum newSgn)

setNum (p:paws) currSgn currNum newNum
	| fst p == currSgn && snd p == currNum = (currSgn,newNum):paws
	| otherwise = p:(setNum paws currSgn currNum newNum)

isFieldBusy [] sgn num = False
isFieldBusy (p:paws) sgn num
	| fst p == sgn && snd p == num = True
	| otherwise = isFieldBusy paws sgn num

movePaw blacks whites currSgn currNum destSgn destNum
	| isFieldBusy (blacks++whites) destSgn destNum == True = []
	| indexOfPaw blacks currSgn currNum /= -1 = movePaw' blacks currSgn currNum destSgn destNum
	| indexOfPaw whites currSgn currNum /= -1 = movePaw' whites currSgn currNum destSgn destNum
		
movePaw' paws currSgn currNum destSgn destNum =
	setSign (setNum paws currSgn currNum destNum) currSgn destNum destSgn

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

{-
hasPawBeat paws opponentPaws index dir
	| 	dir == UP && (chr ((ord sgn)+2)) <= 'h' && (num-2) >=1 &&
		isFieldBusy opponentPaws (chr ((ord sgn)+1)) (num-1) &&
		not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)+2)) (num-2)) = True
	| 	dir == UP && (chr ((ord sgn)-2)) >= 'a' && (num-2) >= 1 &&
		isFieldBusy opponentPaws (chr ((ord sgn)-1)) (num-1) &&
		not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)-2)) (num-2)) = True
	| 	dir == DOWN && (chr ((ord sgn)+2)) <= 'h' && (num+2) <= 8 &&
		isFieldBusy opponentPaws (chr ((ord sgn)+1)) (num+1) &&
		not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)+2)) (num+2)) = True
	| 	dir == DOWN && (chr ((ord sgn)-2)) >= 'a' && (num+2) <= 8 &&
		isFieldBusy opponentPaws (chr ((ord sgn)-1)) (num+1) &&
		not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)-2)) (num+2)) = True
	| otherwise = False
	where
		sgn = fst (paws!!index)
		num = snd (paws!!index)

getPawsBeats paws opponentPaws dir = getPawsBeats' paws opponentPaws dir 0 []
getPawsBeats' paws opponentPaws dir currIndex list
	| currIndex >= length paws = list
	| hasPawBeat paws opponentPaws currIndex dir =
		getPawsBeats' paws opponentPaws dir (currIndex+1) (currIndex:list)
	| otherwise = getPawsBeats' paws opponentPaws dir (currIndex+1) list
-}

test 0 = []
test n
	| n == 4 = (chr(ord('a')+n),n):(test (n-1))
	| otherwise = (chr(ord('a')+n),n):(test (n-1))

getPawBeats paws opponentPaws UP sgn num list n
	| onTheLeft && onTheRight && fst l >= fst r = l
	| onTheLeft && onTheRight && fst l < fst r = r
	| onTheLeft = getPawBeats paws opponentPaws UP (chr ((ord sgn)-2)) (num-2) ((sgn,num):list) (n+1)
	| onTheRight = getPawBeats paws opponentPaws UP (chr ((ord sgn)+2)) (num-2) ((sgn,num):list) (n+1)
	| otherwise = (n,reverse ((sgn,num):list))
	where
		onTheRight = (chr ((ord sgn)+2)) <= 'h' && (num-2) >=1 &&
			isFieldBusy opponentPaws (chr ((ord sgn)+1)) (num-1) &&
			not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)+2)) (num-2))
		onTheLeft = (chr ((ord sgn)-2)) >= 'a' && (num-2) >= 1 &&
			isFieldBusy opponentPaws (chr ((ord sgn)-1)) (num-1) &&
			not (isFieldBusy (paws++opponentPaws) (chr ((ord sgn)-2)) (num-2))
		l = getPawBeats paws opponentPaws UP (chr ((ord sgn)-2)) (num-2) ((sgn,num):list) (n+1)
		r = getPawBeats paws opponentPaws UP (chr ((ord sgn)+2)) (num-2) ((sgn,num):list) (n+1)

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

ww = movePaw b (movePaw b (movePaw b w 'g' 1 'b' 4) 'd' 2 'd' 4) 'b' 2 'b' 6
bb = movePaw (movePaw (movePaw b w 'c' 7 'c' 5) w 'a' 7 'a' 3) w 'e' 7 'e' 3