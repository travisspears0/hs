module Warcaby where

import Data.Char
import Data.List.Split
import System.Random
import System.Process
import Ai
import Helping

rnd n _min _max = fst (randomR (_min,_max) (mkStdGen n))

w:: [(Char,Int)]
b:: [(Char,Int)]
ww:: [(Char,Int)]
bb:: [(Char,Int)]
w = (zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3])
b = (zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6])

ww = (('f',5):(zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3]))
bb = removePaw (removePaw ((('c',4):('g',4):(zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6]))) 'f' 7) 'g' 6

getFieldByCode n = getFieldByCode' (ceiling(n)*2 - ((ceiling(n/4)-1) `mod` 2))
getFieldByCode' n
	| m == 0 = ('h',floor(fromIntegral(n)/8))
	| otherwise = (chr(ord 'a'-1+m),floor(fromIntegral(n)/8)+1)
	where m = n `mod` 8

oppositeDirection dir
	| dir == UP = DOWN
	| otherwise = UP

indexOfPaw paws sgn num = indexOfPaw' paws sgn num 0
indexOfPaw' paws sgn num index
	| index >= length paws = -1
	| fst p == sgn && snd p == num = index
	| otherwise = indexOfPaw' paws sgn num (index+1)
	where p = paws!!index

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

isMoveAvailable paws dir currSgn currNum destSgn destNum
	| not (canPawMove paws dir currSgn currNum 1) = False
	| dir==UP && (chr ((ord currSgn)+1))==destSgn && (currNum-1)==destNum=True
	| dir==UP && (chr ((ord currSgn)-1))==destSgn && (currNum-1)==destNum=True
	| dir==DOWN && (chr ((ord currSgn)+1))==destSgn && (currNum+1)==destNum=True
	| dir==DOWN && (chr ((ord currSgn)-1))==destSgn && (currNum+1)==destNum=True
	| otherwise = False

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

getPawsMoves paws opponentPaws dir = getPawsMoves' paws opponentPaws dir [] paws
getPawsMoves' paws opponentPaws dir list [] = list
getPawsMoves' paws opponentPaws dir list (p:pawsCpy)
	| canPawMove (paws++opponentPaws) dir sgn num 1 = getPawsMoves' paws opponentPaws dir (p:list) pawsCpy
	| otherwise = getPawsMoves' paws opponentPaws dir list pawsCpy
	where
		sgn = fst p
		num = snd p

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

--wyszukuje zbior ruchow rownowaznych o najwyzszym priorytecie czyli najpierw szuka bic
--a jak nie ma bic to szuka zywklych ruchow
--zwraca krotke: (paws,opponentpaws)
aiMakeMove paws opponentPaws dir movesCounter
	| fst beatchosen /= '0' = aiBeat paws opponentPaws (snd (getPawBeats paws opponentPaws dir (fst beatchosen) (snd beatchosen))) ((fst beatchosen),(snd beatchosen))
	| otherwise = (movePaw paws opponentPaws (fst moveChosen) (snd moveChosen) (fst gpm) (snd gpm),opponentPaws)
	where
		moveChosen = aiChooseMove paws opponentPaws dir movesCounter
		beatchosen = aiChooseBeat paws opponentPaws dir movesCounter
		gpm = getPawMove paws opponentPaws (fst moveChosen) (snd moveChosen) dir

aiBeat paws opponentPaws [] current = (paws,opponentPaws)
aiBeat paws opponentPaws (p:path) current = 
	aiBeat (movePaw paws opponentPaws currSgn currNum destSgn destNum)
	(removePaw opponentPaws (chr (floor((fromIntegral (ord(destSgn)+ord(currSgn)))/2))) (floor((fromIntegral (currNum+destNum))/2)))
	path p
	where
		currSgn = fst current
		currNum = snd current
		destSgn = fst p
		destNum = snd p

getRandomElement list n = list!!(rnd n 0 (length list - 1))
--x = RandomRIO (1,10)

--liczy roznice miedzy iloscia mozliwych bic dla gracza i przeciwnika
getGameValue paws opponentPaws dir = 
	(length $getPawsBeats paws opponentPaws dir)-(length $getPawsBeats opponentPaws paws (oppositeDirection dir))


--wybiera najlepsze bicie
aiChooseBeat paws opponentPaws dir movesCounter
	| length pb == 0 = ('0',0)
	| otherwise = getRandomElement (aiChooseBeat' paws opponentPaws dir pb 0 []) movesCounter
	where pb = getPawsBeats paws opponentPaws dir
aiChooseBeat' paws opponentPaws dir [] currentMax list = list
aiChooseBeat' paws opponentPaws dir (b:beats) currentMax list
	| currentMax < fst (getPawBeats paws opponentPaws dir sgnB numB) = 
		aiChooseBeat' paws opponentPaws dir beats (fst (getPawBeats paws opponentPaws dir sgnB numB)) [b]
	| currentMax == fst (getPawBeats paws opponentPaws dir sgnB numB) = 
		aiChooseBeat' paws opponentPaws dir beats currentMax (b:list)
	| otherwise = aiChooseBeat' paws opponentPaws dir beats currentMax list
	where
		sgnB = fst b
		numB = snd b

getRightMove sgn num dir
	| dir==UP && c<='h' && (num-1)>=1=(c,(num-1))
	| dir==DOWN && c<='h' && (num+1)<=8=(c,(num+1))
	| otherwise = ('0',0)
	where
		c = (chr ((ord sgn)+1))

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

--wybiera najlepszy ruch a jak jest pare mozliwych to losuje
aiChooseMove paws opponentPaws dir movesCounter
	| length pm == 0 = ('0',0)
	| otherwise = getRandomElement (aiChooseMove' paws opponentPaws dir pm 0 []) movesCounter
	where pm = getPawsMoves paws opponentPaws dir
aiChooseMove' paws opponentPaws dir [] currVal list = list
aiChooseMove' paws opponentPaws dir (m:moves) currVal list
	| getGameValue (movePaw paws opponentPaws currSgn currNum destSgn destNum) opponentPaws dir > currVal =
		aiChooseMove' paws opponentPaws dir moves (getGameValue (movePaw paws opponentPaws currSgn currNum destSgn destNum) opponentPaws dir) [m]
	| getGameValue (movePaw paws opponentPaws currSgn currNum destSgn destNum) opponentPaws dir == currVal = 
		aiChooseMove' paws opponentPaws dir moves currVal (m:list)
	| otherwise = aiChooseMove' paws opponentPaws dir moves currVal list
	where
		currSgn = fst m
		currNum = snd m
		destSgn = fst (getPawMove paws opponentPaws (fst m) (snd m) dir)
		destNum = snd (getPawMove paws opponentPaws (fst m) (snd m) dir)
{-}
aiMove paws opponentPaws dir movesCounter 
	| length beats /= 0 = aiChooseBeat paws opponentPaws dir movesCounter
	| length moves /= 0 = aiChooseMove paws opponentPaws dir movesCounter
	| otherwise = error "no possible moves!"
	where
		beats = getPawsBeats paws opponentPaws dir
		moves = getPawsMoves paws opponentPaws dir
-}
{-}
ww = movePaw (movePaw w b 'f' 3 'f' 5) b 'c' 2 'f' 3
bb = movePaw (movePaw b w 'e' 6 'e' 4) w 'b' 7 'e' 6
-}
isGameOver blacks whites = length blacks == 0 || length whites == 0

die str f = do
	putStrLn(str)
	f

colors = ["white","black"]

data MODE = AIVSAI | AIVSMAN | MANVSAI | MANVSMAN deriving(Show,Ord,Eq)



---------------------------------------------------------------------

nextHop moveCounter blacks whites str mode
	| mode == AIVSAI = getMoveFromAi moveCounter blacks whites str mode
	| mode == MANVSMAN = orderMove moveCounter blacks whites str mode
	| mode == AIVSMAN && mod moveCounter 2 == 0 = getMoveFromAi moveCounter blacks whites str mode
	| mode == AIVSMAN && mod moveCounter 2 == 1 = orderMove moveCounter blacks whites str mode
	| mode == MANVSAI && mod moveCounter 2 == 0 = orderMove moveCounter blacks whites str mode
	| mode == MANVSAI && mod moveCounter 2 == 1 = getMoveFromAi moveCounter blacks whites str mode

getMoveFromAi moveCounter blacks whites str mode = do
	system "clear"
	printGame blacks whites
	putStrLn("move "++(show moveCounter)++" press enter to get ai move")
	c <- getLine
	if(moveCounter `mod` 2 == 0)
		then nextHop (moveCounter+1) (snd wm) (fst wm) str mode
		else nextHop (moveCounter+1) (fst bm) (snd bm) str mode
		where
			wm = (aiMakeMove whites blacks DOWN moveCounter)
			bm = (aiMakeMove blacks whites UP moveCounter)
		{-}
		then if length getPawsBeats whites blacks DOWN > 0
			then nextHop (moveCounter+1) blacks (whites) str mode--beat
			else if length getPawsMoves whites blacks DOWN > 0
				then nextHop (moveCounter+1) blacks (whites) str mode--move
				else gameOver --game over
		else if length getPawsBeats blacks whites UP == 0
			then  nextHop (moveCounter+1) (blacks) whites str mode--beat
			else if length getPawsMoves blacks whites UP > 0
				then nextHop (moveCounter+1) (blacks) whites str mode--move
				else gameOver --game over
-}
over = do
	putStrLn("gameOver")

orderMove moveCounter blacks whites str mode = do
	system "clear"
	printGame blacks whites
	putStrLn("move ["++show(moveCounter+1)++"] "++colors!!turn++"s to move")
	putStrLn("-moving:  a-b")
	putStrLn("-beating: axb")
	putStrLn(str)
	move <- getLine
	if (length (splitOn "-" move) /= 2)
		then --orderMove moveCounter blacks whites "wrong input"
			if (length (splitOn "x" move) <= 1)
				then orderMove moveCounter blacks whites "wrong input" mode
				else
					if turn == 0
						then whitesBeat moveCounter blacks whites (splitOn "x" move) mode
						else blacksBeat moveCounter blacks whites (splitOn "x" move) mode
		else 
			if turn == 0
				then whitesMove moveCounter blacks whites ((splitOn "-" move)!!0) ((splitOn "-" move)!!1) mode --makeMove moveCounter 0 blacks whites ((splitOn "-" move)!!0) ((splitOn "-" move)!!1)
				else blacksMove moveCounter blacks whites ((splitOn "-" move)!!0) ((splitOn "-" move)!!1) mode
	where 
		turn = (moveCounter `mod` 2)

---------------------------------------------------------------------

blacksMove moveCounter blacks whites from to mode = do
	if not (isMoveAvailable (blacks++whites) UP sgnFrom numFrom sgnTo numTo) || (movePaw blacks whites sgnFrom numFrom sgnTo numTo) == []
		then orderMove moveCounter blacks whites "*could not make that move" mode
		else nextHop (moveCounter+1) (movePaw blacks whites sgnFrom numFrom sgnTo numTo) whites
				("blacks moved from ("++[sgnFrom]++","++(show numFrom)++") to ("++[sgnTo]++","++(show numTo)++")") mode
	nextHop (moveCounter+1) blacks whites "" mode
	where
		sgnFrom = fst (getFieldByCode (read from :: Float))
		numFrom = snd (getFieldByCode (read from :: Float))
		sgnTo = fst (getFieldByCode (read to :: Float))
		numTo = snd (getFieldByCode (read to :: Float))

blacksBeat moveCounter blacks whites (a:arr) mode = do
	if not (checkBeat UP blacks whites a arr)
		then orderMove moveCounter blacks whites "*could not make that beat" mode
		else blacksBeat' moveCounter blacks whites a arr "" mode

blacksBeat' moveCounter blacks whites from [] str mode = do
	nextHop (moveCounter+1) blacks whites str mode
blacksBeat' moveCounter blacks whites from (to:toArr) str mode = do
	blacksBeat' moveCounter 
		(movePaw blacks whites sgnFrom numFrom sgnTo numTo)
		(removePaw whites (chr (floor((fromIntegral (ord(sgnTo)+ord(sgnFrom)))/2))) (floor((fromIntegral (numFrom+numTo))/2)))
		to toArr
		(str++"blacks beat from ("++[sgnFrom]++","++(show numFrom)++") to ("++[sgnTo]++","++(show numTo)++")\n") mode
	where
		sgnFrom = fst (getFieldByCode (read from :: Float))
		numFrom = snd (getFieldByCode (read from :: Float))
		sgnTo = fst (getFieldByCode (read to :: Float))
		numTo = snd (getFieldByCode (read to :: Float))

---------------------------------------------------------------------

whitesMove moveCounter blacks whites from to mode = do
	if not (isMoveAvailable (blacks++whites) DOWN sgnFrom numFrom sgnTo numTo) || ((movePaw whites blacks sgnFrom numFrom sgnTo numTo) == [])
		then orderMove moveCounter blacks whites "*could not make that move" mode
		else nextHop (moveCounter+1) blacks (movePaw whites blacks sgnFrom numFrom sgnTo numTo) 
				("whites moved from ("++[sgnFrom]++","++(show numFrom)++") to ("++[sgnTo]++","++(show numTo)++")") mode
	nextHop (moveCounter+1) blacks whites "" mode
	where
		sgnFrom = fst (getFieldByCode (read from :: Float))
		numFrom = snd (getFieldByCode (read from :: Float))
		sgnTo = fst (getFieldByCode (read to :: Float))
		numTo = snd (getFieldByCode (read to :: Float))

whitesBeat moveCounter blacks whites (a:arr) mode = do
	if not (checkBeat DOWN whites blacks a arr)
		then orderMove moveCounter blacks whites "*could not make that beat" mode
		else whitesBeat' moveCounter blacks whites a arr "" mode

whitesBeat' moveCounter blacks whites from [] str mode = do
	nextHop (moveCounter+1) blacks whites str mode
whitesBeat' moveCounter blacks whites from (to:toArr) str mode = do
	whitesBeat' moveCounter 
		(removePaw blacks (chr (floor((fromIntegral (ord(sgnTo)+ord(sgnFrom)))/2))) (floor((fromIntegral (numFrom+numTo))/2)))
		(movePaw whites blacks sgnFrom numFrom sgnTo numTo) to toArr
		(str++"whites beat from ("++[sgnFrom]++","++(show numFrom)++") to ("++[sgnTo]++","++(show numTo)++")\n") mode
	where
		sgnFrom = fst (getFieldByCode (read from :: Float))
		numFrom = snd (getFieldByCode (read from :: Float))
		sgnTo = fst (getFieldByCode (read to :: Float))
		numTo = snd (getFieldByCode (read to :: Float))

---------------------------------------------------------------------

begin mode = do
	system "clear"
	putStrLn("game started")
	printGame blacks whites
	nextHop 0 blacks whites "" mode
		where
			whites = ((zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3]))
				--(('f',5):(zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3]))
				--((zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3]))
			blacks = ((zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6]))
				--removePaw ((('c',4):(zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6]))) 'f' 7
				--((zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6]))

gameStart = do
	putStrLn("welcome!\nchoose game:\n[1] man vs man\n[2] man vs ai\n[3] ai vs man\n[4] ai vs ai")
	game <- getLine
	if game == "1" then begin MANVSMAN else
		if game == "2" then begin MANVSAI else
			if game == "3" then begin AIVSMAN else
				if game == "4" then begin AIVSAI else
				gameStart

