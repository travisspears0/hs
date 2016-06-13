module Main where

import Data.Char
import Data.List.Split
import System.Random
import System.Process
import Helping
import Ai

---------------------------------------------------------------------

--decyduje na podstawie trybu i licznika ruchow, kto teraz, ai czy gracz i przekierowuje do odpowiedniej funkcji
nextHop moveCounter blacks whites str mode
	| mode == AIVSAI = getMoveFromAi moveCounter blacks whites str mode
	| mode == MANVSMAN = orderMove moveCounter blacks whites str mode
	| mode == AIVSMAN && mod moveCounter 2 == 0 = getMoveFromAi moveCounter blacks whites str mode
	| mode == AIVSMAN && mod moveCounter 2 == 1 = orderMove moveCounter blacks whites str mode
	| mode == MANVSAI && mod moveCounter 2 == 0 = orderMove moveCounter blacks whites str mode
	| mode == MANVSAI && mod moveCounter 2 == 1 = getMoveFromAi moveCounter blacks whites str mode

--wykonuje ruch AI
getMoveFromAi moveCounter blacks whites str mode = do
	system "clear"
	printGame blacks whites
	putStrLn("move ["++(show moveCounter)++"] "++colors!!(moveCounter `mod` 2)++"s to move, press enter to get ai move")
	c <- getLine
	if(moveCounter `mod` 2 == 0)
		then nextHop (moveCounter+1) (snd wm) (fst wm) str mode
		else nextHop (moveCounter+1) (fst bm) (snd bm) str mode
		where
			wm = (aiMakeMove whites blacks DOWN moveCounter)
			bm = (aiMakeMove blacks whites UP moveCounter)

--'konczy' gre
over = do
	putStrLn("gameOver")

--pyta gracza o ruch
orderMove moveCounter blacks whites str mode = do
	system "clear"
	printGame blacks whites
	putStrLn("move ["++show(moveCounter+1)++"] "++colors!!turn++"s to move")
	putStrLn("-moving:  a-b")
	putStrLn("-beating: axb")
	putStrLn(str)
	move <- getLine
	if (length (splitOn "-" move) /= 2)
		then
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

--wykonuje ruch czarnych
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

--wykonuje bicie czarnych
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

--wykonuje ruch bialych
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

--wykonuje bicie bialych
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

--ustawia pionki i rozpoczyna gre w zadanym trybie
begin mode = do
	system "clear"
	putStrLn("game started")
	printGame blacks whites
	nextHop 0 blacks whites "" mode
		where
			whites = ((zip (take 8 ['a'..]) $take 8 $cycle [2,1])++(zip(take 4 ['b','d','f','h']) $take 4 $cycle [3]))
			blacks = ((zip (take 8 ['a'..]) $take 8 $cycle [8,7])++(zip(take 4 ['a','c','e','g']) $take 4 $cycle [6]))


--pyta o tryb gry
gameStart = do
	putStrLn("welcome!\nchoose game:\n[1] man vs man\n[2] man vs ai\n[3] ai vs man\n[4] ai vs ai")
	game <- getLine
	if game == "1" then begin MANVSMAN else
		if game == "2" then begin MANVSAI else
			if game == "3" then begin AIVSMAN else
				if game == "4" then begin AIVSAI else
				gameStart

