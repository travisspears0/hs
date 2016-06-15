module Ai where

import Data.Char
import Data.List.Split
import System.Random
import System.Process
import Helping

--wyszukuje zbior ruchow rownowaznych o najwyzszym priorytecie czyli najpierw szuka bic
--a jak nie ma bic to szuka zywklych ruchow
--zwraca krotke: (paws,opponentpaws)
aiMakeMove paws opponentPaws dir movesCounter
	| fst beatchosen /= '0' = aiBeat paws opponentPaws (tail (snd gpb)) (head (snd gpb))
	| otherwise = (movePaw paws opponentPaws (fst moveChosen) (snd moveChosen) (fst gpm) (snd gpm),opponentPaws)
	where
		moveChosen = aiChooseMove paws opponentPaws dir movesCounter
		beatchosen = aiChooseBeat paws opponentPaws dir movesCounter
		gpm = getPawMove paws opponentPaws (fst moveChosen) (snd moveChosen) dir
		gpb = getPawBeats paws opponentPaws dir (fst beatchosen) (snd beatchosen)

--wykonuje bicie/a ai
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

--wybiera najlepszy ruch a jak jest pare mozliwych to losuje
aiChooseMove paws opponentPaws dir movesCounter
	| length pm == 0 = ('0',0)
	| otherwise = getRandomElement (aiChooseMove' paws opponentPaws dir pm (minBound::Int) []) movesCounter
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

--sprawdza czy gra sie nie skonczyla
isGameOver blacks whites = length blacks == 0 || length whites == 0
