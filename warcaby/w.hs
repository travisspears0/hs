{-data Paw sign number color = Paw Char Int String deriving(Show)

getSign (Paw s n c) = s
getNumber (Paw s n c) = n
getColor (Paw s n c) = c
-}
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

data DIRECTION = UP | DOWN deriving(Show)

hasPawBeat paws opponentPaws sgn num dir
	| 	dir == UP &&
		isFieldBusy opponentPaws (chr ((ord cs)-1)) (num+1) &&
		not isFieldBusy (paws++opponentPaws) (chr ((ord cs)-2)) (num+2) = True
	| 	dir == UP &&
		isFieldBusy opponentPaws (chr ((ord cs)-1)) (num-1) &&
		not isFieldBusy (paws++opponentPaws) (chr ((ord cs)-2)) (num-2) = True
	| dir == DOWN = 

{-
findOptions paws sgn num dir
	| dir == UP = 
	| dir == DOWN = 
-}