import Data.Char
import System.Random

signs = take 8 ['a'..]
numbers = take 8 [1..]
b = createBoard
bb = prepareGame b

indexOf el list = indexOf' el list 0
indexOf' el list curr
	| curr >= length list = -1
	| el == list!!curr = curr
	| otherwise = indexOf' el list (curr+1)

createBoard :: [[Int]]
createBoard = createBoard' 0 []
createBoard' n list
	| n >= length numbers = []
	| otherwise = take 8 (repeat 0):(createBoard' (n+1) [])

getFieldValue sign number board
	| number > 7 || number < 0 || sign > 'h' || sign < 'a' = -1
	| otherwise = board!!(indexOf sign signs)!!number

setFieldValue sign number board newValue = 
		setFieldValueSgn sign number board newValue (ord 'a')

setFieldValueSgn sgn num (b:brd) val currSgn
	| ord sgn == currSgn = (setFieldValueNum num b val 0):brd
	| otherwise = b:(setFieldValueSgn sgn num brd val (currSgn+1))

setFieldValueNum num (b:brd) val currNum
	| currNum == num = val:brd
	| otherwise = b:setFieldValueNum num brd val (currNum+1)

prepareGame board = prepareGame' board 1 0
prepareGame' board currPlayer currPaw
	| currPlayer <= 2 && currPaw < 8 =
		prepareGame' (setFieldValue (chr (97+currPaw)) 
		((currPlayer-1)*6 + mod currPaw 2) board currPlayer) 
		currPlayer (currPaw+1)
	| currPlayer <= 2 && currPaw >= 8 = 
		prepareGame' board (currPlayer+1) 0
	| currPlayer > 2 = board

getGameValue player board = getGameValue' player board 'a' 0
getGameValue' pl brd currSgn currNum
	| val == -1 && currSgn > 'h' = 0
	| val == -1 && currNum > 7 = getGameValue' pl brd (chr ((ord currSgn)+1)) 0
	| val == 0 = getGameValue' pl brd currSgn (currNum+1)
	| val == pl = 1 + getGameValue' pl brd currSgn (currNum+1)
	| val /= pl = getGameValue' pl brd currSgn (currNum+1) - 1
	where val = getFieldValue currSgn currNum brd

makeMove player fromSign fromNumber toSign toNumber board
	| valFrom == player || valTo == 0 = 
		setFieldValue fromSign fromNumber (setFieldValue toSign toNumber board player) 0
	| otherwise = error "cannot move!"
	where
		valFrom = getFieldValue fromSign fromNumber board
		valTo = getFieldValue toSign toNumber board
{-
--zwraca liste krotek(2) z mozliwosciami ruchu i wartosciami gry
--po kazdym z nich
getPossibleMoves currSgn currNum board moves
	|  = 
	|  = 
	where 
-}

getPawsPositions player board = getPawsPositions' player board 'a' 0 []
getPawsPositions' pl brd cs cn list
	| val == -1 && cs > 'h' = []
	| val == -1 && cn > 7 = getPawsPositions' pl brd (chr ((ord cs)+1)) 0 list
	| val /= pl = getPawsPositions' pl brd cs (cn+1) list
	| val == pl = [(cs,cn)]:getPawsPositions' pl brd cs (cn+1) list
	where val = getFieldValue cs cn brd

getPawMoves player board pawSign pawNum
	| getFieldValue pawSign pawNum board /= player = error "no such paw here"
	| otherwise = getPawMoves' player board pawSign pawNum [] 0
getPawMoves' pl brd ps pn list jumpCount = 
	| pl == 1 && getFieldValue (chr ((ord ps)+1)) (pn+1) brd == -1 = []
	| pl == 1 && getFieldValue (chr ((ord ps)+1)) (pn-1) brd == -1 = []
	| pl == 1 && getFieldValue (chr ((ord ps)+1)) (pn-1) brd == -1 = []
	| pl == 1 &&  = 
	| pl == 2 &&  = 
	| pl == 2 &&  = 
