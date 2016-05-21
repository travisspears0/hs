import Data.Char

signs = take 8 ['a'..]
numbers = take 8 [1..]
b = createBoard

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
	| number > 8 || number < 0 || sign > 'h' || sign < 'a' = -1
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

--makeMove fromSign fromNumber toSign toNumber board = 
