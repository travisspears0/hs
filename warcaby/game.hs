signs = take 8 ['a'..]
numbers = take 8 [1..]
b = createBoard

indexOf el list = indexOf' el list 0
indexOf' el list curr
	| curr >= length list = -1
	| el == list!!curr = curr
	| otherwise = indexOf' el list (curr+1)

createBoard = createBoard' 0 []
createBoard' n list
	| n >= length numbers = []
	| otherwise = take 8 (repeat 0):(createBoard' (n+1) [])

getFieldValue sign number board
	| number > 8 || number < 0 || sign > 'h' || sign < 'a' = -1
	| otherwise = board!!(indexOf sign signs)!!number

--rozmieszcza pionki na planszy
prepareGame board = prepareGame' board 1
prepareGame' board player currentPaw
	| 
	| 
	| 
