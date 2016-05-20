import Data.List

data BOARD cols rows fields = Board cols rows fields deriving(Show)

placeQueen col row (Board c r f)
	| row >= r || col >= c = error "field beyond the board"
	| otherwise =  Board c r ((col,row):f)

--1 gdy zajete, 0 gdy wolne
_isFieldFree col row b = isFieldFree col row 0 b
isFieldFree col row curr (Board c r f)
	| col >= c || row >= r = error "field beyond the board"
	| curr >= length f = 0
	| fst (f!!curr) == col && snd (f!!curr) == row = 1
	| otherwise = isFieldFree col row (curr+1) (Board c r f)

b = placeQueen 2 2 (placeQueen 0 3 (placeQueen 1 1 (Board 4 4 [])))
c = placeQueen 2 0 b
t = placeQueen 3 3 (placeQueen 2 1 (Board 4 4 []))

--sprawdza wiersz i zwraca ile wystapilo na nim hetmanow
_checkRow row b = checkRow 0 row b
checkRow currCol row (Board c r f)
	| currCol >= c = 0
	| otherwise =  (_isFieldFree currCol row (Board c r f))+(checkRow (currCol+1) row (Board c r f))

--sprawdza kolumne i zwraca ile wystapilo na nim hetmanow
_checkCol col b = checkCol col 0 b
checkCol col currRow (Board c r f)
	| currRow >= r = 0
	| otherwise =  (_isFieldFree col currRow (Board c r f))+(checkCol col (currRow+1) (Board c r f))	

--czy wiersz, w ktorym jest dany hetman nie koliduje z innymi
_checkQueen queenIndex b = checkQueen queenIndex 0 b
checkQueen queenIndex currIndex (Board c r f)
	| currIndex >= length f = 
		True
	| diff && rcur == rque = 
		False
	| diff && ccur == cque = 
		False
	| diff && abs(ccur-cque) == abs(rcur-rque) =
		False
	| otherwise = checkQueen queenIndex (currIndex+1) (Board c r f)
	where
		diff = currIndex /= queenIndex
		ccur = fst(f!!currIndex)
		rcur = snd(f!!currIndex)
		cque = fst(f!!queenIndex)
		rque = snd(f!!queenIndex)

--sprawdza czy wszystkie hetmany sa ok
_queens b = queens 0 b
queens curr (Board c r f)
	| curr >= length f = True
	| otherwise = _checkQueen curr (Board c r f) && queens (curr+1) (Board c r f)