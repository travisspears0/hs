m = ml 0 []
ml c l
	| c<10 = ml (c+1) (c:l)
	| otherwise = l

z n
	| n<10 = Just (n*2) 
	| otherwise = Nothing

a = do
	x <- getLine
	putStrLn(x++" hehe")

rev str = rev' str []
rev' [] res = []
rev' str res = (last str):rev' (take (length str-1) str) res

data Tree a = E | L a | N (Tree a) a (Tree a)
	deriving (Eq, Show, Ord)



