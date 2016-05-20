m = ml 0 []
ml c l
	| c<10 = ml (c+1) (c:l)
	| otherwise = l