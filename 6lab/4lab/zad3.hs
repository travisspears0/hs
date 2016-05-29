--sum
--product
--reverse
--and
--or
--head
--last

l = take 7 [7..]
b = [True,True,False]

summ l = foldl (+) 0 l
prod = foldl (*) 1
andd = foldl (&&) True
orr = foldl (||) False
headd l = foldl1 (\a x -> a) l
lastt l = foldl1 (\a x -> x) l
reversee l = foldl (\a b -> b:a) [] l

head' l = foldr (\a x -> a) 0 l
last' l = foldl (\a x -> x) 0 l

maxx :: (Ord a) => [a] -> a
maxx = foldl1 (\a b -> if a > b then a else b )