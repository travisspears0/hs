import System.Random

data Box a = Box a deriving(Show)

fm f (Box a) = Box(f a)
fs (Box a) (Box b) = Box(a b)

mn :: (t -> a) -> Box t -> Box a
mn f (Box a) = Box(f a)

x = Box 5

f $$ a = f a

rev [] = []
rev (a:b) = (rev (b))++[a]

m = do
	a <- getLine
	b <- getLine
	c <- getLine
	putStrLn(rev(a) ++ " " ++ rev(b) ++ " " ++ rev(c))

f $$$ g = f (g x)
