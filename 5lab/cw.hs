--f a = (a, "[foo]")

f n = n+1
f' n = (f n, show n)
{-
sklej (a,b) = (fst (f a),snd (f a)++b)

sklejinc (a,b) = (fst (f (a+1)), snd (f a)++b)

x = f 5

nic n = (n,"")

makeMeD f = (f,"[checked]")
-}



data Box a = Box a deriving(Show)
data Box' a = Box' a String deriving(Show)

nic a = Box a

x=Box 4

--sklej :: Box a ->(a->Box b)->Box b
sklej (Box arg) f = f arg

--sklej (Box' arg str) f = Box' ret (att++str)
--	where Box' ret att = f arg


{-
	class Box<a> {
		a arg;
		String str;
	}
-}
data No 
data Ma x = J x | No deriving(Show)
