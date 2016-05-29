mini = -1 :: Integer
data BST key = 
	Nil
	| Node key (BST key) (BST key)
	deriving (Eq, Show, Ord)

insert key Nil = Node key Nil Nil
insert key (Node k l r)
	| key > k		= Node k l (insert key r)
	| key < k		= Node k (insert key l) r
	| key == k		= error "the same key"

maxt :: BST Integer -> Integer
maxt Nil = mini
maxt (Node k l r) = max k (max y z)
	where
		y = maxt l
		z = maxt r

empty Nil = True
empty _ = False

getHead Nil = error "no keys"
getHead (Node k l r) = k

data DIRECTION = NONE | LEFT | RIGHT deriving(Eq,Ord,Show)

_isBinary n = isBinary NONE 0 n
isBinary dir key Nil = True
isBinary dir key (Node k l r) --isBinary l && isBinary r
	| dir == NONE = isBinary LEFT k l && isBinary RIGHT k r
	| dir == LEFT = key > k && isBinary LEFT k l && isBinary RIGHT k r
	| dir == RIGHT = key < k && isBinary LEFT k l && isBinary RIGHT k r

_leavesKeys x = leavesKeys x []
leavesKeys Nil _ = []
leavesKeys (Node k l r) list = list++(leavesKeys l [])++(leavesKeys r [])++[k]
--			++[(Node k l r)]

_leaves x = leaves x []
leaves Nil _ = []
leaves (Node k l r) list = list++(leaves l [])++(leaves r [])++[(Node k l r)]

_nnodes x = nnodes x 0
nnodes Nil x = 0
nnodes (Node _ l r) x = 1+x+(nnodes l 0)+(nnodes r 0)

_nsum x = nsum x 0
nsum Nil x = 0
nsum (Node k l r) x = k+x+(nsum l 0)+(nsum r 0)

search key Nil = False
search key (Node k l r)
	| key == k = True
	| otherwise = search key l || search key r

isBal Nil = 1
isBal (Node _ l r) = isBal l - isBal r
isBalanced x
	| isBal x == 0 = True
	| otherwise = False

data PATH = VLR | VRL | LVR | RVL | RLV | LRV deriving(Show,Ord,Eq)

_traverse pth x = traverse pth x []
traverse pth Nil list = []
traverse pth (Node k l r) list = case pth of
	VLR -> list++[k]++(traverse pth l [])++(traverse pth r [])
	VRL -> list++[k]++(traverse pth r [])++(traverse pth l [])
	LVR -> list++(traverse pth l [])++[k]++(traverse pth r [])
	RVL -> list++(traverse pth r [])++[k]++(traverse pth l [])
	RLV -> list++(traverse pth r [])++(traverse pth l [])++[k]
	LRV -> list++(traverse pth l [])++(traverse pth r [])++[k]

remove key Nil = Nil
remove key (Node k l r)
	| key > k						= Node k l (remove key r)
	| key < k						= Node k (remove key l) r
	| key == k && l/=Nil			= l
	| key == k && r/=Nil			= r
	| key == k && l==Nil && r==Nil	= Nil

--toStrings ...
--tmap ...
--remove ...

{-
remove key Nil = error "no such key"
remove key (Node k l r) = 
-}

t = insert 3 (insert 7 (insert 8 (insert 6 (insert 2 (insert 5 Nil)))))

{-
		LAB 3
-}

_getLevel lvl x = getLevel lvl 0 x []
getLevel lvl curr Nil list = []
getLevel lvl curr (Node k l r) list
	| lvl == curr = list++[k]++(getLevel lvl (curr+1) l [])++(getLevel lvl (curr+1) r [])
	| otherwise = list++(getLevel lvl (curr+1) l [])++(getLevel lvl (curr+1) r [])

_indexOf el list = indexOf el 0 list
indexOf el curr [] = error "empty list"
indexOf el curr list
	| curr >= length list = error "element not found"
	| el == list!!curr = curr
	| otherwise = indexOf el (curr+1) list

_makeLayout x = makeLayout x [] 0 x
makeLayout Nil list depth tree = []
makeLayout (Node k l r) list depth tree = 
	list++(makeLayout l [] (depth+1) tree)++
	[(k,_indexOf k (_traverse LVR tree),depth)]++
	(makeLayout r [] (depth+1) tree)

--dumpDOT
--enumerateLevel