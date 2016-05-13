{-
data BST key val = 
				Nil
			    | Node key val (BST key val) (BST key val)
			    deriving (Eq, Show, Ord)

insert key val Nil = Node key val Nil Nil
insert key val (Node nkey nval nleft nright)
	| key < nkey = Node nkey nval (insert key val nleft) nright
	| key > nkey = Node nkey nval nleft (insert key val nright)
	| key == nkey = error "the same key"

search key Nil = error "key not found"
search key (Node k v l r)
	| key < k = search key l
	| key > k = search key r
	| key == k = v

testTree = insert 5 5 Nil
a = insert 2 2 testTree
b = insert 3 3 testTree
c = insert 7 7 testTree
-}

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

{-
isBinaryary Nil = False
isBinaryary (Node k Nil Nil) = True
isBinaryary (Node k l r)
	| getK l >= k = False
	| getK r <= k = False
	| otherwise = True
-}

data DIRECTION = NONE | LEFT | RIGHT deriving(Eq,Ord,Show)

_isBinary n = isBinary NONE 0 n
isBinary dir key Nil = True
isBinary dir key (Node k l r) --isBinary l && isBinary r
	| dir == NONE = isBinary LEFT k l && isBinary RIGHT k r
	| dir == LEFT = key > k && isBinary LEFT k l && isBinary RIGHT k r
	| dir == RIGHT = key < k && isBinary LEFT k l && isBinary RIGHT k r

_leavesKeys x = leavesKeys x []
leavesKeys Nil _ = []
leavesKeys (Node k l r) list = (leavesKeys l [])++(leavesKeys r [])++[k]++list

_nnodes x = nnodes x 0
nnodes Nil x = 0
nnodes (Node _ l r) x = 1+x+(nnodes l 0)+(nnodes r 0)

_nsum x = nsum x 0
nsum Nil x = 0
nsum (Node k l r) x = k+x+(nsum l 0)+(nsum r 0)

remove key Nil = error "no such key"
remove key (Node k l r) = 

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

traverse pth Nil list = []
traverse pth (Node k l r) list
	| 
	| 
	| 
	| 
	| 
	| 

--toStrings ...
--tmap ...
--remove ...

t = insert 3 (insert 7 (insert 8 (insert 6 (insert 2 (insert 5 Nil)))))