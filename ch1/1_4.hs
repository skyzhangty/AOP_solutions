data Tree a = Tip a | Bin (Tree a) (Tree a)

foldt :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldt f g (Tip a) = f a
foldt f g (Bin a b) = g (foldt f g a) (foldt f g b)

mapt :: (a -> b) -> Tree a -> Tree b
mapt f = foldt (Tip . f) Bin

size :: Tree a -> Int
size = foldt (const 1) (+)

depth :: Tree a -> Int
depth = foldt (const 0) (\x y -> max x y + 1)

data Listl a = Nill | Snoc (Listl a) a
data Gtree a = Node a (Listl (Gtree a))

foldl' :: (b -> a -> b) -> b -> Listl a -> b
foldl' h c Nill = c
foldl' h c (Snoc xs x) = h (foldl' h c xs) x

map' :: (a -> b) -> Listl a -> Listl b
map' f = foldl' (\ys x -> Snoc ys (f x)) Nill 

foldg :: (a -> b -> b) -> b -> (b -> b -> b) -> b ->Gtree a -> b
foldg g c h d (Node x Nill) = c
foldg g c h d (Node x subtrees) = g x (foldl' h d (map' (foldg g c h d) subtrees))

sizeg :: Gtree a -> Int
sizeg = foldg (\_ sum -> sum + 1) 1 (+) 0

depthg :: Gtree a -> Int
depthg = foldg (\_ sum -> sum + 1) 0 max 0

