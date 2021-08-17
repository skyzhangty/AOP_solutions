data Listr a = Nilr | Cons a (Listr a) deriving Show
data Listl a = Nill | Snoc (Listl a) a deriving Show

convert :: Listl a -> Listr a
convert Nill = Nilr
convert (Snoc x y) = snocr (convert x) y
	where
		snocr Nilr y = Cons y Nilr
		snocr (Cons x xs) y = Cons x (snocr xs y)

foldr' :: b -> (a -> b -> b) -> Listr a -> b
foldr' c h Nilr = c
foldr' c h (Cons x xs) = h x (foldr' c h xs)

foldl' :: b -> (b -> a -> b) -> Listl a -> b
foldl' c h Nill = c
foldl' c h (Snoc xs x) = h (foldl' c h xs) x

cat :: Listl a -> Listl a -> Listl a
cat xs = foldl' xs Snoc

concat' = foldr' Nill cat

convertF :: Listl a -> Listr a
convertF = foldl' c h
	where
		c = Nilr
		h = snocr
		snocr Nilr y = Cons y Nilr
		snocr (Cons x xs) y = Cons x (snocr xs y)

convert' :: Listl a -> Listr a -> Listr a
convert' Nill ys = ys
convert' (Snoc xs x) ys = convert' xs (Cons x ys)

convertLinear :: Listl a -> Listr a
convertLinear xs = convert' xs Nilr

catr :: Listr a -> Listr a -> Listr a
catr xs Nilr = xs
catr xs (Cons y ys) = catr (snocr xs y) ys
	where
		snocr Nilr y = Cons y Nilr
		snocr (Cons x xs) y = Cons x (snocr xs y)

foldli :: b -> (b -> a -> b) -> Listr a -> b
foldli c h Nilr = c
foldli c h (Cons x xs) = foldr' (h c x) (\i acc -> h acc i) xs

length' :: Listr a -> Int
length' = foldr' 0 (\_ n -> n+1)

take' :: Int -> Listr a -> Listr a
take' n x = (foldr' c h) x n
	where
		c = \_ -> Nilr
		h i acc 0 = Nilr
		h i acc n = Cons i (acc (n-1))

drop' :: Int -> Listr a -> Listr a
drop' n x = (foldr' c h) x n
	where
		c = \_ -> Nilr
		h i acc 0 = Cons i (acc 0)
		h i acc n = acc (n - 1)

