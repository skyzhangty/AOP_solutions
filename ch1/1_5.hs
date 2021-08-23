data Listr a = Nilr | Cons a (Listr a) deriving Show

foldr' :: b -> (a -> b -> b) -> Listr a -> b
foldr' c h Nilr = c
foldr' c h (Cons a xs) = h a (foldr' c h xs)

zip' :: Listr a -> Listr b -> Listr (a, b)
zip' = foldr' c h
	where
		c = const Nilr
		h a acc Nilr = Nilr
		h a acc (Cons b bs) = Cons (a,b) (acc bs)

