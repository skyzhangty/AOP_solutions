m :: Int -> Int -> Int
m x y
	| x == y = y+1
	| otherwise = m x (m (x - 1) (y +1))

data Nat = Zero | Succ Nat deriving Show
data NatP = One | Succp NatP deriving Show

f :: Nat -> NatP
f Zero = One
f (Succ n) = Succp (f n)

g :: NatP -> Nat
g One = Zero
g (Succp n) = Succ (g n)

foldn :: (a->a) -> a -> Nat -> a
foldn h c Zero = c
foldn h c (Succ n) = h (foldn h c n)

plus :: Nat -> Nat -> Nat
plus m = foldn Succ m

multi :: Nat -> Nat -> Nat
multi m = foldn (plus m) Zero

sqr :: Nat -> Nat
sqr = f . foldn h c
	where 
		c = (Zero, Zero)
		h (n, m) = (Succ n, Succ m)
		f (n, m) = multi n m

lastNat :: (Nat -> Bool)  -> Nat -> Nat
lastNat p = f. foldn h c
	where
		c = (Zero, Zero)
		h (m, n) = if p (Succ m) then (Succ m, Succ m) else (Succ m, n)
		f (m, n) = n 

isEven :: Nat -> Bool
isEven Zero = True
isEven (Succ n) = not (isEven n)

ack :: Nat -> Nat -> Nat
ack Zero n = Succ n
ack (Succ n) Zero = ack n (Succ Zero)
ack (Succ n) (Succ m) = ack n (ack (Succ n) m)

iact :: Int -> Int -> Int
iact 0 n = n + 1
iact n 0 = iact (n - 1) 1
iact n m = iact (n - 1) (iact n (m - 1))

fack :: Nat -> Nat -> Nat
fack = foldn f Succ
	where f = \v -> foldn v (v (Succ Zero))
