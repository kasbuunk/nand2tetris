main :: IO ()
main = do
	if and tests then
		putStr "Success"
	else
		putStr "Fail"

tests :: [Bool]
tests = [testNand,
	testNot,
	testAnd,
	testOr,
	testXor,
	testImpl,
	testEq,
	testAssociative,
	testCommutative
	]

testNand :: Bool
testNand = and [
	nand Zero Zero == One,
	nand Zero One == One,
	nand One Zero == One,
	nand One One == Zero
	]

testNot :: Bool
testNot = and [
	not_ One == Zero,
	not_ Zero == One
	]

testAnd :: Bool
testAnd = and [
	and_ One One == One,
	and_ One Zero == Zero,
	and_ Zero One == Zero,
	and_ Zero Zero == Zero
	]

testOr :: Bool
testOr = and [
	or_ One One == One,
	or_ One Zero == One,
	or_ Zero One == One,
	or_ Zero Zero == Zero
	]

testXor :: Bool
testXor = and [
	xor One One == Zero,
	xor One Zero == One,
	xor Zero One == One,
	xor Zero Zero == Zero
	]

testImpl :: Bool
testImpl = and [
	impl One One == One,
	impl One Zero == Zero,
	impl Zero One == One,
	impl Zero Zero == One
	]

testEq :: Bool
testEq = and [
	eq One One == One,
	eq One Zero == Zero,
	eq Zero One == Zero,
	eq Zero Zero == One
	]

testAssociative :: Bool
testAssociative = and $ map (flip isAssociative bits) [and_, or_, zero, one]

testCommutative :: Bool
testCommutative = and $ map (flip isCommutative bits) [and_]

data Bit = Zero | One
	deriving (Show)

instance Eq Bit where
	Zero == Zero = True
	One == One = True
	_ == _ = False

bits :: [Bit]
bits = [Zero, One]

type Gate = Bit -> Bit -> Bit

nand :: Gate
nand One One = Zero
nand _ _ = One

not_ :: Bit -> Bit
not_ b = nand b b

and_ :: Gate
and_ l r = not_ (nand l r)

or_ :: Gate
or_ l r = not_ (and_ (not_ l ) (not_ r))

xor :: Gate
xor l r = or_ (and_ l (not_ r)) (and_ (not_ l) r)

impl :: Gate
impl l r = or_ (not_ l) r

eq :: Gate
eq l r = or_ (and_ l r) (and_ (not_ l) (not_ r))

zero :: Gate
zero l _ = and_ l (not_ l)

one :: Gate
one l _ = or_ l (not_ l)

xor' :: Gate
xor' a b = or_ aAndNotb notaAndb
	where
		aAndNotb = and_ a notb
		notaAndb = and_ nota b
		notb = not_ b
		nota = not_ a

isAssociative :: Eq a => (a -> a -> a) -> [a] -> Bool
isAssociative f xs = and
	[associative f x y z| x <- xs, y <- xs, z <- xs]

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = f (f x y) z == f x (f y z)

isCommutative :: Eq a => (a -> a -> a) -> [a] -> Bool
isCommutative f xs = and
	[commutative f x y | x <- xs, y <- xs]

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative f x y = f x y == f y x
