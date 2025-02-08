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
	testNor,
	testImpl,
	testEq,
	testAssociative,
	testCommutative,
	testSplitAnd
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

testNor :: Bool
testNor = and [
	nor One One == Zero,
	nor One Zero == Zero,
	nor Zero One == Zero,
	nor Zero Zero == One
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
testAssociative = and $ map (flip isAssociative bits) [and_, or_, zero, one, eq, xor]

testCommutative :: Bool
testCommutative = and $ map (flip isCommutative bits) [and_, or_, zero, one, eq, xor]

testSplitAnd :: Bool
testSplitAnd = and [
	splitAnd One One == (One, One),
	splitAnd Zero One == (Zero, Zero),
	splitAnd One Zero == (Zero, Zero),
	splitAnd Zero Zero == (Zero, Zero)
	]

data Bit = Zero | One
	deriving (Eq, Show)

type UnaryGate = Bit -> Bit

type BinaryGate = Bit -> Bit -> Bit

bits :: [Bit]
bits = [Zero, One]

split :: Bit -> (Bit, Bit)
split x = (x, x)

nand :: BinaryGate
nand One One = Zero
nand _ _ = One

not_ :: UnaryGate
not_ b = nand b b

and_ :: BinaryGate
and_ l r = not_ (nand l r)

or_ :: BinaryGate
or_ l r = not_ (and_ (not_ l ) (not_ r))

xor :: BinaryGate
xor l r = or_ (and_ l (not_ r)) (and_ (not_ l) r)

nor :: BinaryGate
nor l r = and_ (not_ l) (not_ r)

impl :: BinaryGate
impl l r = or_ (not_ l) r

eq :: BinaryGate
eq l r = or_ (and_ l r) (and_ (not_ l) (not_ r))

zero :: BinaryGate
zero l _ = and_ l (not_ l)

one :: BinaryGate
one l _ = or_ l (not_ l)

xor' :: BinaryGate
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

splitAnd :: Bit -> Bit -> (Bit, Bit)
splitAnd l r = split (and_ l r)
