main :: IO ()
main = do
	if and tests then
		putStr "Success"
	else
		putStr "Fail"

tests :: [Bool]
tests = [testNand
	, testNot
	, testAnd
	, testOr
	, testXor
	, testNor
	, testImpl
	, testEq
	, testAssociative
	, testCommutative
	, testSplitAnd
	, testEval
	, testSubstitute
	, testDistributive
	, testDeMorgan
	, testAddBuses
	, testAdd
	, testAddCarry
	, testAddWithCarry
	, testMux
	, testDemux
	, testMuxDemux
	, testAndBus
	, testAnd16
	, testNotBus
	, testNot16
	, testOrBus
	, testOr16
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

-- testAssociative semantically proves the associative property of binary operators.
testAssociative :: Bool
testAssociative = and $ map (isAssociative bits) [and_, or_, zero, one, eq, xor]

-- testCommutative semantically proves the commutative property of binary operators.
testCommutative :: Bool
testCommutative = and $ map (isCommutative bits) [and_, or_, zero, one, eq, xor]

-- testDistributive semantically proves the distributive property of pairs of binary operators.
testDistributive :: Bool
testDistributive = and [
	isDistributive bits and_ or_,
	isDistributive bits or_ and_
	]

testDeMorgan :: Bool
testDeMorgan = and [
	isDeMorgan bits not_ or_ and_,
	isDeMorgan bits not_ and_ or_
	]

testSplitAnd :: Bool
testSplitAnd = and [
	splitAnd One One == (One, One),
	splitAnd Zero One == (Zero, Zero),
	splitAnd One Zero == (Zero, Zero),
	splitAnd Zero Zero == (Zero, Zero)
	]

expr1 :: LogicalExpr Char
expr1 = Literal One

expr2 :: LogicalExpr Char
expr2 = BGate xor
		(Literal One)
		(BGate nand
			(Literal Zero)
			(BGate and_
				(UGate not_ (Literal Zero))
				(Literal One)))

expr3 :: LogicalExpr Char
expr3 = Var 'p'

testEval :: Bool
testEval = and [
	eval expr1 == One,
	eval expr2 == Zero
	]

testSubstitute :: Bool
testSubstitute = and [
	eval (sub expr3 dict) == One,
	eval (sub' expr3 dict') == Zero,
	eval (sub' (BGate xor (Literal One) (Var 'p')) dict') == One,
	eval (sub' (BGate xor (Literal Zero) (Var 'p')) dict') == Zero,
	eval (sub' (BGate nand (UGate not_ (Literal Zero)) (Var 'p')) dict') == One
	]
	  where dict = (\c -> case c of
			'p' -> One
			_ -> undefined
			)
		dict' = [('p', Zero)]

testAddBuses :: Bool
testAddBuses = and [
	addBus (take l (repeat One)) (take l (repeat Zero)) == (take l (repeat One), Zero)
	, addBus [Zero] [Zero] == ([Zero], Zero)
	, addBus [One] [Zero] == ([One], Zero)
	, addBus [Zero] [One] == ([One], Zero)
	, addBus [One] [One] == ([Zero], One) -- Overflows.
	, addBus [Zero, One] [Zero, One] == ([One, Zero], Zero)
	, addBus [Zero, Zero, One] [Zero, Zero, One] == ([Zero, One, Zero], Zero)
	, addBus [Zero, One, One] [Zero, One, One] == ([One, One, Zero], Zero)
	, addBus [One, One, Zero] [Zero, One, One] == ([Zero, Zero, One], One) -- Overflows.
	]
	where l = 16

testAdd :: Bool
testAdd = and [
	add Zero Zero == (Zero, Zero),
	add Zero One == (One, Zero),
	add One Zero == (One, Zero),
	add One One == (Zero, One)
	]

testAddWithCarry :: Bool
testAddWithCarry = and [
	addWithCarry Zero Zero Zero == (Zero, Zero),
	addWithCarry Zero Zero One == (One, Zero),
	addWithCarry Zero One Zero == (One, Zero),
	addWithCarry Zero One One == (Zero, One),
	addWithCarry One Zero Zero == (One, Zero),
	addWithCarry One Zero One == (Zero, One),
	addWithCarry One One Zero == (Zero, One),
	addWithCarry One One One == (One, One)
	]

testAddCarry :: Bool
testAddCarry = and [
	addCarry (Zero, Zero) ([], Zero)== ([Zero], Zero),
	addCarry (One, Zero) ([], Zero) == ([One], Zero),
	addCarry (Zero, One) ([], Zero) == ([One], Zero),
	addCarry (Zero, One) ([Zero, One], Zero) == ([One, Zero, One], Zero),
	addCarry (Zero, One) ([], One) == ([Zero], One),
	addCarry (One, One) ([], One) == ([One], One)
	]

testMux :: Bool
testMux = and [
	mux Zero One Zero == Zero
	, mux Zero One One == One
	]

testDemux :: Bool
testDemux = and [
	demux Zero Zero == (Zero, Zero)
	, demux Zero One == (Zero, Zero)
	, demux One Zero == (One, Zero)
	, demux One One == (Zero, One)
	]

testMuxDemux :: Bool
testMuxDemux = and [
	uncurry mux (demux Zero Zero) Zero == Zero
	, uncurry mux (demux One Zero) Zero == One
	, uncurry mux (demux One One) Zero == Zero
	, uncurry mux (demux Zero One) Zero == Zero
	, uncurry mux (demux Zero Zero) One == Zero
	, uncurry mux (demux One Zero) One == Zero
	, uncurry mux (demux One One) One == One
	, uncurry mux (demux Zero One) One == Zero
	]

testAndBus :: Bool
testAndBus = and [
	andBus [Zero] [Zero] == [Zero]
	, andBus [One] [Zero] == [Zero]
	, andBus [One] [One] == [One]
	, andBus [Zero, One, Zero] [One, One, One] == [Zero, One, Zero]
	, andBus [Zero, Zero, One, Zero] [Zero, One, One, One] == [Zero, Zero, One, Zero]
	]

testAnd16 :: Bool
testAnd16 = and [
	and16 [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		[Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		== [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
	, and16 [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		[Zero, Zero, One, One, Zero, Zero, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		== [Zero, Zero, One, Zero, Zero, Zero, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
	]

testNotBus :: Bool
testNotBus = and [
	notBus [Zero] == [One]
	, notBus [One] == [Zero]
	, notBus [One, Zero] == [Zero, One]
	, notBus [Zero, Zero, One, Zero] == [One, One, Zero, One]
	]

testNot16 :: Bool
testNot16 = and [
	not16 [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		== [One, Zero, Zero, One, One, Zero, One, One, Zero, Zero, Zero, Zero, One, Zero, Zero, One]
	, not16 (take 16 ones) == (take 16 zeroes)
	, not16 (take 16 zeroes) == (take 16 ones)
	]

testOrBus :: Bool
testOrBus = and [
	orBus [Zero] [Zero] == [Zero]
	, orBus [One] [Zero] == [One]
	, orBus [Zero, Zero, One, One] [One, Zero, One, Zero] == [One, Zero, One, One]
	]

testOr16 :: Bool
testOr16 = and [
	or16 [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		[Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		== [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
	, or16 [Zero, One, One, Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		[Zero, Zero, One, One, Zero, Zero, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
		== [Zero, One, One, One, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, Zero]
	]

data Bit = Zero | One
	deriving (Eq, Show)

-- bits enumerates the set of values Bit can have.
bits :: [Bit]
bits = [Zero, One]

ones :: [Bit]
ones = One : ones

zeroes :: [Bit]
zeroes = Zero : zeroes

data LogicalExpr a = Literal Bit
	| Var a -- Variable is used to substitute for a Bit.
	| BGate BinaryGate (LogicalExpr a) (LogicalExpr a)
	| UGate UnaryGate (LogicalExpr a)

eval :: LogicalExpr a -> Bit
eval (Literal b) = b
eval (Var x) = undefined -- Cannot evaluate a non-substituted variable.
eval (BGate gate l r) = gate (eval l) (eval r)
eval (UGate gate e) = gate (eval e)

-- sub substitutes the variables in a LogicalExpr with a mapping function.
sub :: LogicalExpr a -> (a -> Bit) -> LogicalExpr a
sub (Var x) f = Literal (f x)

-- sub' substitutes the variables in a LogicalExpr with a list of pairs.
sub' :: Eq a => LogicalExpr a -> [(a, Bit)] -> LogicalExpr a
sub' (Literal b) _ = Literal b
sub' (Var x) vs = if null thing
			then Var x
			else Literal (head thing)
	where thing = [bit | (var, bit) <- vs, var == x]
sub' (UGate g e) vs = UGate g (sub' e vs)
sub' (BGate g l r) vs = BGate g (sub' l vs) (sub' r vs)

type UnaryGate = Bit -> Bit

type BinaryGate = Bit -> Bit -> Bit

type Splitter = Bit -> (Bit, Bit)

-- split splits the signal to two identical outgoing signals.
split :: Splitter
split x = (x, x)

-- nand is the only primitive that is defined by matching on its arguments.
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

mux :: Bit -> Bit -> Bit -> Bit
mux x y sel = if sel == Zero then x else y

demux :: Bit -> Bit -> (Bit, Bit)
demux x sel = if sel == Zero then (x, Zero) else (Zero, x)

orBus :: [Bit] -> [Bit] -> [Bit]
orBus xs ys = [or_ x y | (x, y) <- zip xs ys]

or16 :: [Bit] -> [Bit] -> [Bit]
or16 xs ys | length xs == 16 && length ys == 16 = orBus xs ys
	    | otherwise = undefined

andBus :: [Bit] -> [Bit] -> [Bit]
andBus xs ys = [and_ x y | (x, y) <- zip xs ys]

and16 :: [Bit] -> [Bit] -> [Bit]
and16 xs ys | length xs == 16 && length ys == 16 = andBus xs ys
	    | otherwise = undefined

notBus :: [Bit] -> [Bit]
notBus = map not_

not16 :: [Bit] -> [Bit]
not16 xs | length xs == 16 = notBus xs
	    | otherwise = undefined

-- xor' is an equivalent implementation of xor, with named functions for the
-- edges that symbolise connections, similar to notation in HDL.
xor' :: BinaryGate
xor' a b = or_ aAndNotb notaAndb
	where
		aAndNotb = and_ a notb
		notaAndb = and_ nota b
		notb = not_ b
		nota = not_ a

isAssociative :: Eq a => [a] -> (a -> a -> a) -> Bool
isAssociative xs f = and
	[associative f x y z| x <- xs, y <- xs, z <- xs]

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = f (f x y) z == f x (f y z)

isCommutative :: Eq a => [a] -> (a -> a -> a) -> Bool
isCommutative xs f = and
	[commutative f x y | x <- xs, y <- xs]

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative f x y = f x y == f y x

isDistributive :: Eq a => [a] -> (a -> a -> a) -> (a -> a -> a) -> Bool
isDistributive xs f g = and
	[distributive f g x y z | x <- xs, y <- xs, z <- xs]

distributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> a -> a -> a -> Bool
distributive f g x y z = f x (g y z) == g (f x y) (f x z)

isDeMorgan :: Eq a => [a] -> (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Bool
isDeMorgan xs u f g = and
	[deMorgan u f g x y | x <- xs, y <- xs]

deMorgan :: Eq a => (a -> a) -> (a -> a -> a) -> (a -> a -> a) -> a -> a -> Bool
deMorgan u f g x y = u (f x y) == g (u x) (u y)

splitAnd :: Bit -> Bit -> (Bit, Bit)
splitAnd l r = composeBGateSplitter and_ split l r

composeBGateSplitter :: BinaryGate -> Splitter -> (Bit -> Bit -> (Bit, Bit))
composeBGateSplitter bg s l r = s (bg l r)

-- add adds two Bits and returns the result and a carry Bit.
add :: Bit -> Bit -> (Bit, Bit)
add = addWithCarry Zero

-- addWithCarry adds three Bits (two from input and a third from the previous 
-- carry) and returns the result and a carry Bit.
addWithCarry :: Bit -> Bit -> Bit -> (Bit, Bit)
addWithCarry x y z = (thisDigit, carry)
	where
		thisDigit = xor x (xor y z)
		carry = or_ (or_ (and_ x y) (and_ x z)) (and_ y z)

-- addBus adds two buses of Bits and returns a Bit signaling overflow.
addBus :: [Bit] -> [Bit] -> ([Bit], Bit)
addBus xs ys = zs
	where
		xs' = xs
		ys' = ys
		zs = foldr addCarry acc (zip xs' ys')
		acc = ([], Zero)

addCarry :: (Bit, Bit) -> ([Bit], Bit) -> ([Bit], Bit)
addCarry (x, y) (list, prevCarry) = (s:list, newCarry)
	where
		(s, newCarry) = addWithCarry x y prevCarry
