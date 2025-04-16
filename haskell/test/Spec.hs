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
	, testHalfAdder
	, testAddCarry
	, testFullAdder
	, testMux
	, testDemux
	, testMuxDemux
	, testAndBus
	, testAnd16
	, testNotBus
	, testNot16
	, testOrBus
	, testOr16
	, testMux16
	, testOr8Way
	, testMux4Way16
	, testMux8Way16
	, testDmux4Way
	, testDmux8Way
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

testHalfAdder :: Bool
testHalfAdder = and [
	halfAdder Zero Zero == (Zero, Zero),
	halfAdder Zero One == (One, Zero),
	halfAdder One Zero == (One, Zero),
	halfAdder One One == (Zero, One)
	]

testFullAdder :: Bool
testFullAdder = and [
	fullAdder Zero Zero Zero == (Zero, Zero),
	fullAdder Zero Zero One == (One, Zero),
	fullAdder Zero One Zero == (One, Zero),
	fullAdder Zero One One == (Zero, One),
	fullAdder One Zero Zero == (One, Zero),
	fullAdder One Zero One == (Zero, One),
	fullAdder One One Zero == (Zero, One),
	fullAdder One One One == (One, One)
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

testMux16 :: Bool
testMux16 = and [
	mux16 zero16 zero16 Zero == zero16
	, mux16 zero16 zero16 One == zero16
	, mux16 zero16 arbitraryString16_0 Zero == zero16
	, mux16 zero16 arbitraryString16_0 One == arbitraryString16_0
	, mux16 arbitraryString16_0 zero16 Zero == arbitraryString16_0
	, mux16 arbitraryString16_0 zero16 One == zero16
	, mux16 arbitraryString16_0 arbitraryString16_1 Zero == arbitraryString16_0
	, mux16 arbitraryString16_0 arbitraryString16_1 One == arbitraryString16_1
	]
	where

testOr8Way :: Bool
testOr8Way = and [
	or8Way zero8 == Zero
	, or8Way one8 == One
	, or8Way arbitraryString8 == One
	, or8Way arbitraryString8' == One
	]

testMux4Way16 :: Bool
testMux4Way16 = and [
	muxApplied Zero Zero == arbitraryString16_0
	, muxApplied Zero One == arbitraryString16_1
	, muxApplied One Zero == arbitraryString16_2
	, muxApplied One One == arbitraryString16_3
	]
	where
		muxApplied = mux4Way16 arbitraryString16_0 arbitraryString16_1 arbitraryString16_2 arbitraryString16_3

testMux8Way16 :: Bool
testMux8Way16 = and [
	muxApplied Zero Zero Zero == arbitraryString16_0
	, muxApplied Zero Zero One == arbitraryString16_1
	, muxApplied Zero One Zero == arbitraryString16_2
	, muxApplied Zero One One == arbitraryString16_3
	, muxApplied One Zero Zero == arbitraryString16_4
	, muxApplied One Zero One == arbitraryString16_5
	, muxApplied One One Zero == arbitraryString16_6
	, muxApplied One One One == arbitraryString16_7
	]
	where
		muxApplied = mux8Way16 
			arbitraryString16_0
			arbitraryString16_1
			arbitraryString16_2
			arbitraryString16_3
			arbitraryString16_4
			arbitraryString16_5
			arbitraryString16_6
			arbitraryString16_7

testDmux4Way :: Bool
testDmux4Way = and [
	dmux4Way Zero Zero Zero == (Zero, Zero, Zero, Zero)
	, dmux4Way Zero Zero One == (Zero, Zero, Zero, Zero)
	, dmux4Way Zero One Zero == (Zero, Zero, Zero, Zero)
	, dmux4Way Zero One One == (Zero, Zero, Zero, Zero)
	, dmux4Way One Zero Zero == (One, Zero, Zero, Zero)
	, dmux4Way One Zero One == (Zero, One, Zero, Zero)
	, dmux4Way One One Zero == (Zero, Zero, One, Zero)
	, dmux4Way One One One == (Zero, Zero, Zero, One)
	]

testDmux8Way :: Bool
testDmux8Way = and [
	dmux8Way Zero Zero Zero Zero == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero Zero Zero One == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero Zero One Zero == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero Zero One One == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero One Zero Zero == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero One Zero One == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero One One Zero == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way Zero One One One == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way One Zero Zero Zero == (One, Zero, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way One Zero Zero One == (Zero, One, Zero, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way One Zero One Zero == (Zero, Zero, One, Zero, Zero, Zero, Zero, Zero)
	, dmux8Way One Zero One One == (Zero, Zero, Zero, One, Zero, Zero, Zero, Zero)
	, dmux8Way One One Zero Zero == (Zero, Zero, Zero, Zero, One, Zero, Zero, Zero)
	, dmux8Way One One Zero One == (Zero, Zero, Zero, Zero, Zero, One, Zero, Zero)
	, dmux8Way One One One Zero == (Zero, Zero, Zero, Zero, Zero, Zero, One, Zero)
	, dmux8Way One One One One == (Zero, Zero, Zero, Zero, Zero, Zero, Zero, One)
	]

zero16 = take 16 zeroes
one16 = take 16 ones
arbitraryString16_0 = [Zero, Zero, Zero, One, Zero, Zero, One, Zero, Zero, Zero, One, Zero, Zero, Zero, Zero, One]
arbitraryString16_1 = [One, Zero, One, Zero, Zero, One, One, Zero, One, One, One, One, One, Zero, Zero, One]
arbitraryString16_2 = [Zero, Zero, One, Zero, One, Zero, One, Zero, One, One, One, One, One, Zero, One, One]
arbitraryString16_3 = [One, Zero, One, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, One, Zero, One, Zero, One]
arbitraryString16_4 = [Zero, Zero, Zero, One, One, Zero, One, Zero, Zero, Zero, One, One, Zero, Zero, Zero, One]
arbitraryString16_5 = [Zero, Zero, One, Zero, Zero, One, One, One, One, Zero, One, One, One, Zero, Zero, One]
arbitraryString16_6 = [Zero, One, One, Zero, One, Zero, One, Zero, One, One, Zero, Zero, One, One, One, One]
arbitraryString16_7 = [One, Zero, One, One, Zero, Zero, Zero, Zero, Zero, One, Zero, Zero, One, One, Zero, One]

zero8 = take 8 zeroes
one8 = take 8 ones
arbitraryString8 = take 8 arbitraryString16_0
arbitraryString8' = take 8 arbitraryString16_1

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
or_ l r = nand (not_ l) (not_ r)

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
mux x y sel = or_ (and_ x (not_ sel)) (and_ y sel)

demux :: Bit -> Bit -> (Bit, Bit)
demux x sel = (and_ x (not_ sel), and_ x sel)

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

muxBus :: [Bit] -> [Bit] -> Bit -> [Bit]
muxBus xs ys sel = [mux x y sel | (x, y) <- zip xs ys]

mux16 :: [Bit] -> [Bit] -> Bit -> [Bit]
mux16 xs ys sel | length xs == 16 = muxBus xs ys sel
	        | otherwise = undefined

orFold :: [Bit] -> Bit
orFold = foldl or_ Zero

or8Way :: [Bit] -> Bit
or8Way xs | length xs == 8 = orFold xs
	  | otherwise = undefined

mux4WayBus :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> Bit -> [Bit]
mux4WayBus xs ys zs us selb sela = muxBus m1 m2 selb
	where
		m1 = muxBus xs ys sela
		m2 = muxBus zs us sela

mux4Way16 :: [Bit] -> [Bit] -> [Bit] -> [Bit] -> Bit -> Bit -> [Bit]
mux4Way16 = mux4WayBus

mux8WayBus :: [Bit] -> [Bit] -> [Bit] -> [Bit]
	-> [Bit] -> [Bit] -> [Bit] -> [Bit]
	-> Bit -> Bit -> Bit -> [Bit]
mux8WayBus as bs cs ds es fs gs hs sel3 sel2 sel1 = muxBus m1 m2 sel3
	where
		m1 = mux4WayBus as bs cs ds sel2 sel1
		m2 = mux4WayBus es fs gs hs sel2 sel1

mux8Way16 :: [Bit] -> [Bit] -> [Bit] -> [Bit]
	-> [Bit] -> [Bit] -> [Bit] -> [Bit]
	-> Bit -> Bit -> Bit -> [Bit]
mux8Way16 = mux8WayBus

dmux4Way :: Bit -> Bit -> Bit -> (Bit, Bit, Bit, Bit)
dmux4Way x selb sela = (x1, x2, x3, x4)
	where
		(x1_x3, x2_x4) = demux x sela
		(x1_x2, x3_x4) = demux x selb
		(x1, x3) = demux x1_x3 selb
		(x2, x4) = demux x2_x4 selb

dmux8Way :: Bit -> Bit -> Bit -> Bit
	-> (Bit, Bit, Bit, Bit, Bit, Bit, Bit, Bit)
dmux8Way x selc selb sela = (x1, x2, x3, x4, x5, x6, x7, x8)
	where
		(x1_x5, x2_x6, x3_x7, x4_x8) = dmux4Way x selb sela
		(x1, x5) = demux x1_x5 selc
		(x2, x6) = demux x2_x6 selc
		(x3, x7) = demux x3_x7 selc
		(x4, x8) = demux x4_x8 selc

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
halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder = fullAdder Zero

-- fullAdder adds three Bits (two from input and a third from the previous 
-- carry) and returns the result and a carry Bit.
fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder x y z = (thisDigit, carry)
	where
		thisDigit = xor x (xor y z)
		carry = or_ (or_ (and_ x y) (and_ x z)) (and_ y z)

-- addBus adds two buses of Bits and returns a Bit signaling overflow.
addBus :: [Bit] -> [Bit] -> ([Bit], Bit)
addBus xs ys = foldr addCarry ([], Zero) (zip xs ys)

addCarry :: (Bit, Bit) -> ([Bit], Bit) -> ([Bit], Bit)
addCarry (x, y) (list, prevCarry) = (s:list, newCarry)
	where
		(s, newCarry) = fullAdder x y prevCarry
