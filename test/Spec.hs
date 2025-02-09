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
	testSplitAnd,
	testEval,
	testSubstitute,
	testDistributive
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

data Bit = Zero | One
	deriving (Eq, Show)

-- bits enumerates the set of values Bit can have.
bits :: [Bit]
bits = [Zero, One]

data LogicalExpr a = Literal Bit
	| Var a -- Variable is used to substitute for a Bit
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

splitAnd :: Bit -> Bit -> (Bit, Bit)
splitAnd l r = composeBGateSplitter and_ split l r

composeBGateSplitter :: BinaryGate -> Splitter -> (Bit -> Bit -> (Bit, Bit))
composeBGateSplitter bg s l r = s (bg l r)
