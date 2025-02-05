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
	testImpl
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


data Bit = Zero | One
	deriving (Eq, Show)

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _ _ = One

not_ :: Bit -> Bit
not_ b = nand b b

and_ :: Bit -> Bit -> Bit
and_ l r = not_ (nand l r)

or_ :: Bit -> Bit -> Bit
or_ l r = not_ (and_ (not_ l ) (not_ r))

xor :: Bit -> Bit -> Bit
xor l r = or_ (and_ l (not_ r)) (and_ (not_ l) r)

impl :: Bit -> Bit -> Bit
impl l r = or_ (not_ l) r
