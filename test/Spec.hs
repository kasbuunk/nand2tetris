main :: IO ()
main = do
	if and tests then
		putStr "Success"
	else
		putStr "Fail"

tests :: [Bool]
tests = [testNand]

data Bit = Zero | One
	deriving (Eq, Show)

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

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _ _ = One

not_ :: Bit -> Bit
not_ b = nand b b

and_ :: Bit -> Bit -> Bit
and_ One One = One
and_ _ _ = Zero
