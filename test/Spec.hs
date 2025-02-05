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

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _ _ = One

not_ :: Bit -> Bit
not_ b = nand b b
