main :: IO ()
main = do
	if and tests then
		putStrLn "Success"
	else
		putStrLn "Fail"

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

nand :: Bit -> Bit -> Bit
nand One One = Zero
nand _ _ = One
