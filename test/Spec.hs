main :: IO ()
main = do
	if and tests then
		putStrLn "Success"
	else
		putStrLn "Fail"

tests :: [Bool]
tests = [testNand]

testNand :: Bool
testNand = and [
	nand False False,
	nand True False,
	nand False True,
	not (nand True True)
	]

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _ = True
