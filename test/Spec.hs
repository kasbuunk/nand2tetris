main :: IO ()
main = do
	if and tests then
		putStrLn "Success"
	else
		putStrLn "Fail"

tests :: [Bool]
tests = [testNand]

testNand :: Bool
testNand = nand False False

nand False False = True
