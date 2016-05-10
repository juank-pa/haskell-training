printSecond :: IO ()
printSecond = putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond

greeting = "Yarrrr"
