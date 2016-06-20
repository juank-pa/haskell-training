module Chapter11.PrintExp where

allFnStr :: (Show a, Show b) => String -> String -> String -> [a] -> [b] -> String
allFnStr nm t1 t2 ins outs = unlines $ zipWith (flip (fnStr nm t1 t2) ins) [1..] (combine (length ins) outs)

combine :: Int -> [b] -> [[b]]
combine n outs = go n outs
  where
    go _ []     = []
    go 0 _      = []
    go 1 _      = map (:[]) outs
    go n (_:os) = [a : b | a <- outs, b <- go (n - 1) outs]

fnStr :: (Show a, Show b) => String -> String -> String -> Int -> [a] -> [b] -> String
fnStr nm t1 t2 i ins = unlines . (fstLn:) . map (fnName++) . zipWith (fnLn mxSz) ins
  where fnName = nm ++ show i ++ " "
        mxSz = foldr (max . length . show) 0 ins
        fstLn = fnName ++ ":: " ++ t1 ++ " -> " ++ t2

fnLn :: (Show a, Show b) => Int -> a -> b -> String
fnLn s q q' = concat [padQ, "= ", show q']
  where padQ = show q ++ replicate ((s + 1) - length (show q)) ' '


