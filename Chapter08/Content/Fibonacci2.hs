module Chapter08.Content.FibonacciPart2 where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = (x - 1) (x - 2)
-- note: this doesn't work quite yet.
