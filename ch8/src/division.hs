module Division where

data Quotient a =
    Result a
  | DivideByZero deriving Show

dividedBy :: Integral a => a -> a -> Quotient a
dividedBy x y = go x y 0 False
  where go n d count neg
          | d == 0         = DivideByZero
          | n < 0 && d < 0 = go (negate n) (negate d) 0 False
          | d < 0          = go (negate n) (negate d) 0 True
          | n < 0          = go (n + d) d (count + 1) True
          | n >= 0 && neg  = Result (negate count)
          | n < d          = Result count
          | otherwise      = go (n - d) d (count + 1) False
