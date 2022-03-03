module A where

data T = A Int | B Float deriving Eq

f :: Int -> Int
f 0 = 3
f 1 = do
    2
f x = x