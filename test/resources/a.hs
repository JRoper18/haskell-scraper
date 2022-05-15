module A where
-- data T = A Int | B Float deriving Eq


{-# NOINLINE f #-}

f :: Int -> Int
f 0 = 3
f 1 = do
    2
f x = do
    let tmp = x * 2
    tmp + 1