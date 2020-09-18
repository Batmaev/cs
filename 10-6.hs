data Matrix a = Matrix Int Int (Int -> Int -> a) 

instance (Show a) => Show (Matrix a) where
    show (Matrix nrows ncols m) = 
        unlines [unwords [
            show (m i j) | j <- [1..ncols]
            ] | i <- [1..nrows]]

simpleMatrix :: Matrix Double
simpleMatrix = Matrix 3 4 m where
    m i j = [[4,1,1, 9],
             [4,0,4, 16],
             [4,1,2, 12]] !! (i - 1) !! (j - 1)

simpleMatrix2 :: Matrix Double
simpleMatrix2 = Matrix 3 4 m where
    m i j = [[10,1,0, 1],
             [1,10,1, 2],
             [1,1,1, 16]] !! (i - 1) !! (j - 1)

scaryMatrix :: Matrix Double
scaryMatrix = Matrix 100 100 m where
    m i j | j == 100          = fromIntegral i
          | i == 100          = 1
          | abs (i - j) == 1  = 1
          | i == j            = 10
          | otherwise         = 0

 
toIdentityMatrix (Matrix nrows ncols m) = Matrix nrows ncols (h nrows) where
    h 0 i j = m i j
    h n i j | j >= n = if i == n then h (n-1) i j / h (n-1) i i else
                let ain = h (n-1) i n -- Элемент, который надо занулить
                in h (n-1) i j - h n n j * ain
            | otherwise = h (n-1) i j