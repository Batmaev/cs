data Matrix a = Matrix Int Int (Int -> Int -> a) 

getElem i j (Matrix _ _ m) = m i j

getLastColumn (Matrix nrows ncols m) = [m i ncols | i <- [1..nrows]]

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

scaryMatrix :: Int -> Matrix Double
scaryMatrix size = Matrix size (size+1) m where
    m i j | j == size+1       = fromIntegral i
          | i == size         = 1
          | abs (i-j) == 1    = 1
          | i == j            = 10
          | otherwise         = 0


backwards2id (Matrix nrows ncols m) = Matrix nrows ncols (h nrows) where
    h 0 i j = m i j
    h n i j | j >= n = if i == n then h (n-1) i j / h (n-1) i i else
                let ain = h (n-1) i n -- Элемент, который надо занулить
                in h (n-1) i j - h n n j * ain
            | otherwise = h (n-1) i j


directly2id (Matrix nrows ncols m) = Matrix nrows ncols (h 0 m) where
    h n m i j | n == nrows || j < n = m i j
              | otherwise = h n' m' i j where
                  n' = n+1
                  m' i j | i == n' = m i j / m i i
                         | otherwise = m i j - m' n' j * m i n'

main = putStrLn $ show $ getElem 1 6 $ directly2id $ scaryMatrix 5 