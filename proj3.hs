data Bit = Zero | One
     deriving (Show, Eq)

-- Q1
flipBit :: Bit -> Bit
flipBit n
         | n == Zero = One
         | n == One = Zero

-- Q2
invert :: [Bit] -> [Bit]
invert [] = []
invert seq = map flipBit seq

-- Q3
all_bit_seqs :: Int -> [[Bit]]
all_bit_seqs n = sequence (replicate n [Zero, One])


-- Q4
bitSum1 :: [Bit] -> Int
-- bitSum1 [] = 0
bitSum1 lst = length (filter (isOne) lst)

isOne :: Bit -> Bool
isOne n 
       | n == One = True
       | otherwise = False

-- Q5
bitSum2 :: [Maybe Bit] -> Int
bitSum2 [] = 0
bitSum2 [Nothing] = 0
bitSum2 (x:xs) 
         | x == Just One = 1 + (bitSum2 xs)
         | otherwise = (bitSum2 xs)

