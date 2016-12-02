-- Part 1
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

-- Part 2
data List a = Empty | Cons a (List a)
     deriving Show

-- Q6
toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

-- Q7
toHaskellList :: List a -> [a]
toHaskellList Empty = []
toHaskellList (Cons first rest) = first:(toHaskellList rest)

-- Q8
append :: List a -> List a -> List a
append Empty Empty = Empty
append (Cons firstA restA) (Cons firstB restB) 
           = toList((toHaskellList (Cons firstA restA)) ++ (toHaskellList (Cons firstB restB)))

-- Q9
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f (Cons first rest)
         | (f first) == True = removeAll f rest
         | otherwise = Cons first (removeAll f rest)

-- Q10
sort :: Ord a => List a -> List a 
sort Empty = Empty
sort (Cons first rest) = toList (quicksort (toHaskellList (Cons first rest)))

-- http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html: Example: Sorting
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smalls ++ [x] ++ bigs
                   where smalls = quicksort [n | n <- xs, n <= x]
                         bigs   = quicksort [n | n <- xs, n > x]

-- Part 3
-- Q11
best_partition :: [Int] -> (Int, [Int], [Int])
best_partition seq
          | (length seq) == 1 = ((head seq), [], seq)









