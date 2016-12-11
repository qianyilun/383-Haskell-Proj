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
append a Empty = a
append Empty b = b
append (Cons first rest) b = Cons first (append rest b)

-- Q9
removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty = Empty
removeAll f (Cons first rest)
         | (f first) == True = removeAll f rest
         | otherwise = Cons first (removeAll f rest)

-- Q10
getSmall :: Ord a => a -> List a -> List a
getSmall _ Empty = Empty
getSmall a (Cons first rest) 
         | (first <= a) = Cons first (getSmall first rest)
         | otherwise = getSmall first rest

getBig :: Ord a => a -> List a -> List a
getBig _ Empty = Empty
getBig a (Cons first rest) 
         | (first > a) = Cons first (getBig first rest)
         | otherwise = getBig first rest

sort :: Ord a => List a -> List a
sort Empty = Empty
sort (Cons first rest) = (append (append smalls (Cons first Empty)) bigs)
                         where smalls = sort(getSmall first rest)
                               bigs = sort(getBig first rest)

-- Part 3
-- Q11
first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c

-- Generates a partition with given list, given left/right part with index 0 and 1
-- e.g [7,4,3][1,0,0] => (0, [4,3], [7])
genFinalPart :: [Int] -> [Int] -> (Int, [Int], [Int])
genFinalPart [] [] = (0, [], [])
genFinalPart (x:xs) (y:ys)
                    | y == 0 = ( abs(sum(left1) - sum(right1)), left1, right1 )
                    | y == 1 = ( abs(sum(left2) - sum(right2)), left2, right2 )
                where left1  = [x] ++ second(genFinalPart xs ys)
                      right1 = third(genFinalPart xs ys)
                      left2  = second(genFinalPart xs ys)
                      right2 = [x] ++ third(genFinalPart xs ys)

-- Generates all bit sequences. Used be genBitIndex
genAllBitSeq :: Int -> [[Int]]
genAllBitSeq n | n < 1 = []
               | n == 1 = [[0],[1]]
               | otherwise = map ((++) [0]) (genAllBitSeq(n - 1)) ++ 
                             map ((++) [1]) (genAllBitSeq(n - 1))

genAllZeroes :: Int -> [Int]
genAllZeroes n | n < 1 = []
               | otherwise = (++) [0] (genAllZeroes(n - 1))

-- bit indexes for all partitions
genBitIndex :: Int -> [[Int]]
genBitIndex n | n == 1 = [[1]]
                | n > 1  = filter (/= genAllZeroes n) (map ((++) [0]) (genAllBitSeq(n - 1)))

-- Quicksort helper functions for paritions
smallPartition :: (Int, [Int], [Int]) -> [(Int, [Int], [Int])] -> [(Int, [Int], [Int])]
smallPartition (a, b, c) [] = []
smallPartition (a, b, c) (x:xs) | first(x) <= a = [x] ++ (smallPartition (a, b, c) xs)
                                   | otherwise     = (smallPartition (a, b, c) xs)

bigPartition :: (Int, [Int], [Int]) -> [(Int, [Int], [Int])] -> [(Int, [Int], [Int])]
bigPartition (a, b, c) [] = []
bigPartition (a, b, c) (x:xs) | first(x) > a = [x] ++ (bigPartition (a, b, c) xs)
                                 | otherwise    = (bigPartition (a, b, c) xs)

quicksortPartition :: [(Int, [Int], [Int])] -> [(Int, [Int], [Int])]
quicksortPartition [] = []
quicksortPartition (x:xs) = smalls ++ [x] ++ bigs
                        where smalls = quicksortPartition(smallPartition x xs)
                              bigs   = quicksortPartition(bigPartition   x xs)

-- http://www.cs.sfu.ca/CourseCentral/383/tjd/haskell_functions_lhs.html: Example: Sorting
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = smalls ++ [x] ++ bigs
                   where smalls = quicksort [n | n <- xs, n <= x]
                         bigs   = quicksort [n | n <- xs, n > x]

-- Main function
best_partition :: [Int] -> (Int, [Int], [Int])
best_partition x = head ( quicksortPartition (map (genFinalPart (quicksort x)) (genBitIndex (length x))))






