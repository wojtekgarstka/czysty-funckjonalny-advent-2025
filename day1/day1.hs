-- Test pierwszej funckji

import System.IO  
import Control.Monad
import Debug.Trace

parse :: [String] -> [(Char, Int)]
parse [] = []
parse ((c:cs):xs) = (c, read cs) : parse xs

spin_and_count :: [(Char, Int)] -> Int -> Int
spin_and_count [] status = 0
spin_and_count (d : xs) status = 
        let newStatus =  (mod (status + spin d) 100)
            zero = if status == 0 then 1 else 0
            -- total = trace (show status ++ " to new status "++ show newStatus)  (zero + spin_and_count xs newStatus)
            total = (zero + spin_and_count xs newStatus)
    in total


spin_while_counting :: [(Char, Int)] -> Int -> Int
spin_while_counting [] status = 0
spin_while_counting (d : xs) status =
        let newStatus =  (mod (status + spin d) 100)
            zero = spin_count d status
            -- total = trace ("Counting from " ++ show status ++ " to "++ show newStatus ++ " with  " ++ show d ++ " and gotten zero " ++ show zero) (zero + spin_while_counting xs newStatus)
            total = (zero + spin_while_counting xs newStatus)
    in total

spin_count :: (Char, Int) -> Int -> Int
spin_count (d, n) status = 
        let lr = if d == 'L' then (-) else (+)
            all = length [x | x <- [1 .. n],(mod (status `lr` x) 100) == 0]
    in all


spin :: (Char, Int) -> Int
spin (d, n) =
        let x = if d == 'L' then -n else n
    in x

main = do  
        contents <- readFile "input"
        let input = parse . words $ contents
        print ("Wynik to " ++ show (spin_and_count input 50))
        print ("Wynik2 to " ++ show (spin_while_counting input 50))

f :: [String] -> [Int]
f = map read
