import System.IO  
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe

find_second_number :: String -> Int -> Int
find_second_number [] num = 0
find_second_number (beg: rest) num = 
    if digitToInt beg /= num then find_second_number rest num
    else if rest == [] then 0
        else num *10 + (maximum [digitToInt num | num <- nub rest]) 

largest_joltage :: String -> Int
largest_joltage bank = 
    let uniq = reverse . sort $ [digitToInt  num |num <- nub bank]
        scnd_number_in_bank = find_second_number bank
        in forceMaybe (find  (/=0) (map scnd_number_in_bank uniq))

forceMaybe :: Maybe a -> a
forceMaybe = fromJust

largest_joltage2 :: String -> Int -> Int
largest_joltage2 [] to_switch = 0
largest_joltage2 bank 1 = maximum [digitToInt num | num <- nub bank]
largest_joltage2 bank to_switch = 
    let poss = take (length bank - (to_switch-1)) bank -- bank bez to_switch-1 ostatnich znaków by by symbolizować to że może zadziałać
        uniq =  (reverse . sort $ [digitToInt  num |num <- nub poss])
        -- trzeba lazily znaleźć pierwszy numer 
        number = maximum uniq
        best_bank = head [rest | proposed@(x: rest) <- tails bank,  digitToInt x == number ]
        -- join number and best of 
        all = read (show number ++ show (largest_joltage2 best_bank (to_switch-1))) :: Int
        in all

main = do  
        contents <-   readFile "input"
        let banks = lines contents
        -- print . show $ [largest_joltage x | x <- banks]
        print ("Wynik to " ++ show (sum [largest_joltage x | x <- banks]))
        print ("Wynik to " ++ show (sum [largest_joltage2 x 12 | x <- banks]))
        -- 174901111357980 too high