import System.IO  
import Control.Monad
import Debug.Trace
import Data.List.Split

split' :: String -> (String, String)
--assumes 
split' str =
        let l = div (length str) 2
            res = splitAt l str
    in res

valid :: String -> Bool
valid str =
    if mod (length str) 2 == 1 then False
        else 
            let (str1, str2) = split' str
        in str1 == str2 

unfold :: String -> [String]
unfold str = 
    let (start:end:_) = splitOn "-" str
        str_list = [show x | x <- [(read start :: Int) .. (read end :: Int)] ]
    in str_list

main = do  
        contents <- readFile "input"
        let input = splitOn "," (head (words contents)) -- so no \n
        -- print . show $ ([num |num <- concat [unfold x | x<- input], valid num == False])
        print $ "Zadanie 1 to " ++ show (sum [read num :: Int |num <- concat [unfold x | x<- input], (valid num) == False])
        -- 1447159 is too low xdddddd sum of all not count 1083209228234713 is too high xdd not correct either
        -- print (show [read num :: Int | num <- concat [unfold x | x<- input], (valid num) == True])