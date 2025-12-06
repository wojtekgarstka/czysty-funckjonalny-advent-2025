import System.IO  
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.List

split' :: String -> (String, String)
--assumes 
split' str =
        let l = div (length str) 2
            res = splitAt l str
    in res

invalid :: String -> Bool
invalid str =
    if mod (length str) 2 == 1 then False
        else 
            let (str1, str2) = split' str
        in str1 == str2 

invalid_variable :: String -> Int -> Bool
invalid_variable str repeat =
    if rem (length str) repeat /= 0
    then False
    else
        let chunk_len = div (length str) repeat
            in if chunk_len == length str
                then False
                else
                    let (start:end) = chunksOf chunk_len str
                        all_match = all (== start) end
                        in all_match

invalid_multiple :: String -> Bool
invalid_multiple str = any (==True) [invalid_variable str chunk_len | chunk_len <- [(min 2 (length str)) .. (length str)]]

unfold :: String -> [String]
unfold str = 
    let (start:end:_) = splitOn "-" str
        str_list = [show x | x <- [(read start :: Int) .. (read end :: Int)] ]
    in str_list

main = do  
        contents <- readFile "input"
        let input = splitOn "," (head (words contents)) -- so no \n
        -- print . show $ ([num |num <- concat [unfold x | x<- input], valid num == False])
        print $ "Zadanie 1 to " ++ show (sum [read num :: Integer |num <- concat [unfold x | x<- input], (invalid num) == True])
        -- 1447159 is too low xdddddd sum of all not count 1083209228234713 is too high xdd not correct either
        -- print (show [read num :: Int | num <- concat [unfold x | x<- input], (invalid num) == True])
        print $ "Zadanie 2 to " ++ show (sum [read num :: Integer |num <- concat [unfold x | x<- input], (invalid_multiple num) == True])
        -- print (show [read num :: Integer |num <- concat [unfold x | x<- input], (invalid_multiple num) == True])