import System.IO  
import Control.Monad
import Debug.Trace
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe

binary_rows :: String -> [Int]
binary_rows row = [if ch == '@' then 1 else 0 | ch <- row]

adjencancy_map :: [[Int]] -> [[Int]]
adjencancy_map rows = 
    let neighbours =  [(x,y) | x<- [-1,0,1], y <- [-1,0,1], (x,y) /= (0, 0) ]
        len = length (rows !! 0)
    in  [sum_rows  [  shift_row (get_row rows (n+y)) x  | (x,y)<-neighbours] | n <- [0 .. (len -1)]]

sum_row :: [Int] -> [Int] -> [Int]
sum_row row1 row2 = [x+y| x<- row1, y<- row2] 

sum_rows :: [[Int]] -> [Int]
--  IMPORTANT LESSON ON HOW FOLDR WORKS AND ZIPWITH
--  foldr1 basically applies the function to every element,
sum_rows rows = foldr1 (zipWith (+)) rows

get_row :: [[Int]] -> Int -> [Int]
get_row rows n  
    | n < 0 || n > (length (rows !! 0) - 1) = take (length (rows !! 0)) (repeat 0)
    | otherwise = rows !! n

    

shift_row :: [Int] -> Int -> [Int]
-- shifts row so that at N position there is what is at N+shift (no negatives! -1 means "shift forward")
-- zeros if index is suppoused to be out of bounds
shift_row row 0 = row
shift_row row shift =
    if shift > 0 
        then (drop (shift) row) ++ (take shift (repeat 0)) 
        else (take (-shift) (repeat 0)) ++ (take (length row + shift) row)


task1 :: [[Int]] -> [[Int]] -> Int
task1 paper_rolls neighbours_map =
    length . filter (==True) $ [surrounded_4 paper neigh |(paper, neigh) <- zip (concat paper_rolls) (concat neighbours_map) ]


surrounded_4 :: Int -> Int -> Bool
surrounded_4 paper neigh  
    | paper == 0 = False
    | neigh < 4 = True
    | otherwise = False

map_of_removable :: [[Int]] -> [[Int]] -> [[Bool]]
map_of_removable paper_rolls neighbours_map =
    [[x | x <- zipWith surrounded_4 pap_row neigh_row ] | (pap_row, neigh_row) <- zip paper_rolls neighbours_map ]


count_removables ::  [[Bool]] -> Int
count_removables removable = length (filter (==True) (concat removable))

grid_minus_bool :: [[Int]] -> [[Bool]] -> [[Int]]
grid_minus_bool paper_rolls to_remove =
    [ [ if pap == 1 && rem then 0 else pap
      | (pap, rem) <- zip papRow remRow
      ]
    | (papRow, remRow) <- zip paper_rolls to_remove
    ]

remove_until_done     :: ([[Int]] -> [[Int]] -> [[Bool]]) -- map_of_removable - the function that verifies what to delete
    -> ([[Int]] -> [[Bool]] -> [[Int]]) -- function that deletes result from state
    -> [[Int]] -- initial step
    -> [Int]  -- counts after each step
remove_until_done removing_map substraction_from_state = go
    where
        go grid = -- grid of ones and zeros where paper is
            let removable = removing_map grid (adjencancy_map grid)
                count_remove = count_removables removable 
            in if count_remove == 0
                then [0]
                else count_remove : go (substraction_from_state grid removable)



main = do  
        contents <-   readFile "input"
        let rows = lines contents
        let number_rows = [binary_rows x | x <- rows]
        -- print (show (number_rows !! 0))
        -- print (show (shift_row (number_rows !! 0) (-1)))
        -- print (show ((adjencancy_map number_rows) !! 0 ))
        let neighbour_count = adjencancy_map number_rows
        -- print (show ((adjencancy_map number_rows) !! 1 ))
        print ("Forklift can access "++ (show  (task1 number_rows neighbour_count)) ++ " rolls with less than 4 neighbours")
        let removable = map_of_removable number_rows neighbour_count
        print (show (count_removables removable))
        let list_removables = remove_until_done map_of_removable grid_minus_bool number_rows
        print (show (list_removables))
        print (show (sum list_removables))

        


