import           Data.List
import           Data.List.Split
-- in and output

parseLine :: String -> ([Int],Char,String)
parseLine line =
  let [ range, char , pw ] = words line
  in ( map read (splitOn "-" range) :: [Int], head char, pw)

isValid :: ([Int],Char,String) -> Bool
isValid (range,char,pw) =
  let count = (length $ filter (== char) pw)
  in ((count >= head range) && (count <= last range))


isValid2 :: ([Int],Char,String) -> Bool
isValid2 (range,char,pw) =
  let count = (length $ filter (\(idx,c) -> (elem idx range) && (char== c)) (zip [1..] pw  ))
  in count == 1

main :: IO ()
main = do
  cnt <- readFile "../data/day2"

  let input = map parseLine $ lines cnt

  putStr "part1:\t"
  putStrLn $ show $ length $ filter isValid input

  putStr "part2:\t"
  putStrLn $ show $ length $ filter isValid2 input
