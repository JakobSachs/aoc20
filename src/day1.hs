import           Data.List
import           Data.Maybe

-- this could probably be better / combined into a single function
combine2 xs ys =  [[x,y]  | x <- xs, y <- ys, x /= y]
combine3 xs ys zs = [ [x,y,z] | x <- xs, y <-ys , z <- zs,  (x /= y) && ( x /= z )]


selfSums2 x = map sum $ combine2 x x
selfProds2 x = map product $ combine2 x x
 -- same with these

selfSums3 x = map sum $ combine3 x x x
selfProds3 x = map product $ combine3 x x x

solve1 :: [Int] -> Int
solve1 input = snd $  fromJust $ find (\x -> (fst x) == 2020) $
               zip (selfSums2 input) (selfProds2 input)

solve2 :: [Int] -> Int
solve2 input = snd $ fromJust $ find (\x-> (fst x) == 2020) $
              zip (selfSums3 input) (selfProds3 input)


-- in and output
parseInput :: String -> [Int]
parseInput str = map read $ lines str :: [Int]

main :: IO ()
main = do
  cnt <- readFile "../data/day1"
  let inpt = parseInput cnt :: [Int]

  putStr "part1:\t"
  putStrLn $ show $ solve1 inpt

  putStr "part2:\t"
  putStrLn $ show $ solve2 inpt
