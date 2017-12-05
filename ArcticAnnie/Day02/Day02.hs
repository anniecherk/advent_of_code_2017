module Main where


main :: IO ()
main = do
        contents <- readFile "input.txt"
        let rows = map (map (\x -> read x ::Int) . words) (lines contents)
        let soln = sum $ map (\row -> maximum row - minimum row) rows
        putStrLn $ "Part 1! " ++ show soln
