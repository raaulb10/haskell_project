{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

conv :: Int -> [Int] -> Int
conv num xn = snd $ foldl cumConv (take num xn, 0) $ drop num xn
  where
    cumConv (pp@(p:pl), cnt) cur
      | sum pp < sum pl + cur = (pl ++ [cur], cnt+1) 
      | otherwise = (pl ++ [cur], cnt)

main = do
    lines <- readLines "resources/day_01.txt"
    putStrLn $ show $ conv 1 $ map read $ lines
    putStrLn $ show $ conv 3 $ map read $ lines
