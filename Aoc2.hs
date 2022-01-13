calculateMoves :: (Integer, Integer, Integer, Integer)-> ([Char], String) -> (Integer, Integer, Integer, Integer)
calculateMoves = calculatePosition
  where
    calculatePosition (horizontal_pos, depth_pos, depthWithAim, aim) (direction, movement)
      | direction == "down" = (horizontal_pos, depth_pos + read movement, depthWithAim, aim + read movement)
      | direction == "up" = (horizontal_pos, depth_pos - read movement, depthWithAim, aim - read movement)
      | otherwise = (horizontal_pos + read movement, depth_pos,  depthWithAim + (read movement * aim), aim)

problemOne :: Foldable t => t ([Char], String) -> Integer
problemOne moves = horizontal_pos * depth_pos
  where
    (horizontal_pos, depth_pos, _, _) = foldl calculateMoves (0, 0, 0, 0) moves

problemTwo :: Foldable t => t ([Char], String) -> Integer
problemTwo moves = horizontal_pos * depthWithAim
  where
    (horizontal_pos, _, depthWithAim, _) = foldl calculateMoves (0, 0, 0, 0) moves

main :: IO ()
main = do
      input <- readFile "resources/day_02.txt"
      let moves = [(x, y) | i <- lines input, let s = span (/= ' ') i, let x = fst s, let y = drop 1 $ snd s]
      print (problemOne moves)
      print (problemTwo moves)