import Genetic hiding (main)

import System.Environment
import Text.Printf

eas :: Int -> IO Double
eas g = do
  q <- evolve g
  return $ fromIntegral $ score q

main = do
  args <- getArgs
  let n = read (head args)::Int --trials
  let gen = read (head $ tail args)::Int
  putStrLn ("Running " ++ (show n) ++ " trials for " ++ (show gen) ++ " generations each")
  q <- sequence $ replicate n $ eas gen
  putStrLn ("Average score: " ++ (show $ (sum q) / (fromIntegral (length q))))
