module Genetic where

-- Genetic algorithms.
import System.Random
import Data.Vector (toList)
import Data.List (sort)
import Control.Monad (replicateM)
import Control.Monad.Loops (iterateM_)

import NN
import Snake

-- first, some constants.
population_size = 10
mutation_rate = 0.05
net_depth = 2
net_width = 10

score :: Net -> Int
score n = score' newGame 0 n

score' :: Position -> Int -> Net -> Int
score' board i n =
  let output    = compute (toList $ toGameboard board) n
      direction = case (snd $ head $ reverse $ sort $ zip output [0..]) of
          0 -> InputUp
          1 -> InputDown
          2 -> InputLeft
          3 -> InputRight
          otherwise -> error "what the fuck"
      next      = generateFrame direction board
   in case next of
        Just (True,newboard) -> score' newboard (i+11) n
        Just (False,newboard) -> score' newboard (i+1) n
        Nothing -> i

crossover' :: [Double] -> [Double] -> IO [Double]
crossover' a b = do
  k <- randomRIO (0,length a)
  return $ (take k a) ++ (drop k b)

kalfas :: [Double] -> [Double] -> IO [Double]
kalfas a b = do
  return $ zipWith (\x y -> (x+y)/2) a b

crossover :: Net -> Net -> IO Net
crossover a b = do
  q <- crossover' (flatten a) (flatten b)
  return (expand q (length a) (length $ head a))

roulette :: [(Int,a)] -> Int -> a
roulette [] _ = error "value passed to roulette is too high"
roulette ((k,x):xs) v = if (v < k) then x else roulette xs (v-k)

-- currently this potentially allows asexual reproduction, which isn't great but we'll leave it for now
mate :: [Int] -> [Net] -> IO (Net,Net)
mate fitnesses nets = do
  k <- randomRIO (0, sum fitnesses)
  let q = zip fitnesses nets
  let a = roulette q k
  let b = roulette q k
  return (a,b)

mutate :: Net -> IO Net
mutate n = do
  let depth = length n
  let width = length $ head n
  let flat = flatten n
  k <- randomRIO (0,length n)
  v <- randomRIO (-1.0,1.0)
  return $ expand ((take k flat) ++ (v:(drop (k+1) flat))) depth width

generation :: [Net] -> IO [Net]
generation nets = do
  let fitnesses = map score nets
  replicateM (length nets) (reproduce fitnesses nets)

reproduce :: [Int] -> [Net] -> IO Net
reproduce fitnesses nets = do
  (sperm,egg) <- mate fitnesses nets
  zygote <- crossover sperm egg
  -- now potentially mutate the zygote
  k <- randomIO :: IO Double
  mutant <- (if k < mutation_rate then mutate zygote else return zygote)
  return mutant

evolve :: Int -> IO Net
evolve time = do
  k <- sequence $ replicate population_size $ makenetr net_depth net_width
  res <- (iterateM_ generation k)
  return $ res !! time
