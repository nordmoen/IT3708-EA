module Config where

import Selection
import OneMax

crossover 	= 0.9
mutation	= 1.0 / fromIntegral bits
elitism		= 3
size		= 10
bits		= 40
seed		= 42  :: Int
rounds		= 100 :: Int
maxFitness	= bits
selectionM :: [(BitArray, Int)] -> IO [(BitArray, Double)]
selectionM	= defaultRank
protocol :: [BitArray] -> IO [BitArray]
protocol	= fullGenerational selectionM elitism size crossover mutation
initial		= createBitArrayPop bits size
