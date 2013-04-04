module Config where

import Selection
import OneMax

crossover 	= 0.9
mutation	= 1.0 / fromIntegral bits
elitism		= 3
size		= 10
bits		= 40
rounds		= 100 :: Int
maxFitness	= bits
selection :: Int -> [BitArray] -> IO [(BitArray, BitArray)]
selection	= defaultRank
protocol :: [BitArray] -> IO [BitArray]
protocol	= fullGenerational selection elitism size crossover mutation
initial		= createBitArrayPop bits size
