module Config where

import Selection
import OneMax

crossover 	= 0.9
mutation	= 1.0 / fromIntegral bits
elitism		= 3
size		= 10
bits		= 40
rounds		= 100
selection	= defaultRank
protocol	= fullGenerational selection elitism size crossover mutation
initial		= createPop bits size :: IO [Bitarray]
