module Main where

import Config
import Evolution

main = do
	putStrLn "Starting Evolution"
	init <- initial
	evoLoop init protocol rounds maxFitness
