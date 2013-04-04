module Main where

import Config
import Evolution

main = do
	evoLoop initial protocol rounds maxFitness
