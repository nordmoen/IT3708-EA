module Main where

import Config
import Evolution

import System.Random(setStdGen, mkStdGen)

main = do
	setStdGen $ mkStdGen seed
	putStrLn "Starting Evolution"
	init <- initial
	evoLoop init protocol rounds maxFitness
