module Main where

import Config
import Evolution

import System.Random(setStdGen, mkStdGen)

main :: IO ()
main = do
	setStdGen $ mkStdGen seed
	putStrLn "Starting Evolution"
	initPop <- initial
	_ <- evoLoop initPop protocol rounds maxFitness
	putStrLn "Evolution has ended"
