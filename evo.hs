{-# LANGUAGE BangPatterns #-}

module Evolution(evoLoop) where

import EA

evoLoop :: (Phenotype b a, Genome a) =>
	[b] -> 			--Population
	([b] -> IO [b]) -> 	--Selection protocol	
	Int ->			--Number of rounds
	Double -> 		--Max fitness
	IO [b]
evoLoop !pop !protocol rounds max = do
	if rounds == 0
	then do 
		putStrLn "Done"
		printStats pop
		return pop
	else do
		putStrLn $ show rounds ++ " left"
		printStats pop
		protocol pop

printStats :: (Phenotype b a, Genome a) => [b] -> IO ()
printStats pop = do
	let b = best pop
	let avg = average pop
	let std = stdev avg pop
	putStrLn $ "Current best: " ++ show b ++ show (fitness pop b)
	putStrLn $ "Average: " ++ show avg ++ "(" ++ show std ++ ")"
