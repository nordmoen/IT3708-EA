{-# LANGUAGE BangPatterns #-}

module Evolution(evoLoop) where

import EA

evoLoop :: (Phenotype b a, Genome a) =>
	[b] -> 			--Population
	([b] -> IO [b]) -> 	--Selection protocol	
	Int ->			--Number of rounds
	Int -> 			--Max fitness
	IO [b]
evoLoop !pop !protocol rounds max = do
	if rounds == 0
	then do 
		putStrLn $ replicate nLines '-'
		printStats pop
		return pop
	else do
		putStrLn $ replicate nLines '-'
		putStrLn $ show rounds ++ " left"
		printStats pop
		putStrLn $ replicate nLines '-'
		next <- protocol pop
		let b = fitness next (best next)
		if max >= 0 && b >= max
		then do
			putStrLn $ replicate nLines '-'
			putStrLn $ "Reach the max fitness(" ++ show max ++ ")"
			putStrLn "Best fit individual:"
			putStrLn $ "\t" ++ show (best next)
			putStrLn $ replicate nLines '-'
			return next
		else evoLoop next protocol (rounds - 1) max

printStats :: (Phenotype b a, Genome a) => [b] -> IO ()
printStats pop = do
	let b = best pop
	let avg = average pop
	let std = stdev avg pop
	putStrLn $ "Current best: " ++ show b ++ "(Fitness: " ++ show (fitness pop b) ++ ")"
	putStrLn $ "Average: " ++ show avg ++ "(" ++ show std ++ ")"

nLines = 80 :: Int
