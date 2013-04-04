{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE BangPatterns #-}

module EA(Genome(..), Phenotype(..), best, average, stdev) where

import Data.List(maximumBy)

class (Eq a, Show a) => Genome a where
	crossover	:: Double -> a -> a -> IO (a, a)
	mutate		:: Double -> a -> IO a

class (Eq a, Show a, Genome b) => Phenotype a b | a -> b where
	--In case of Coevolution where each phenotype needs to be compared to every other in the population
	fitness		:: [a] -> a -> Int 
	genome		:: a -> b
	develop		:: b -> a

-- | Return the best individual in the population
best :: (Phenotype b a, Genome a) => [b] -> b
best !pop = maximumBy (\one two -> fitness pop one `compare` fitness pop two) pop

-- | Return the average fitness in the population
average :: (Phenotype b a, Genome a) => [b] -> Double
average !pop = fromIntegral avg / fromIntegral (length pop)
	where avg = foldr (\new acc -> acc + fitness pop new) 0 pop

stdev :: (Phenotype b a, Genome a) => Double -> [b] -> Double
stdev !avg !pop = sqrt st
	where 	st1 = foldr (\new acc -> acc + (fromIntegral (fitness pop new) - avg) ** 2) 0 pop
		st  = st1 / fromIntegral (length pop)
