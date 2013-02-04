module Fitness(fitProp, sigmaScale, tourSelect, rankScale) where

import System.Random
import Control.Monad
import Control.Monad.State
import Data.List(sortBy)
import Data.Ord(comparing)
import Genome
import Control.Exception(assert)

--Some handy functions not exported from this module
totalFitness :: [(a, Int)] -> Double
totalFitness pop = fromIntegral $ foldr (\p acc -> snd p + acc) 0 pop

avg :: [(a, Int)] -> Double
avg pop = totalFitness pop / fromIntegral (length pop)

stdev :: [(a, Int)] -> Double -> Double
stdev pop avg = sqrt (dev / fromIntegral (length pop))
		where dev = foldr (\(_, e) acc -> (fromIntegral e - avg)^2 + acc) 0 pop
--END handy functions

--Roulette wheel selection method
rouletteSelect'' :: Double -> [(a, Double)] -> a
rouletteSelect'' rand li = fst $ foldr1 (\(i, val) acc -> if val > rand then (i, val) else acc) li

rouletteSelect' :: (RandomGen g, MonadState g m) =>
	[(a, Double)] -> m a
rouletteSelect' li = do
		randgen <- get
		let (a, g) = random randgen
		put g
		return $ rouletteSelect'' a li

rouletteSelect :: (RandomGen g, MonadState g m) =>
	Int -> [(a, Double)] -> m [a]
rouletteSelect size li = replicateM size (rouletteSelect' nLi)
	where nLi = scanl1 (\(_, acc) (i, val) -> (i, acc + val)) li
			

--Fitness proportionat selection:
fitProp :: (Genome a, RandomGen g, MonadState g m) =>
	Int -> [(a, Int)] -> m [a]
fitProp size pop = do
		let factor = totalFitness pop
		let scaled = foldr (\(it, val) acc -> (it, fromIntegral val / factor) : acc) [] pop
		rouletteSelect size scaled

--Sigma scaling selection
sigmaScale :: (Genome a, RandomGen g, MonadState g m) =>
	Int -> [(a, Int)] -> m [a]
sigmaScale size pop = do
		let avgPop = avg pop
		let stdevPop = stdev pop avgPop
		let scaled = foldr (\(it, val) acc -> (it, sigmaScale' val avgPop stdevPop) : acc) [] pop
		rouletteSelect size scaled

sigmaScale' :: Int -> Double -> Double -> Double
sigmaScale' val avg stdev = 1 + ((fromIntegral val - avg) / (2*stdev))

--Tournament selection
randSelect :: (RandomGen g, MonadState g m) => [a] -> m a
randSelect li = do
		randgen <- get
		let (r, g) = randomR (1, length li) randgen
		put g
		return $ li !! (r - 1)

tourSelect' :: (Genome a, RandomGen g, MonadState g m) =>
	Int -> Double -> [(a, Int)] -> m a
tourSelect' k s pop = do
		randgen <- get
		randGroup <- replicateM k (randSelect pop)
		let (x:xs) = reverse $ sortBy (comparing snd) randGroup
		let (r, g) = random randgen
		put g
		return $ if (1.0 - s) > assert (r < 1.0) r then fst x else fst $ head xs

tourSelect :: (Genome a, RandomGen g, MonadState g m) =>
	Int -> Double -> Int -> [(a, Int)] -> m [a]
tourSelect k s size pop = replicateM size (tourSelect' k s pop)

--Rank selection
rankScale :: (Genome a, RandomGen g, MonadState g m) =>
	Double -> Double -> Int -> [(a, Int)] -> m [a]
rankScale minR maxR size pop = do
		let len = length pop
		let sorted = sortBy (comparing snd) pop
		let ziped = zip sorted [1..]
		let scaled = foldr (\((it, _), rank) acc -> (it, rankScale' minR maxR rank len) : acc) [] ziped
		rouletteSelect size scaled

rankScale' :: Double -> Double -> Int -> Int -> Double
rankScale' minR maxR rank len = minR + (maxR - minR) * ((fromIntegral rank - 1) / (fromIntegral len - 1))
