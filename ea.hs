{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Genome(Genome, oneMaxFit, BitArray, fullReplace) where

import Control.Exception(assert)
import Control.Monad.State
import Data.List(delete)
import System.Random

--Inspiration for the class declaration retreived from: http://stackoverflow.com/a/5049911

class (Eq a) => Genome a where
    fitness	:: a -> Int
    crossover   :: (RandomGen g, MonadState g m, RealFrac b) => b -> a -> a -> m (a, a)
    mutate  	:: (RandomGen g, MonadState g m, RealFrac b) => b -> a -> m a

type BitArray = [Bool]

instance Genome BitArray where
	fitness     = oneMaxFit
	crossover   = crossBit
	mutate      = mutateBit

oneMaxFit :: BitArray -> Int
oneMaxFit = foldr (\b acc -> if b then acc + 1 else acc) 0

slice :: Int -> Int -> [a] -> [a]
slice from to li = take (to - from) $ drop from li

crossBit rate p1 p2 = do
		child1 <- crossBit' rate p1 p2
		child2 <- crossBit' rate p2 p1
		return $ assert (length child1 == length p1) (child1, child2)

crossBit' rate p1 p2 = do
		randGen <- get
		let amount = floor $ rate * fromIntegral  (length p1)
		let (pos, nGen) = randomR (0, length p1) randGen
		put nGen
		return (slice 0 pos p2 ++ (slice pos amount p1) ++ 
			(slice (pos + amount) (length p2) p2))

mutateBit rate gene = do
		randGen <- get
		let amount = floor $ rate * fromIntegral  (length gene)
		let rands = take amount $ randomRs (0, length gene) randGen
		let (_, g) = next randGen
		put g -- Done to ensure that if mutateBit is called twice in a row it will differ
		return $ foldr mutate' gene rands

mutate' :: Int -> BitArray -> BitArray
mutate' pos gene = x ++ (not y) : ys
	where (x, y:ys) = splitAt pos gene

selectMates :: (Genome a, RandomGen g, MonadState g m) => Int ->[a] -> m [(a, a)]
selectMates size li = selectMates' size li li

selectMates' :: (Genome a, RandomGen g, MonadState g m) => Int -> [a] -> [a] -> m [(a, a)]
selectMates' 0 _ _ = return []
selectMates' size [] rep = selectMates' size rep rep
selectMates' size li rep = do
		randgen <- get
		let (first, g) = randomR (1, length li) randgen
		let (second, g2) = randomR (1, length li) g
		put g2
		if first == second
		then selectMates' size li rep
		else do
		let fElem = li !! first
		let sElem = li !! second
		rest <- selectMates' (size - 1) (delete fElem (delete sElem li)) rep
		return $ (fElem, sElem) : rest
		

fullReplace :: (Genome a, RandomGen g, MonadState g m) =>
	(a -> Int) -> (Int -> [(a, Int)] -> m [a]) -> Double -> Double -> [a] -> m [a]
fullReplace fit select cRate mRate parents = do
		let fited = zip parents $ map fit parents
		let len = length parents
		mates <- select len fited
		paired <- selectMates len mates
		children <- mapM (\(c1, c2) -> crossover cRate c1 c2) paired
		let (ch, ch2) = unzip children
		let children2 = ch ++ ch2
		mapM (mutate mRate) children2
