{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OneMax where

import EA
import Control.Monad(replicateM)
import System.Random(randomIO)
import Data.Foldable(foldrM)

type BitArray = [Bool]

instance Genome BitArray where
	crossover = bitArrayC
	mutate = bitArrayM

instance Phenotype BitArray BitArray where
	fitness _ = foldr (\n acc -> if n then acc + 1 else acc) 0
	genome 	= id
	develop = id

bitArrayC :: Double -> BitArray -> BitArray -> IO (BitArray, BitArray)
bitArrayC rate a b = do
	r1 <- randomIO :: IO Double
	r2 <- randomIO :: IO Double
	let len = floor $ fromIntegral (length a) / 2.0
	let a1 = if r1 < rate then take len a ++ drop len b else a
	let b1 = if r2 < rate then drop len a ++ take len a else b
	return (a1, b1)

bitArrayM :: Double -> BitArray -> IO BitArray
bitArrayM rate = foldrM (\n acc -> do
	r <- randomIO :: IO Double
	let nn = if r < rate then not n else n
	return $ nn : acc) []

createBitArrayPop :: Int -> Int -> IO [BitArray]
createBitArrayPop bits size = replicateM size $ replicateM bits randomIO :: IO [BitArray]
