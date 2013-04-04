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
	crossover = oneC
	mutate = muteC

instance Phenotype BitArray BitArray where
	fitness pop = foldr (\n acc -> if n then acc + 1 else acc) 0
	genome a = a
	develop = id

oneC :: Double -> BitArray -> BitArray -> IO (BitArray, BitArray)
oneC rate a b = do
	r1 <- randomIO :: IO Double
	r2 <- randomIO :: IO Double
	let len = floor $ fromIntegral (length a) / 2.0
	let a1 = if r1 < rate then take len a ++ drop len b else a
	let b1 = if r2 < rate then drop len a ++ take len a else b
	return (a1, b1)

muteC :: Double -> BitArray -> IO BitArray
muteC rate = foldrM (\n acc -> do
	r <- randomIO :: IO Double
	let nn = if r < rate then not n else n
	return $ nn : acc) []

createBitArrayPop :: Int -> Int -> IO [BitArray]
createBitArrayPop bits size = replicateM size $ replicateM bits randomIO :: IO [BitArray]
