{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module OneMax where

import EA
import Control.Monad(replicateM)
import System.Random(randomIO)
import Data.Foldable(foldrM)

type Bitarray = [Bool]

instance Genome Bitarray where
	crossover = oneC
	mutate = muteC
	develop = id
	createPop bits size = replicateM size $ replicateM bits randomIO :: IO [Bitarray]

instance Phenotype Bitarray Bitarray where
	fitness pop a = foldr (\n acc -> if n then acc + 1 else acc) 0 a
	genome a = a

oneC :: Double -> Bitarray -> Bitarray -> IO (Bitarray, Bitarray)
oneC rate a b = do
	r1 <- randomIO :: IO Double
	r2 <- randomIO :: IO Double
	let len = floor $ fromIntegral (length a) / 2.0
	let a1 = if r1 < rate then take len a ++ drop len b else a
	let b1 = if r2 < rate then drop len a ++ take len a else b
	return (a1, b1)

muteC :: Double -> Bitarray -> IO Bitarray
muteC rate a = foldrM (\n acc -> do
	r <- randomIO :: IO Double
	let nn = if r < rate then not n else n
	return $ nn : acc) [] a
