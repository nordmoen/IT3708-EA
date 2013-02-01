module Genome where

import Control.Monad.State
import System.Random

class Genome a where
    fitness	:: a -> Int
    crossover   :: (RandomGen g, MonadState g m, RealFrac b) => b -> a -> a -> m a
    mutate  	:: (RandomGen g, MonadState g m, RealFrac b) => b -> a -> m a

type BitArray = [Bool]

instance Genome BitArray where
	fitness     = oneMaxFit
	crossover   = crossBit
	mutate      = undefined

oneMaxFit :: BitArray -> Int
oneMaxFit = foldr (\b acc -> if b then acc + 1 else acc) 0

slice :: Int -> Int -> [a] -> [a]
slice from to li = take (to - from) $ drop from li

crossBit rate p1 p2 = do
			randGen <- get
			let amount = floor $ rate * fromIntegral  (length p1)
			let (pos, nGen) = randomR (0, length p1) randGen
			put nGen
			return (slice 0 pos p2 ++ (slice pos amount p1) ++ 
				(slice (pos + amount) (length p2) p2))

