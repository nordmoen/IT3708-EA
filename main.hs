import Fitness
import Genome
import Control.Monad(replicateM, liftM)
import Control.Monad.State
import Control.Monad.Trans
import System.Random

calcDiv :: Genome a => (a -> Int) -> [a] -> (Double, Double, Double)
calcDiv fit li = (best, avg, stdev)
	where 	fitList = map fit li
		len = fromIntegral $ length fitList
		best = fromIntegral $ maximum fitList
		avg = (fromIntegral $ sum fitList) / len
		stdev = sqrt $ (foldr (\v acc -> (fromIntegral v - avg)^2 + acc) 0 fitList) / len

--genLoop :: (Genome a, RandomGen g, MonadState g m) => 
	--(a -> Int) -> 					--Fitness function
	--(Int -> [(a, Int)] -> m [a]) -> 		--Selection algorithm
	--((a -> Int) -> (Int -> [(a, Int)] -> m [a]) -> Double -> Double -> [a] -> m [a]) ->
	--(Int -> Double -> Double -> Double -> IO ()) ->	--Logging function
	--Double -> 					--Crossover rate
	--Double -> 					--Mutation rate
	--Int -> 						--Max generation
	--Int -> 						--Current generation
	--[a] -> 						--Population
	--m ()
genLoop fit select proto logger cRate mRate maxGen currGen li = when (currGen < maxGen) $ do
		let (best, avg, stdev) = calcDiv fit li
		logger currGen best avg stdev
		next <- proto fit select cRate mRate li
		genLoop fit select proto logger cRate mRate maxGen (currGen + 1) next

cmdLogger :: Int -> Double -> Double -> Double -> IO ()
cmdLogger currGen best avg stdev = do
		putStrLn $ "Current generation: " ++ show currGen
		putStrLn $ "Best fitness: " ++ show best
		putStrLn $ "Average fitness: " ++ show avg ++ ", Stdev: " ++ show stdev

fileLogger :: String -> Int -> Double -> Double -> Double -> IO ()
fileLogger filename currGen best avg stdev = appendFile filename output
	where output = show currGen ++ "\t" ++ show best ++ "\t" ++ show avg ++ "\t" ++ show stdev

createRandom :: (RandomGen g, MonadState g m) => Int -> Int -> m [BitArray]
createRandom amount bitLength = replicateM amount (createRandom' bitLength)

createRandom' :: (RandomGen g, MonadState g m) => Int -> m BitArray
createRandom' len = replicateM len createRandom''

createRandom'' :: (RandomGen g, MonadState g m) => m Bool
createRandom'' = do
		randgen <- get
		let (b, g) = random randgen
		put g
		return b
		
main = do
		let (randBits, gen) = runState (createRandom 20 10) (mkStdGen 100)
		evalState (genLoop oneMaxFit fitProp fullReplace cmdLogger 0.3 0.3 50 0 randBits) gen
		return ()
