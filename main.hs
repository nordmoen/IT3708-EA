import Fitness
import Genome
import Control.Monad.State
import System.Random

calcDiv :: Genome a => (a -> Int) -> [a] -> (Double, Double, Double)
calcDiv fit li = (best, avg, stdev)
	where 	fitList = map fit li
		len = fromIntegral $ length fitList
		best = fromIntegral $ maximum fitList
		avg = (fromIntegral $ sum fitList) / len
		stdev = sqrt $ (foldr (\v acc -> (fromIntegral v - avg)^2 + acc) 0 fitList) / len

genLoop :: (Genome a, RandomGen g, MonadState g m) => 
	(a -> Int) -> -- Fitness function
	(Int -> [(a, Int)] -> m [a]) -> -- Selection algorithm
	((a -> Int) -> (Int -> [(a, Int)] -> m [a]) -> Double -> Double -> [a] -> m [a]) ->
	Double -> -- Crossover rate
	Double -> --Mutation rate
	Int -> -- Max generation
	Int -> --Current generation
	[a] -> -- Population
	IO ()
genLoop fit select proto cRate mRate maxGen currGen li = do
		if currGen >= maxGen
		then putStrLn "Max number of generations"
		else do
		putStrLn $ "Current generation: " ++ show maxGen
		let (best, avg, stdev) = calcDiv fit li
		putStrLn $ "Current best: " ++ show best
		putStrLn $ "Generation average: " ++ show avg ++ ", Stdev: " ++ show stdev
		let (next, g) = runState (proto fit select cRate mRate li) (mkStdGen 10)
		genLoop fit select proto cRate mRate maxGen (currGen + 1) next
		
main = putStrLn "Not done!"
