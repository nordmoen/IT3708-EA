module Selection(rankSelection, defaultRank, fullGenerational) where

--Global imports
import Data.Foldable(foldrM)
import Data.List(sortBy)
import Control.Monad(replicateM, liftM)
import System.Random(randomIO)
--Local imports
import EA

--Code and values based on http://www.idi.ntnu.no/emner/it3708/lectures/notes/ea-selection.pdf

-- |Rank selection mechanism
rankSelection :: (Phenotype b a) =>
	Double -> --The minimum value for the rank selection, range: [0, 1]
	Double -> --The maximum value for the rank selection, range: [1, 2]
	Int -> 	--The number of couples to select
	[b] -> 	--The population to select individuals from
	IO [(b, b)] --Since the selection is dependant on randomness this must be within a monad
rankSelection min max amount population = do
	rands <- replicateM amount randomIO :: IO [Double]
	rands2 <- replicateM amount randomIO :: IO [Double]
	return $ foldr (\(r1, r2) acc -> (wheel r1, wheel r2) : acc) [] (zip rands rands2)
		where 	sorted = sortBy (fit population) population
			size = fromIntegral $ length population - 1
			calc = [min + (max - min)*((i - 1)/size) | i <- [0..]]
			norm = normalized $ zip sorted calc
			wheel = rouletteSelection norm

-- |Rank selection initialized with the most commonly used min and max values
defaultRank :: (Phenotype b a) => Int -> [b] -> IO [(b, b)]
defaultRank = rankSelection 0.5 1.5

normalized :: [(a, Double)] -> [(a, Double)]
normalized list = map (\(a, val) -> (a, val / fact)) list
	where fact = foldr (\(_, val) acc -> val + acc) 0 list

--Helper method used in rank and full generational replacement
fit pop f s = fitness pop f `compare` fitness pop s

rouletteSelection' :: Double -> [(a, Double)] -> Double -> a
rouletteSelection' _ (x:[]) _ = fst x --X is last element, must pick x
rouletteSelection' acc (x:xs) rand =	if rand < nAcc
					then fst x
					else rouletteSelection' nAcc xs rand
				where nAcc = acc + snd x

rouletteSelection :: [(a, Double)] -> Double -> a
rouletteSelection = rouletteSelection' 0

-- |Full generational replacement selection protocol
fullGenerational :: (Phenotype b a, Genome a) =>
	(Int -> [b] -> IO [(b, b)]) -> --Selection mechanism
	Int -> --Elitism
	Int -> --The number of children to create
	Double -> --Crossover rate
	Double -> --Mutation rate
	[b] -> --Population to select from
	IO [b] --The new population created
fullGenerational selection e amount cross mute pop = do
	let a = ceiling (fromIntegral amount / 2.0)
	parents <- selection a pop
	--next <- breed parents cross mute
	let breed' = breed cross mute
	next <- foldrM breed' [] parents
	return $ drop e next ++ take e reverseSorted
		where reverseSorted = reverse $ sortBy (fit pop) pop

breed :: (Phenotype b a, Genome a) => Double -> Double -> (b, b) -> [b] -> IO [b]
breed cross mute (dad, mom) acc = do
	(childA, childB) <- crossover cross (genome dad) (genome mom)
	childAMute <- mutate mute childA
	childBMute <- mutate mute childB
	let newA = develop childAMute
	let newB = develop childBMute
	return $ newB : newA : acc
