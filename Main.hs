import System.Random


import           Control.Monad               (replicateM, replicateM_, forM_)
import Data.STRef
import Data.Function 
import           Data.Vector.Unboxed (freeze)
import qualified Data.Vector.Unboxed as IV
import qualified Data.Vector.Unboxed.Mutable as V
import           System.Random       (randomRIO)
import Debug.Trace
debug = flip trace

fracDiv = (/) `on` fromIntegral

modify v i n
	| i == n = return v
	| otherwise = do
		sample <- V.read v =<< randomRIO (0, i)
		V.write v i sample
		modify v (i+1) n

-- take list of random integers
modify'' indeces v i n 
	| i == n = return v
	| otherwise = do
		sample <- V.read v (head indeces)
		V.write v i sample
		modify'' (tail indeces) v (i+1) n

-- modify' - for exercising: how to write it in imperative fashion? using forM and cycling..
-- ??
	

-- poya's urn
exercise mod n = do
    vector <- V.replicate n (0 :: Int)
    V.write vector 0 1
    V.write vector 1 2
    mod vector 2 n
    ivector <- freeze vector 
    return ivector
    -- let greens = IV.length $ IV.filter (\i-> i== 1) ivector
    -- return $ fracDiv greens n
    --
eva_lurn vec part =
	let greens = IV.length $ IV.filter (\i-> i== 1) (IV.take part vec)
	in fracDiv greens part

simulateUrnIO n = do
    let n = 30000

    -- randIndeces <- mapM (\i -> randomRIO(0,i)) [1..n]
    -- print =<< exercise (modify'' randIndeces) n

    
    res <- exercise modify n
    forM_  [x*100 | x <- [1..30] ] (\i -> do 
    	print $ (show i) ++ ": " ++ (show $ eva_lurn res i))
    -- print =<< exercise modify n

-- n: ensemble size (n parallel experiments)
-- turns - turns of game
-- bank - current bank amount
-- part - part of `bank` to gamble
-- accum - history of bank
simulateGambleIO n turns bank part = replicateM n ( simulateGambleIOsingle turns bank part)

simulateGambleIOsingle turns bank part
	| (turns == 0) = return bank
	| otherwise = do
		coin <- randomRIO(0,1) :: IO Int
		case () of _
				| coin == 0 -> simulateGambleIOsingle (turns-1) (bank-(bank*part*0.4)) part 
				| otherwise -> simulateGambleIOsingle (turns-1) (bank+(bank*part*0.5)) part

simulateGambleIOgraphic n turns part bank = replicateM n ( simulateGambleIOgraph turns part [] bank )

simulateGambleIOgraph turns part accum bank 
	| (turns == 0) = return $ reverse (bank:accum)
	| otherwise = do
		coin <- randomRIO(0,1) :: IO Int
		case () of _
				| coin == 0 -> simulateGambleIOgraph (turns-1) part (bank:accum) (bank-(bank*part*0.4)) 
				| otherwise -> simulateGambleIOgraph (turns-1) part (bank:accum) (bank+(bank*part*0.5)) 
mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)





main :: IO ()
main = do
    let turns = 350
    let ensemble = 3500
    let part = 1
    let bank = 1
    -- simres <- simulateGambleIOsingle turns 1 part
    -- print $ simres

    simres' <- simulateGambleIO ensemble turns 1 part
    -- print $ simres'
    print $(sum simres'/fromIntegral(ensemble))

    -- simulateUrnIO 30000
