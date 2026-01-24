import System.Console.CmdArgs
import System.Random
import System.IO
import Control.Monad               (replicateM, replicateM_, forM_)
import Data.List (intercalate, zip)
import Data.STRef
import Data.Function 
import           Data.Vector.Unboxed (freeze)
import qualified Data.Vector.Unboxed as IV
import qualified Data.Vector.Unboxed.Mutable as V
import           System.Random       (randomRIO)

data SimMode = Urn | Gamble deriving (Data, Typeable, Show, Eq)

data MyApp = MyApp
    { 
      output   :: FilePath
    , mode     :: SimMode
    , ensemble :: Int
    , totalTurns    :: Int
    , bankPart :: Double
    , onWin    :: Double
    , onLose   :: Double
    } deriving (Data, Typeable, Show, Eq)

myApp :: MyApp
myApp = MyApp
    { 
      output = "output.txt" &= typ "FILE" &= help "Output file"
      ,mode = Gamble 
    , ensemble = 1 &= help "amount of independent simulations" &= name "ensemble" 
    , totalTurns = 100 &= help "Number of turns" &= name "totalTurns"
    , bankPart = 1.0 &= help "part of bank to be at stake at each turn" &= name "bankPart" 
    , onWin = 0.5 &= help "part of stake to add" &= name "onWin" 
    , onLose = 0.4 &= help "part of stake to subtract" &= name "onLose" 
    } &= summary "simulation for Polya's urn and gambling" &= help "A useful application" &= program "myapp"

fracDiv = (/) `on` fromIntegral

-- take list of random integers
modify indeces v i n 
	| i == n = return v
	| otherwise = do
		sample <- V.read v (head indeces)
		V.write v i sample
		modify (tail indeces) v (i+1) n

-- poya's urn
exercise mod n = do
    vector <- V.replicate n (0 :: Int)
    V.write vector 0 1
    V.write vector 1 2
    mod vector 2 n
    ivector <- freeze vector 
    return $ IV.toList ivector
    -- let greens = IV.length $ IV.filter (\i-> i== 1) ivector
    -- return $ fracDiv greens n
    --
eval_urn vec part =
	let greens = IV.length $ IV.filter (\i-> i== 1) (IV.take part vec)
	in fracDiv greens part

simulateUrnIO args = do
    let turns = totalTurns args
    randIndeces <- mapM (\i -> randomRIO(0,i)) [1..turns]
    res <- exercise (modify randIndeces) turns
    return $ zip [1..] res
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

simulateGambleIOgraphic cmdargs = let 
						n = ensemble cmdargs
						turns = totalTurns cmdargs
					    in 
					    do
						    let turns = totalTurns cmdargs
						    let amount = ensemble cmdargs 
						    raw_data <- replicateM n ( simulateGambleIOgraph cmdargs turns [] 1 )
						    let data1 = transformLists $ (map fromIntegral [1 .. turns]) : raw_data
						    putStrLn $ "turns,"  ++ (generateLinesHeader amount)
						    mapM_ (putStrLn . intercalate ", " . map show) data1


simulateGambleIOgraph cmdargs turns accum bank 
	| (turns == 0) = return $ reverse (bank:accum)
	| otherwise = do
		coin <- randomRIO(0,1) :: IO Int
		let onwin = onWin cmdargs
		let onlose = onLose cmdargs
		let part = bankPart cmdargs
		case () of _
				| coin == 0 -> computeForward (bank-(bank*part*onlose)) 
				| otherwise -> computeForward (bank+(bank*part*onwin)) 
				where computeForward = simulateGambleIOgraph cmdargs (turns-1) (bank:accum)
mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- takes list of N sequences of L length, transforms to list of L lists with N elems (zips)
transformLists ([]:xs) = []
transformLists xs = (head <$> xs) : transformLists (tail <$> xs)

generateLinesHeader :: Int -> String
generateLinesHeader n = intercalate ", " [ "line" ++ show i | i <- [1..n] ]

main :: IO ()
main = do
    args <- cmdArgs myApp
    hPutStrLn stderr $ "Arguments: " ++ show args
    let gamemode = mode args
    case gamemode of
    	Urn ->    do
		    dat <- simulateUrnIO args 
		    print dat
	Gamble -> do
			simulateGambleIOgraphic args
