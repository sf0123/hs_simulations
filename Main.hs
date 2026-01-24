import System.Console.CmdArgs
import System.Random
import System.IO
import Control.Monad               (replicateM, replicateM_, forM_, foldM)
import Control.Monad.State (put, get, runState)
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
      output = "stdout" &= typ "FILE" &= help "Output file name, or 'stdout'" &= name "output"
      ,mode = Gamble  &= help "Urn | Gamble"
    , ensemble = 1 &= help "amount of independent simulations"
    , totalTurns = 100 &= help "Number of turns"
    , bankPart = 1.0 &= help "part of bank to be at stake at each turn"
    , onWin = 0.5 &= help "part of stake to add"
    , onLose = 0.4 &= help "part of stake to subtract"
    } &= summary "simulation for Polya's urn and gambling" &= help "app for simulation of some ergodic and non-ergodic processes  " &= program "simulate"

fracDiv = (/) `on` fromIntegral

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

func1 rest el  = do
	(accum,len) <- get 
	let total = if el == 1 then accum+1 else accum
	let distr = total/len :: Double 
	put(total,len+1) 
	return $ distr:rest

modifyToDistr a = reverse $ fst $ runState (foldM func1 [] a) (0,1)

-- todo - aggregate green/amount for each cell
simulateUrnIO args = do
    let amount = ensemble args
    let turns = totalTurns args
    let fname = output args
    res <- replicateM amount (simulateUrnIO' args)
    putStrLn $ "turns," ++ (generateLinesHeader amount)
    mapM_ (putStrLn . intercalate ", " . map show) (transformLists $ (fromIntegral <$> [1..turns]): res)


simulateUrnIO' args = do
    let turns = totalTurns args
    randIndeces <- mapM (\i -> randomRIO(0,i)) [1..turns]
    res <- exercise (modify randIndeces) turns
    return $ modifyToDistr res

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
						    let outfile = output cmdargs
						    let genOutput = (\outf -> do
							   outf $ "turns,"  ++ (generateLinesHeader amount)
							   mapM_ (outf . intercalate ", " . map show) data1
								   )
						    case outfile of
						    	"stdout" -> genOutput putStrLn
							(filePath) -> do
							    handle <- openFile filePath WriteMode
							    genOutput (hPutStrLn handle)
   							    hClose handle




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
		    simulateUrnIO args 
	Gamble -> do
	            simulateGambleIOgraphic args
