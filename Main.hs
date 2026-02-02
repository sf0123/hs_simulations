import Control.Monad (foldM, forM_, replicateM, replicateM_)
import Control.Monad.State (get, put, runState)
import Data.Function
import Data.List (intercalate, zip)
import Data.STRef
import Data.Vector.Unboxed (freeze)
import qualified Data.Vector.Unboxed as IV
import qualified Data.Vector.Unboxed.Mutable as V
import System.Console.CmdArgs
import System.IO
import System.Process
import System.Random
import System.Random (randomRIO)

data SimMode = Urn | Gamble | RandVar deriving (Data, Typeable, Show, Eq)

data MyApp = MyApp
  { output :: String,
    mode :: SimMode,
    ensemble :: Int,
    totalTurns :: Int,
    bankPart :: Double,
    onWin :: Double,
    onLose :: Double
  }
  deriving (Data, Typeable, Show, Eq)

myApp :: MyApp
myApp =
  MyApp
    { output = "stdout" &= help "visualizer app name, or 'stdout'" &= name "output",
      mode = Gamble &= help "Urn | Gamble | RandVar ",
      ensemble = 5 &= help "amount of independent simulations",
      totalTurns = 100 &= help "Number of turns",
      bankPart = 1.0 &= help "part of bank to be at stake at each turn",
      onWin = 0.5 &= help "part of stake to add",
      onLose = 0.4 &= help "part of stake to subtract"
    }
    &= summary "simulation for Polya's urn and gambling"
    &= help "app for simulation of some ergodic and non-ergodic processes  "
    &= program "simulate"

fracDiv = (/) `on` fromIntegral

modify [] vec _ _ = return vec
modify (cur_index : rest) vec total i
  | i == total = return vec
  | otherwise = do
      sample <- V.read vec cur_index
      V.write vec i sample
      modify rest vec total (i + 1)

-- poya's urn
exercise mod n = do
  vector <- V.replicate n (0 :: Int)
  V.write vector 0 1
  V.write vector 1 2
  mod vector n 2
  ivector <- freeze vector
  return $ IV.toList ivector

func1 rest el = do
  (accum, len) <- get
  let total = if el == 1 then accum + 1 else accum
  let distr = total / len :: Double
  put (total, len + 1)
  return $ distr : rest

modifyToDistr a = reverse $ fst $ runState (foldM func1 [] a) (0, 1)

-- todo - aggregate green/amount for each cell
simulateUrnIO args = do
  let amount = ensemble args
  replicateM amount (simulateUrnIO' args)

simulateUrnIO' args = do
  let turns = totalTurns args
  randIndeces <- mapM (\i -> randomRIO (0, i)) [1 .. turns]
  res <- exercise (modify randIndeces) turns
  return $ modifyToDistr res

simulateGambleIO n turns bank part = replicateM n (simulateGambleIOsingle turns bank part)

simulateGambleIOsingle turns bank part
  | (turns == 0) = return bank
  | otherwise = do
      coin <- randomRIO (0, 1) :: IO Int
      case () of
        _
          | coin == 0 -> simulateGambleIOsingle (turns - 1) (bank - (bank * part * 0.4)) part
          | otherwise -> simulateGambleIOsingle (turns - 1) (bank + (bank * part * 0.5)) part

simulateGambleIOgraphic cmdargs =
  do
    let turns = totalTurns cmdargs
    let n = ensemble cmdargs
    replicateM n (simulateGambleIOgraph cmdargs turns [] 1)

simulateGambleIOgraph cmdargs turns accum bank
  | (turns == 0) = return $ reverse (bank : accum)
  | otherwise = do
      coin <- randomRIO (0, 1) :: IO Int
      let onwin = onWin cmdargs
      let onlose = onLose cmdargs
      let part = bankPart cmdargs
      case () of
        _
          | coin == 0 -> computeForward (bank - (bank * part * onlose))
          | otherwise -> computeForward (bank + (bank * part * onwin))
          where
            computeForward = simulateGambleIOgraph cmdargs (turns - 1) (bank : accum)

simulateRandomVar args = do
	let amount = ensemble args
	replicateM amount $  simulateRandomVar' args
simulateRandomVar' args = do
		let n = totalTurns args
		ret <- replicateM n $ randomRIO (0, 1000) ::IO [Int] -- todo - get from args?
		return $ fromIntegral <$> ret

simulateRandomVarAverage = undefined
-- ex [[1,2,3], [4,5,6]] -> [[1,4], [2,5], [3,6]]
repackLists ([] : xs) = []
repackLists xs = (head <$> xs) : repackLists (tail <$> xs)

-- addMean_ :: Fractional  a => [[a]] -> Int -> [[a]]
addMean_ [] llen = []
addMean_ (l : ls) llen = (sum (l) / (fromIntegral llen) : l) : addMean_ ls llen

-- add mean only for data with > 1 sample
addMean [] = []
addMean ar@(l : ls) = if llen > 1 then addMean_ ar llen else ar
  where
    llen = length l

double_l tt = fromIntegral <$> [1 .. tt]

preprend what ([]) = []
preprend [] to = to
preprend (c : cs) (to : tos) = (c : to) : preprend cs tos

enumerate_with tt = preprend (double_l tt)

show_with_comma = intercalate ", " . map show

-- prepend x axis with turn numbers
data_to_csv total_turns =
  unlines
    . map show_with_comma
    . enumerate_with total_turns
    . addMean
    . repackLists

data_header ensemble = "turns," ++ mean_ ++ (intercalate ", " ["line" ++ show i | i <- [1 .. ensemble]]) ++ "\n"
  where
    mean_ = if ensemble == 1 then "" else "mean,"

startProcessStdin :: String -> IO (ProcessHandle, Handle)
startProcessStdin shellCmd = do
  (Just stdinHandle, _, _, proch) <-
    createProcess
      (shell shellCmd)
        { std_in = CreatePipe,
          std_out = Inherit,
          std_err = Inherit
        }
  return (proch, stdinHandle)

main :: IO ()
main = do
  args <- cmdArgs myApp
  hPutStrLn stderr $ "Arguments: " ++ show args
  let turns = totalTurns args
  let en = ensemble args
  let gamemode = mode args
  experiment_data <- case gamemode of
    Urn -> simulateUrnIO args
    Gamble -> simulateGambleIOgraphic args
    RandVar -> simulateRandomVar args
  let res = data_header en ++ data_to_csv turns experiment_data
  let outfile = output args
  -- can i refactor this to get write function and call it single time? did not wrapped my head around it yet..
  case outfile of
    "stdout" -> putStrLn res -- by default - write ot stdout
    (shellcmd) -> do
      (p, h) <- startProcessStdin shellcmd
      hPutStrLn h res
      hClose h
      waitForProcess p
      return ()
