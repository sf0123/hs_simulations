import Control.Monad (foldM, forM_, replicateM, replicateM_)
import Control.Monad.ST
import Control.Monad.State (get, put, runState)
import Data.Function
import Data.List (intercalate, zip)
import Data.Random.Distribution.Bernoulli
import Data.Random.Distribution.Uniform
import Data.Random.RVar
import Data.STRef
import Data.Vector.Unboxed (freeze)
import qualified Data.Vector.Unboxed as IV
import qualified Data.Vector.Unboxed.Mutable as V
import System.Console.CmdArgs
import System.IO
import System.Process
import System.Random (RandomGen, StdGen, mkStdGen, randomR, randomRIO)
import System.Random.Stateful (RandomGenM, StateGenM, genRange, randomM, randomRM, runStateGen_)

data SimMode = Urn | Gamble | Uni | Bern deriving (Data, Typeable, Show, Eq)

data MyApp = MyApp
  { output :: String,
    mode :: SimMode,
    ensemble :: Int,
    totalTurns :: Int,
    bankPart :: Double,
    onWin :: Double,
    onLose :: Double,
    winProb :: Double,
    seed :: Int
  }
  deriving (Data, Typeable, Show, Eq)

myApp :: MyApp
myApp =
  MyApp
    { output = "stdout" &= help "visualizer app name, or 'stdout'" &= name "output",
      mode = Gamble &= help "Urn | Gamble | Uni | Bern",
      ensemble = 4 &= help "amount of independent simulations",
      totalTurns = 15 &= help "Number of turns",
      bankPart = 1.0 &= help "part of bank to be at stake at each turn",
      onWin = 0.5 &= help "part of stake to add",
      winProb = 0.5 &= help "probability to win on single turn",
      onLose = 0.4 &= help "part of stake to subtract",
      seed = 0 &= help "integer as seed"
    }
    &= summary "simulation for Polya's urn and gambling"
    &= help "app for simulation of some ergodic and non-ergodic processes  "
    &= program "simulate"

fracDiv = (/) `on` fromIntegral

urnStep ::
  (V.PrimMonad m, V.Unbox a) =>
  [Int] ->
  V.MVector (V.PrimState m) a ->
  Int ->
  Int ->
  m (V.MVector (V.PrimState m) a)
urnStep [] vec _ _ = return vec
urnStep (cur_index : rest) vec total i
  | i == total = return vec
  | otherwise = do
      sample <- V.read vec cur_index
      V.write vec i sample
      urnStep rest vec total (i + 1)

-- takes list of 1 and 0s and converts
-- relative amount of 1s to its position
modifyToDistr :: (Foldable t, Eq a, Num a) => t a -> [Double]
modifyToDistr a = reverse . fst . runState (foldM f [] a) $ (0, 1)
  where
    f acc el = do
      (accum, len) <- get
      let total = if el == 1 then accum + 1 else accum
      let distr = total / len :: Double
      put (total, len + 1)
      return $ distr : acc

genvec :: (V.Unbox a, Num a) => Int -> [Int] -> [a]
genvec n indeces = runST $ do
  v <- V.new n
  V.write v 0 1
  V.write v 1 2
  urnStep indeces v n 2
  IV.toList <$> freeze v

simulateUrnOnce n = do
  indeces <- genDistr n
  return $ modifyToDistr $ (genvec n indeces :: [Int])

-- simulateUrnN :: RandomGenM g r m =>g -> Int -> m [[Double]]
simulateUrnN args =
  let amount = ensemble args
      t = totalTurns args
   in (replicateM amount $ (simulateUrnOnce t))

-- genDistr r t = (fmap fromIntegral) <$> (mapM (\i -> randomRM (0, i) r) [1 .. t])
genDistr t = (fmap fromIntegral) <$> (mapM $ uniform 0) [1 .. t]

-- simulateGambleN :: (RandomGen r) => r -> MyApp -> [[Double]]
simulateGambleN cmdargs =
  let turns = totalTurns cmdargs
      n = ensemble cmdargs
   in (replicateM n $ simulateGambleOnce cmdargs turns [] 1)

requireInt :: Int -> Int
requireInt = id

simulateGambleOnce cmdargs 0 accum bank = return $ (reverse (bank : accum))
simulateGambleOnce cmdargs turns accum bank = do
  let p = winProb cmdargs
  coin' <- bernoulli p
  let coin = coin' :: Int
  let onwin = onWin cmdargs
  let onlose = onLose cmdargs
  let part = bankPart cmdargs
  let new_bank = case coin of
        0 -> bank - (bank * part * onlose)
        otherwise -> bank + (bank * part * onwin)
  simulateGambleOnce cmdargs (turns - 1) (bank : accum) new_bank

simulateBernoullyRandomVarN args =
  let p = winProb args
   in simulateRandomVarN (bernoulli p) args

simulateUniRandomVarN = simulateRandomVarN $ uniform 0 1000

simulateRandomVarN distr args =
  let ens = (ensemble args)
      n = totalTurns args
      arrToDouble = fmap $ fromIntegral . requireInt
   in replicateM ens (arrToDouble <$> (replicateM n distr))

-- ex [[1,2,3], [4,5,6]] -> [[1,4], [2,5], [3,6]]
repackLists ([] : xs) = []
repackLists xs = (head <$> xs) : repackLists (tail <$> xs)

addMean_ :: (Fractional a) => [[a]] -> Int -> [[a]]
addMean_ [] llen = []
addMean_ (l : ls) llen = (sum (l) / (fromIntegral llen) : l) : addMean_ ls llen

-- add mean only for data with > 1 sample
addMean [] = []
addMean ar@(l : ls)
  | llen > 1 = addMean_ ar llen
  | otherwise = ar
  where
    llen = length l

double_l tt = fromIntegral <$> [1 .. tt]

preprend what ([]) = []
preprend [] to = to
preprend (c : cs) (to : tos) = (c : to) : preprend cs tos

enumerate_with tt = preprend (double_l tt)

show_with_comma = intercalate ", " . map show

-- prepend x axis with turn numbers
-- data_to_csv :: [[Double]] -> [String]
data_to_csv total_turns =
  unlines
    . map show_with_comma
    . enumerate_with total_turns
    . addMean
    . repackLists

data_header ensemble =
  "turns,"
    ++ mean_label
    ++ line_labels
    ++ "\n"
  where
    mean_label = if ensemble == 1 then "" else "mean,"
    line_labels = "line" ++ intercalate ",line" line_numbers
    line_numbers = show <$> [1 .. ensemble]

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
  let s = seed args
  rseed <- if s == 0 then randomRIO (0, maxBound) else pure s
  let rnd = mkStdGen rseed
  let turns = totalTurns args
  let en = ensemble args
  let gamemode = mode args
  -- runStateGen_ rnd
  let sim = case gamemode of
        Urn -> simulateUrnN args
        Gamble -> simulateGambleN args
        Uni -> simulateUniRandomVarN args
        Bern -> simulateBernoullyRandomVarN args
  let experiment_data = runStateGen_ rnd $ runRVar sim
  let res = data_header en ++ data_to_csv turns experiment_data
  let outfile = output args
  case outfile of
    "stdout" -> putStrLn res -- by default - write ot stdout
    (shellcmd) -> do
      (p, h) <- startProcessStdin shellcmd
      hPutStrLn h res
      hClose h
      waitForProcess p
      return ()
