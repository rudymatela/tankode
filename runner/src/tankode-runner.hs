-- tankode's logic, in Haskell
-- started by Rudy Matela on 2016-12-22 18:30

import Tankode
import Tankode.Data
import Tankode.Show
import Tankode.Constants
import Colour
import Data.Ratio
import List
import Control.Arrow ((***))
import Random
import Data.Maybe
import Data.Function
import System.Console.CmdArgs.Explicit
import System.Environment
import Control.Monad
import System.Posix (ProcessID, signalProcess, sigINT)
import System.IO
import Data.IORef

data Args = Args
  { tankodes :: [String]
  , maxTicks :: Int
  , field    :: Field
  , showHelp :: Bool
  , seed     :: Maybe Int
  , dump     :: Bool
  , nBattles :: Int

  , drawCharge :: Bool
  , drawHealth :: Bool
  , drawScan :: Bool
  , motionBlur :: Bool
  , dumpFrames :: Bool
  , windowSize :: Maybe (Int,Int)
  , closeWindow :: Bool
  }

prepareArgs :: Args -> Mode Args
prepareArgs args =
  mode "speculate" args "" (flagArg (\s a -> Right a {tankodes = s:tankodes a}) "")
  [ "ttime-limit" --= \s a -> a {maxTicks = read s * ticksPerSecond}
  , "ssize"       --= \s a -> a {field = let (w,'x':h) = span (/= 'x') s
                                         in makeField (read w % 1) (read h % 1)}
  , " seed"       --= \s a -> a {seed = Just $ read s}
  , "hhelp"       --.   \a -> a {showHelp = True}
  , "ddump"       --.   \a -> a {dump = True}
  , "nnbattles"   --= \s a -> a {nBattles = read s}

  -- options passed along to the display program
  , " draw-charge"     --. \a -> a {drawCharge  = True}
  , " draw-health"     --. \a -> a {drawHealth  = True}
  , " motion-blur"     --. \a -> a {motionBlur  = True}
  , " draw-scan"       --. \a -> a {drawScan    = True}
  , " dump-frames"     --. \a -> a {dumpFrames  = True}
  , " close-window"    --. \a -> a {closeWindow = True}
  , " no-draw-charge"  --. \a -> a {drawCharge  = False}
  , " no-draw-health"  --. \a -> a {drawHealth  = False}
  , " no-motion-blur"  --. \a -> a {motionBlur  = False}
  , " no-draw-scan"    --. \a -> a {drawScan    = False}
  , " no-dump-frames"  --. \a -> a {dumpFrames  = False}
  , " no-close-window" --. \a -> a {closeWindow = False}
  , " window-size" --= \s a ->
       a {windowSize = let (w,'x':h) = span (/= 'x') s
                       in Just (read w,read h)}
  ]
  where
  (short:long) --= fun = flagReq  (filter (/= " ") [[short],long]) ((Right .) . fun) "X" ""
  (short:long) --. fun = flagNone (filter (/= " ") [[short],long]) fun                   ""

args :: Args
args = Args
  { tankodes = []
  , maxTicks = 100 * ticksPerSecond
  , nBattles = 1
  , field = updateObstacles (++ obstacles) $ makeField 12 8
  , showHelp = False
  , seed = Nothing
  , dump = False
  , drawCharge = False
  , drawHealth = True
  , motionBlur = True
  , drawScan = True
  , dumpFrames = False
  , closeWindow = False
  , windowSize = Nothing
  }
  where
  obstacles =
    [ ((12,8),(12,7),(8,11))
    , ((3,8),(4,8),(4,7)), ((4,7),(4,8),(5,8))
    , ((0,0),(1,0),(0,1))
    , ((7,0),(8,0),(8,1)), ((8,1),(8,0),(9,0))
    , ((5,7/2),(7,9/2),(13/2,5))
    , ((5,7/2),(7,9/2),(11/2,3))
    ]

printSimulation :: Args -> Field -> State -> IO ()
printSimulation args f ts = do
  putStrLn $ showField f
  putStrLn . unlines $ map showId ts
  printStates args 0 0 0 f ts

printStates :: Args -> Int -> Int -> Int -> Field -> State -> IO ()
printStates args n winFor tieFor f ts
  | n >= maxTicks args = return ()
  | winFor >= 2 * ticksPerSecond = return ()
  | tieFor >= 6 * ticksPerSecond = return ()
  | otherwise = do
  putStrLn $ showTick n ts
  ts' <- nextState f ts
  let winFor' = if nActive ts <= 1 then winFor + 1 else winFor
  let tieFor' = if and $ zipWith ((==) `on` integrity) ts ts'
                  then tieFor + 1
                  else 0
  printStates args (n+1) winFor' tieFor' f ts'
  where
  showTick i ts = "tick " ++ show i ++ "\n" ++ showState f ts

mainWith :: Args -> IO ()
mainWith Args{showHelp = True} = print $ helpText [] HelpFormatDefault (prepareArgs args)
mainWith Args{tankodes = []} = putStrLn "must pass at least one tankode"
mainWith args = do
  pidsRef <- newIORef []
  dpid <- if dump args
            then return Nothing
            else Just <$> pipeToDisplay pidsRef args
  -- TODO: need to handle updatding pidsRef when not pipeToDisplay,
  -- in that case, we will need an alternate sigCHLD capture
  -- otherwise we may be signaling unrelated processes to terminate
  -- maybe do this by taking the old handler and calling it?
  gen <- mkNewStdGen (seed args)
  setupAndPrintSimulation gen pidsRef args

setupAndPrintSimulation :: StdGen -> IORef [ProcessID] -> Args -> IO ()
setupAndPrintSimulation gen pidsRef args@Args{field = f, tankodes = ts} = do
-- TODO: make a function places :: Field -> [Loc]
  let (gen1,gen2,gen3) = split3 gen
  let poss = startingPositions f gen1
  tanks <- traverse setupTankode $ map words ts
  let tanks' = zipWith (\t l -> t{loc = l}) (catMaybes tanks) poss
  writeIORef pidsRef (map pid tanks')
  let hs = startingHeadings gen2
  let tanks'' = zipWith (\t h -> t{heading = h}) tanks' hs
  printSimulation args f tanks''
  pids <- readIORef pidsRef
  mapM_ (signalProcess sigINT) pids
  writeIORef pidsRef []
  if nBattles args <= 1
    then putStrLn "end"
    else setupAndPrintSimulation gen3 pidsRef args{nBattles = nBattles args - 1}

main :: IO ()
main = do
  mainWith =<< processArgs (prepareArgs args)

pipeToDisplay :: IORef [ProcessID] -> Args -> IO ProcessID
pipeToDisplay pids args = do
  dn <- dirname <$> getExecutablePath
  pipeTo pids . concat $
    [ [dn ++ "/" ++ "../display/bin/tankode-display"]
    , ["draw-charge"    |       drawCharge args]
    , ["no-draw-health" | not $ drawHealth args]
    , ["no-motion-blur" | not $ motionBlur args]
    , ["no-draw-scan"   | not $ drawScan   args]
    , ["dump-frames"    |       dumpFrames args]
    , ["close-window"   |      closeWindow args]
    , ["window-size=" ++ show w ++ "x" ++ show h
      | let sz = windowSize args, isJust sz, let Just (w,h) = sz]
    ]

dirname :: String -> String
dirname = reverse . tail . dropWhile (/= '/') . reverse

mkNewStdGen :: Maybe Int -> IO StdGen
mkNewStdGen Nothing  = newStdGen
mkNewStdGen (Just x) = return $ mkStdGen x

split3 :: RandomGen g => g -> (g,g,g)
split3 g = (g',g'',g''')
  where
  (g',g'''') = split g
  (g'',g''') = split g''''
