-- tankode-runner.hs -- main runner of the Tankode game
--
-- Copyright (C) 2016, 2017  Rudy Matela
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU Lesser General Public License
-- version 2.1, as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
--
-- (development started by Rudy Matela on 2016-12-22 18:30)
import Tankode
import Tankode.Data
import Tankode.Show
import Tankode.Constants
import Colour
import Data.Ratio
import List hiding (split)
import Control.Arrow ((***))
import Random
import Data.Maybe
import Data.Function
import System.Console.CmdArgs.Explicit
import System.Environment
import Control.Monad
import System.Posix (ProcessID, signalProcess, sigINT, getProcessStatus)
import System.IO
import Data.IORef
import qualified Tankode.Obstacles as Obstacles

data Args = Args
  { tankodes :: [String]
  , maxTicks :: Int
  , fields   :: [Field]
  , showHelp :: Bool
  , showVersion :: Bool
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
  , sound :: Bool
  }

addField :: Field -> Args -> Args
addField f args@Args{fields = fs} = args{fields = f:fs}

update1stField :: (Field -> Field) -> Args -> Args
update1stField upd Args{fields = []} = error "undefined field, first set field size with -s WxH"
update1stField upd args@Args{fields = (f:fs)} = args{fields = upd f:fs}

prepareArgs :: Args -> Mode Args
prepareArgs args =
  mode "speculate" args "" (flagArg (\s a -> Right a {tankodes = s:tankodes a}) "")
  [ "ttime-limit" --= \s a -> a {maxTicks = read s * ticksPerSecond}
  , "ssize"       --= \s -> let (w,'x':h) = span (/= 'x') s
                            in addField $ makeField (read w % 1) (read h % 1)
  , " seed"       --= \s a -> a {seed = Just $ read s}
  , "hhelp"       --.   \a -> a {showHelp = True}
  , "Vversion"    --.   \a -> a {showVersion = True}
  , "ddump"       --.   \a -> a {dump = True}
  , "nnbattles"   --= \s a -> a {nBattles = read s}
  , "oobstacle"   --= \s -> update1stField $ \f -> let w = width f; h = height f
                                                   in addObstacles (readObstacles w h s) f

  -- options passed along to the display program
  , " draw-charge"     --. \a -> a {drawCharge  = True}
  , " draw-health"     --. \a -> a {drawHealth  = True}
  , " motion-blur"     --. \a -> a {motionBlur  = True}
  , " draw-scan"       --. \a -> a {drawScan    = True}
  , " dump-frames"     --. \a -> a {dumpFrames  = True}
  , " close-window"    --. \a -> a {closeWindow = True}
  , " sound"           --. \a -> a {sound       = True}
  , " no-draw-charge"  --. \a -> a {drawCharge  = False}
  , " no-draw-health"  --. \a -> a {drawHealth  = False}
  , " no-motion-blur"  --. \a -> a {motionBlur  = False}
  , " no-draw-scan"    --. \a -> a {drawScan    = False}
  , " no-dump-frames"  --. \a -> a {dumpFrames  = False}
  , " no-close-window" --. \a -> a {closeWindow = False}
  , " no-sound"        --. \a -> a {sound       = False}
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
  , fields = []
  , showHelp = False
  , showVersion = False
  , seed = Nothing
  , dump = False
  , drawCharge = False
  , drawHealth = True
  , motionBlur = True
  , drawScan = False
  , dumpFrames = False
  , closeWindow = False
  , windowSize = Nothing
  , sound = True
  }

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
mainWith Args{showVersion = True} = putStrLn "Tankode 0.0.1"
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
  setupAndPrintSimulation gen pidsRef dpid args

setupAndPrintSimulation :: StdGen -> IORef [ProcessID] -> Maybe ProcessID -> Args -> IO ()
setupAndPrintSimulation gen pidsRef dpid args@Args{fields = []} =
  setupAndPrintSimulation gen pidsRef dpid args
    { fields =
        [ addObstacles (Obstacles.old      12 8) $ makeField 12 8
        , addObstacles (Obstacles.ox       12 8) $ makeField 12 8
        , addObstacles (Obstacles.rounded  11 6) $ makeField 11 6
        , addObstacles (Obstacles.ox       11 7) $ makeField 11 7
        , addObstacles (Obstacles.rounded3  8 8) $ makeField  8 8
        , addObstacles (Obstacles.bowtie   12 8) $ makeField 12 8
        , addObstacles (Obstacles.corners   7 7) $ makeField  7 7
        ]
    }
setupAndPrintSimulation gen pidsRef dpid args@Args{fields = f:fs, tankodes = ts} = do
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
    then do
      putStrLn "end"
      when (isJust dpid) $
        getProcessStatus True False (fromJust dpid) >> return ()
    else setupAndPrintSimulation gen3 pidsRef dpid args{fields = fs ++ [f], nBattles = nBattles args - 1}

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
    , ["no-sound"       | not $      sound args]
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
