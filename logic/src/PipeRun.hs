module PipeRun
  ( pun
  )
where

import System.Posix
import System.IO

execFile :: [String] -> IO a
execFile (cmm:args) = do
  env <- getEnvironment
  executeFile cmm True args (Just env)

-- pipe-forks a process
porkProcess :: (Fd -> Fd -> IO ()) -> IO (ProcessID,Fd,Fd)
porkProcess act = do
  (fromChild,toParent) <- createPipe
  (fromParent,toChild) <- createPipe
  cpid <- forkProcess (act fromParent toParent)
  return (cpid, fromChild, toChild)

-- pipe-forks a process, child stdio is bound to pipe returned to parent
porkProcessStdIO :: IO () -> IO (ProcessID,Fd,Fd)
porkProcessStdIO act' = porkProcess act
  where
  act fromParent toParent = do
    dupTo fromParent stdInput
    closeFd fromParent
    dupTo toParent stdOutput
    closeFd toParent
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout LineBuffering
    act'

-- pipe-runs a program, stdio is bound to a handles
pun :: [String] -> IO (Handle,Handle)
pun as = do
  (_,fin,fout) <- pun' as
  hin  <- fdToHandle fin
  hout <- fdToHandle fout
  hSetBuffering hin  LineBuffering
  hSetBuffering hout LineBuffering
  return (hin,hout)
  where
  pun' as = porkProcessStdIO (execFile as)
