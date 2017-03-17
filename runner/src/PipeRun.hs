module PipeRun
  ( pun
  , pipeTo
  , propagateSIGTERM
  )
where

import System.Posix
import System.Posix.Process
import System.Posix.Signals
import System.IO
import System.Exit

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

-- pipe-runs a program, stdio is bound to a pair of handles
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

-- pipes the current process to a child.
-- if the child terminates, sends sigTERM to the current process group
pipeTo :: [String] -> IO ()
pipeTo as = do
  (fromParent,toChild) <- createPipe
  cpid <- forkProcess (act fromParent)
  terminateOnSIGCHLD cpid
  dupTo toChild stdOutput
  closeFd toChild
  hSetBuffering stdout LineBuffering
  return ()
  where
  act fromParent = do
    dupTo fromParent stdInput
    closeFd fromParent
    hSetBuffering stdin LineBuffering
    execFile as

terminateOnSIGCHLD :: ProcessID -> IO ()
terminateOnSIGCHLD pid = do
  installHandler sigCHLD (CatchInfo handler) Nothing
  return ()
  where
  handler (SignalInfo s _ (SigChldInfo pid' _ _))
    | s == sigCHLD && pid' == pid = signalGroup sigTERM
  handler _ = return ()
  signalGroup signal = do
    gid <- getProcessGroupID
    signalProcessGroup signal gid
    exitSuccess

propagateSIGTERM :: IO ()
propagateSIGTERM = do
  propagate sigTERM
  propagate sigINT
  propagate sigTSTP
  propagate sigHUP
  propagate sigPIPE
  propagate sigQUIT
  return ()
  where
  propagate signal = installHandler signal (CatchOnce $ signalGroup signal) Nothing
  signalGroup signal = hPrint stderr signal >> getProcessGroupID >>= signalProcessGroup signal
