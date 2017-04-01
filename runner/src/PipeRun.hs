-- |
-- Module      : PipeRun
-- Copyright   : (c) 2017  Rudy Matela
-- License     : LGPL 2.1 (see the file LICENSE)
--
-- Utilities to run programs with pipes.
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
import Data.IORef

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
pun :: [String] -> IO (ProcessID,Handle,Handle)
pun as = do
  (pid,fin,fout) <- pun' as
  hin  <- fdToHandle fin
  hout <- fdToHandle fout
  hSetBuffering hin  LineBuffering
  hSetBuffering hout LineBuffering
  return (pid,hin,hout)
  where
  pun' as = porkProcessStdIO (execFile as)

-- pipes the current process to a child.
-- if the child terminates, sends sigTERM to the current process group
pipeTo :: IORef [ProcessID] -> [String] -> IO ProcessID
pipeTo cpidsRef as = do
  (fromParent,toChild) <- createPipe
  cpid <- forkProcess (act fromParent)
  terminateOnSIGCHLD cpidsRef cpid
  dupTo toChild stdOutput
  closeFd toChild
  hSetBuffering stdout LineBuffering
  return cpid
  where
  act fromParent = do
    dupTo fromParent stdInput
    closeFd fromParent
    hSetBuffering stdin LineBuffering
    execFile as

terminateOnSIGCHLD :: IORef [ProcessID] -> ProcessID -> IO ()
terminateOnSIGCHLD pidsRef pid = do
  installHandler sigCHLD (CatchInfo (handler pidsRef)) Nothing
  return ()
  where
  handler pidsRef (SignalInfo s _ (SigChldInfo pid' _ _)) | s == sigCHLD =
    if pid' == pid
      then do
        mapM_ (signalProcess sigINT) =<< readIORef pidsRef
        signalProcess sigTERM =<< getProcessID
        exitSuccess
      else modifyIORef pidsRef (filter (/= pid'))
  handler _ _ = return ()

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
