module Language.Verilog.Simulator
  ( Simulator
  , SimCommand  (..)
  , SimResponse (..)
  , simulator
  ) where

import Control.Monad (when)
import Data.Array.IO
import Data.Bits
import Data.IORef
import Data.Monoid
import System.IO

import Data.VCD hiding (Var, step)
import qualified Data.VCD as VCD

import Data.BitVec
import Language.Verilog.Netlist

--check msg = putStrLn msg >> hFlush stdout

-- | A Simulator executes 'SimCommand's.
type Simulator = SimCommand -> IO (Maybe SimResponse)

-- | Simulation commands.
data SimCommand
  = Init         (Maybe FilePath)
  | Step
  | GetSignalId  Path
  | GetSignal    NetId
  | Close

-- | Simulation responses.
data SimResponse
  = Id    NetId  -- ^ Response to GetSignalId.
  | Value BitVec -- ^ Response to GetSignal.

-- | Builds a 'Simulator' given a 'Netlist'.
simulator :: Netlist BlackBoxInit -> IO Simulator
simulator netlist' = do
  let netlist = sortTopo netlist'
  memory <- memory netlist
  vcd    <- newIORef Nothing
  sample <- newIORef $ return ()
  step   <- newIORef $ return ()
  return $ \ cmd -> case cmd of
    Init        file -> initialize netlist memory vcd file sample step
    Step             -> readIORef step >>= id >> return Nothing
    GetSignalId path -> return $ getSignalId netlist path
    GetSignal   id   -> readArray memory id >>= return . Just . Value
    Close            -> close vcd sample step >> return Nothing

getSignalId :: Netlist BlackBoxInit -> Path -> Maybe SimResponse
getSignalId netlist path = case lookup path paths' of
  Nothing -> Nothing
  Just i  -> Just $ Id i
  where
  paths = [ (paths, id) | Reg id _ paths _ <- netlist ] ++ [ (paths, id) | Var id _ paths _ <- netlist ] 
  paths' = [ (path, id) | (paths, id) <- paths, path <- paths ]

type Memory = IOArray Int BitVec

memory :: Netlist BlackBoxInit -> IO Memory
memory netlist
  | null netlist = error "Empty netlist, nothing to simulate."
  | otherwise    = newArray (0, maximum ids) 0
  where
  ids = concatMap f netlist
  f a = case a of
    Var  a _ _ _ -> [a]
    Reg  a _ _ _ -> [a]
    BBox _ _ _   -> []

initialize :: Netlist BlackBoxInit -> Memory -> IORef (Maybe VCDHandle) -> Maybe FilePath -> IORef (IO ()) -> IORef (IO ()) -> IO (Maybe SimResponse)
initialize netlist memory vcd file sample step = do
  close vcd sample step
  mapM_ (initializeNet memory) netlist 
  case file of
    Nothing -> return ()
    Just file -> do
      h <- openFile file WriteMode
      vcd' <- newVCD h S
      writeIORef vcd $ Just vcd'
      writeIORef sample $ VCD.step vcd' 1
      mapM_ (f memory vcd' sample) netlist
  netlist <- mapM initializeBBox netlist
  initializeStep netlist memory sample step
  return Nothing
  where
  f :: Memory -> VCDHandle -> IORef (IO ()) -> Net BlackBoxInit -> IO ()
  f memory vcd sample a = case a of
    BBox _ _ _ -> return ()
    _ -> mapM_ (\ signal -> do
      sample' <- var vcd signal $ bitVec width 0
      modifyIORef sample (>> (readArray memory i >>= sample'))
      ) signals
    where
    (i, width, signals) = case a of
      Reg i w p _ -> (i, w, p)
      Var i w p _ -> (i, w, p)
      BBox _ _ _ -> undefined

initializeNet :: Memory -> Net BlackBoxInit -> IO ()
initializeNet memory a = case a of
  Var  i w _ _ -> writeArray memory i $ bitVec w 0
  Reg  i w _ _ -> writeArray memory i $ bitVec w 0
  BBox _ _ _   -> return ()

initializeBBox :: Net BlackBoxInit -> IO (Net BlackBoxStep)
initializeBBox a = case a of
  Var a b c d -> return $ Var a b c d
  Reg a b c d -> return $ Reg a b c d
  BBox i o init -> init >>= return . BBox i o

initializeStep :: Netlist BlackBoxStep -> Memory -> IORef (IO ()) -> IORef (IO ()) -> IO ()
initializeStep netlist memory sample step = do
  let steps = map stepNet netlist
  writeIORef step $ do
    sequence_ steps
    readIORef sample >>= id
  where
  read   = readArray memory
  write' = writeMemory memory
  stepNet :: Net BlackBoxStep -> IO ()
  stepNet a = case a of
    BBox inputs outputs f -> do
      outs <- mapM read inputs >>= f
      sequence_ [ write' a b | (a, b) <- zip outputs outs ]
    Reg q _ _ d -> read d >>= write' q
    Var i _ _ expr -> case expr of
      AInput        -> return ()
      AVar    a     -> read a >>= write
      AConst  a     -> write a
      ASelect a b c -> do { a <- read a; b <- read b; c <- read c; write $ select a (b, c) }
      ABWNot  a     -> read a >>= write . complement
      ABWAnd  a b   -> do { a <- read a; b <- read b; write $ a .&. b }
      ABWXor  a b   -> do { a <- read a; b <- read b; write $ a `xor` b }
      ABWOr   a b   -> do { a <- read a; b <- read b; write $ a .|. b }
      AMul    a b   -> do { a <- read a; b <- read b; write $ a * b }
      AAdd    a b   -> do { a <- read a; b <- read b; write $ a + b }
      ASub    a b   -> do { a <- read a; b <- read b; write $ a - b }
      AShiftL a b   -> do { a <- read a; b <- read b; write $ shiftL a $ fromIntegral $ value b }
      AShiftR a b   -> do { a <- read a; b <- read b; write $ shiftR a $ fromIntegral $ value b }
      AEq     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a == value b then 1 else 0) }
      ANe     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a /= value b then 1 else 0) }
      ALt     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a <  value b then 1 else 0) }
      ALe     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a <= value b then 1 else 0) }
      AGt     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a >  value b then 1 else 0) }
      AGe     a b   -> do { a <- read a; b <- read b; write $ bitVec 1 (if value a >= value b then 1 else 0) }
      AMux    a b c -> do { a <- read a; b <- read b; c <- read c; write (if value a /= 0 then b else c) }
      AConcat a b   -> do { a <- read a; b <- read b; write $ mappend a b }
      where
      write = write' i

writeMemory :: Memory -> Int -> BitVec -> IO ()
writeMemory memory i a = do
  b <- readArray memory i
  when (width b /= width a) $ error $ "Memory update with different bit-vector width:  index: " ++ show i ++ "  old: " ++ show b ++ "  new: " ++ show a
  writeArray memory i a

close :: IORef (Maybe VCDHandle) -> IORef (IO ()) -> IORef (IO ()) -> IO ()
close vcd sample step = do
  vcd' <- readIORef vcd
  case vcd' of
    Nothing -> return ()
    Just vcd -> hClose $ handle vcd
  writeIORef vcd    $ Nothing
  writeIORef sample $ return ()
  writeIORef step   $ return ()

