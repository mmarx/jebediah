module Jebediah.JACK
    ( Config (..)
    , EventTime (EventTime)
    , MIDIEventList
    , Measure (Measure)
    , Beat (Beat)
    , Subdivision (Subdivision)
    , PortName (PortName)
    , at
    , EL.fromPairList
    , jebediahMain
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent ( forkIO
                          , threadDelay
                          )
import Control.Concurrent.MVar ( newEmptyMVar
                               , putMVar
                               , takeMVar
                               )
import Control.Concurrent.STM ( TChan
                              , TQueue
                              , atomically
                              , newTChanIO
                              , newTQueueIO
                              , readTChan
                              , tryReadTQueue
                              , writeTChan
                              , writeTQueue
                              )
import Control.Monad (when)
import Control.Monad.Exception.Synchronous (ExceptionalT)
import Control.Monad.Trans.Class (lift)
import Data.Default (Default, def)
import qualified Data.EventList.Absolute.TimeBody as EL
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import Foreign.C.Error (Errno)
import qualified Sound.MIDI.Message as M
import Sound.JACK hiding (withPort)
import Sound.JACK.MIDI ( withPort
                       , writeEventsToPort
                       )
import qualified Sound.JACK.MIDI as JM
import System.Posix.Signals ( installHandler
                            , keyboardSignal
                            , Handler (Catch)
                            )

newtype PortName = PortName { unPortName :: String }
newtype SampleRate = SampleRate { unSampleRate :: Int }
newtype BufferSize = BufferSize { unBufferSize :: Int }
newtype EventTime = EventTime { unEventTime :: Int }
newtype Measure = Measure { unMeasure :: Int }
newtype Beat = Beat { unBeat :: Int }
newtype Subdivision = Subdivision { unSubdivision :: Int }
type EventList = EL.T NFrames M.T
type MIDIEventList = EL.T EventTime M.T


portRegister :: TChan PortId -> PortId -> Bool -> IO ()
portRegister newPorts pId _ = do
  atomically $ writeTChan newPorts pId

withBreaking :: IO () -> IO ()
withBreaking stuff = do
  quitting <- newEmptyMVar
  _ <- installHandler keyboardSignal
       (Catch $ do
           threadDelay 1000000
           putMVar quitting ())
       Nothing
  _ <- forkIO stuff
  _ <- takeMVar quitting
  return ()

handleRegistrations :: [PortName] -> TChan PortId -> Client -> Port typ Output -> IO ()
handleRegistrations targets newPorts client out = do
  pId <- atomically $ readTChan newPorts
  port <- portById client pId
  pn <- portName port
  as <- portAliases port
  connectTargets' targets client out $ pn:as

connectTargets :: [PortName] -> Client -> Port typ Output -> String -> IO ()
connectTargets targets client out pn = do
  as <- portAliases =<< portByName client pn
  connectTargets' targets client out $ pn:as

connectTargets' :: [PortName] -> Client -> Port typ Output -> [String] -> IO ()
connectTargets' _ _ _ [] = return ()
connectTargets' targets client out pns = do
  on <- portName out
  let cns = intersect pns $ unPortName <$> targets
      want = not . null $ cns
  when want $ do
    mapM_ (\tn -> handleExceptions $ connect client on tn) cns

process :: JM.Port Output -> TQueue EventList -> NFrames -> ExceptionalT Errno IO ()
process out queue nf = do
  mEvents <- lift $ atomically $ tryReadTQueue queue
  writeEventsToPort out nf $ fromMaybe EL.empty mEvents

toEventLists :: Config -> SampleRate -> BufferSize -> MIDIEventList -> [EventList]
toEventLists cfg sr bs = map EL.fromPairList .
                         toEventLists' 0 [] [] .
                         EL.toPairList .
                         EL.mapTime (\t -> NFrames $ round $
                                     fpe * (fromIntegral $ unEventTime t))
    where bs' = fromIntegral $ unBufferSize bs
          bpm = beatsPerMinute cfg
          bps = bpm % 60
          eps = bps * fromIntegral (subdivisions cfg)
          fpe = (fromRational $ fromIntegral $ unSampleRate sr) / eps
          toEventLists' _ acc cur [] = reverse $ (reverse cur):acc
          toEventLists' off acc cur el@((NFrames time, body):rest)
              | time <= off * bs' = toEventLists' off acc ((NFrames $ time `mod` bs', body):cur) rest
              | otherwise = toEventLists' (off + 1) ((reverse cur):acc) [] el

data Config = Config { beatsPerMinute :: Int
                     , beatsPerMeasure :: Int
                     , subdivisions :: Int
                     , targetPorts :: [PortName]
                     , startupDelay :: Int
                     , clientName :: String
                     , outName :: String
                     }

instance Default Config
    where def = Config { beatsPerMinute = 120
                       , beatsPerMeasure = 4
                       , subdivisions = 64
                       , targetPorts = []
                       , startupDelay = 1000000
                       , clientName = "jebediah"
                       , outName = "midi"
                       }

populateEvents :: Config -> SampleRate -> BufferSize -> MIDIEventList -> TQueue EventList -> IO ()
populateEvents cfg sr bs el events = do
  threadDelay $ startupDelay cfg
  atomically $ mapM_ (writeTQueue events) $ toEventLists cfg sr bs el

at :: Config -> Measure -> Beat -> Subdivision -> EventTime
at cfg measure beat subdiv = EventTime $ ((unMeasure measure) - 1) * eventsPerMeasure +
                             ((unBeat beat) - 1) * eventsPerBeat +
                             ((unSubdivision subdiv) - 1)
    where eventsPerBeat = subdivisions cfg
          eventsPerMeasure = eventsPerBeat * beatsPerMeasure cfg

jebediahMain :: Config -> MIDIEventList -> IO ()
jebediahMain cfg el = do
  let tgts = targetPorts cfg
  handleExceptions $ withClientDefault (clientName cfg) $ \client -> do
    (newPorts, events) <- lift $ do
      s <- SampleRate <$> getSampleRate client
      b <- BufferSize <$> getBufferSize client
      n <- newTChanIO
      e <- newTQueueIO
      _ <- forkIO $ populateEvents cfg s b el e
      return $ (n, e)
    withPortRegistration client (portRegister newPorts) $
      withPort client (outName cfg) $ \out ->
         withProcess client (process out events) $
           withActivation client $ lift $ do
             getPorts client >>= mapM_ (connectTargets tgts client out)
             withBreaking $ handleRegistrations tgts newPorts client out
