{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jebediah.JACK
    ( Config (..)
    , EventTime ( EventTime
                , unEventTime
                )
    , MIDIEventList
    , Measure (Measure)
    , Beat (Beat)
    , Subdivision (Subdivision)
    , PortName (PortName)
    , at
    , EL.fromPairList
    , EL.merge
    , EL.mergeBy
    , ignoreIncoming
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
                              , readTQueue
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
                       , readEventsFromPort
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
                    deriving (Eq, Ord, Num)
newtype Measure = Measure { unMeasure :: Int }
newtype Beat = Beat { unBeat :: Int }
newtype Subdivision = Subdivision { unSubdivision :: Int }
type EventList = EL.T NFrames M.T
type MIDIEventList = EL.T EventTime M.T
type Ports = (JM.Port Input, JM.Port Output)

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

handleRegistrations :: Config -> TChan PortId -> Client -> Ports -> IO ()
handleRegistrations cfg newPorts client (inp, out) = do
  pId <- atomically $ readTChan newPorts
  port <- portById client pId
  pn <- portName port
  as <- portAliases port
  connectTargets (targetPorts cfg) client out $ pn:as
  connectSources (sourcePorts cfg) client inp $ pn:as

connectPorts :: Config -> Client -> Ports -> String -> IO ()
connectPorts cfg client (inp, out) pn = do
  as <- portAliases =<< portByName client pn
  connectTargets (targetPorts cfg) client out $ pn:as
  connectSources (sourcePorts cfg) client inp $ pn:as

connectTargets :: [PortName] -> Client -> JM.Port Output -> [String] -> IO ()
connectTargets _ _ _ [] = return ()
connectTargets targets client port pns = do
  let cns = intersect pns $ unPortName <$> targets
      want = not . null $ cns
  pn <- portName port
  when want $ mapM_ (\tn -> handleExceptions $ connect client pn tn) cns

connectSources :: [PortName] -> Client -> JM.Port Input -> [String] -> IO ()
connectSources _ _ _ [] = return ()
connectSources sources client port pns = do
  let cns = intersect pns $ unPortName <$> sources
      want = not . null $ cns
  pn <- portName port
  when want $ mapM_ (\tn -> handleExceptions $ connect client tn pn) cns

process :: JM.Port Input -> JM.Port Output -> TQueue EventList -> TQueue EventList -> NFrames -> ExceptionalT Errno IO ()
process inp out iq oq nf = do
  mEvents <- lift $ atomically $ tryReadTQueue oq
  writeEventsToPort out nf $ fromMaybe EL.empty mEvents
  iEvents <- readEventsFromPort inp nf
  lift $ atomically $ writeTQueue iq iEvents

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
                     , sourcePorts :: [PortName]
                     , targetPorts :: [PortName]
                     , startupDelay :: Int
                     , clientName :: String
                     , inName :: String
                     , outName :: String
                     }

instance Default Config
    where def = Config { beatsPerMinute = 120
                       , beatsPerMeasure = 4
                       , subdivisions = 64
                       , sourcePorts = []
                       , targetPorts = []
                       , startupDelay = 1000000
                       , clientName = "jebediah"
                       , inName = "midi-in"
                       , outName = "midi-out"
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

processIncoming :: TQueue EventList -> ([M.T] -> IO ()) -> IO ()
processIncoming iq handle = do
  events <- atomically $ readTQueue iq
  handle $ EL.getBodies events
  processIncoming iq handle

ignoreIncoming :: [M.T] -> IO ()
ignoreIncoming _ = return ()

jebediahMain :: Config -> MIDIEventList -> ([M.T] -> IO ()) -> IO ()
jebediahMain cfg el handleEvents = do
  handleExceptions $ withClientDefault (clientName cfg) $ \client -> do
    (newPorts, events, msgs) <- lift $ do
      s <- SampleRate <$> getSampleRate client
      b <- BufferSize <$> getBufferSize client
      n <- newTChanIO
      e <- newTQueueIO
      m <- newTQueueIO
      _ <- forkIO $ populateEvents cfg s b el e
      _ <- forkIO $ processIncoming m handleEvents
      return $ (n, e, m)
    withPortRegistration client (portRegister newPorts) $ do
      withPort client (outName cfg) $ \out ->
        withPort client (inName cfg) $ \inp ->
          withProcess client (process inp out msgs events) $
            withActivation client $ lift $ do
              let ports = (inp, out)
              getPorts client >>= mapM_ (connectPorts cfg client ports)
              withBreaking $ handleRegistrations cfg newPorts client ports
