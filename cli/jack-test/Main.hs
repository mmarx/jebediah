module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent ( forkIO
                          , threadDelay
                          )
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad ( unless
                     , when
                     )
import Control.Monad.Exception.Synchronous (ExceptionalT)
import Control.Monad.Trans.Class (lift)
import qualified Data.EventList.Absolute.TimeBody as EventList
import Data.List (intersect)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Foreign.C.Error (Errno)
import System.Posix.Signals ( installHandler
                            , keyboardSignal
                            , Handler (Catch)
                            )

import qualified Sound.MIDI.Message as MIDI
import qualified Sound.MIDI.Message.Channel as CM
import qualified Sound.MIDI.Message.Channel.Voice as VM
import Sound.JACK hiding (withPort)
import Sound.JACK.MIDI ( withPort
                       , writeEventsToPort
                       )
import qualified Sound.JACK.MIDI as JM

targetPorts :: [String]
targetPorts = [ "Bass-Station-II:midi/capture_1"
              , "Nord-Electro-4:midi/capture_1"
              , "MIDISPORT-2x2-Anniv:midi/capture_1"
              , "midi-monitor:input"
              ]

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

handleRegistrations :: [String] -> TChan PortId -> Client -> Port typ Output -> IO ()
handleRegistrations targets newPorts client out = do
  pId <- atomically $ readTChan newPorts
  port <- portById client pId
  pn <- portName port
  as <- portAliases port
  connectTargets' targets client out $ pn:as

connectTargets :: [String] -> Client -> Port typ Output -> String -> IO ()
connectTargets targets client out pn = do
  as <- portAliases =<< portByName client pn
  connectTargets' targets client out $ pn:as

connectTargets' :: [String] -> Client -> Port typ Output -> [String] -> IO ()
connectTargets' _ _ _ [] = return ()
connectTargets' targets client out pns = do
  on <- portName out
  let cns = intersect pns targets
      want = not . null $ cns
  when want $ do
    mapM_ (\tn -> handleExceptions $ connect client on tn) cns

type EL = EventList.T NFrames MIDI.T

process :: JM.Port Output -> TQueue EL -> NFrames -> ExceptionalT Errno IO ()
process out queue nf = do
  mEvents <- lift $ atomically $ tryReadTQueue queue
  writeEventsToPort out nf $ fromMaybe EventList.empty mEvents

nOn = MIDI.Channel $ CM.Cons { CM.messageChannel = CM.toChannel 1
                             , CM.messageBody = CM.Voice $ VM.NoteOn (VM.toPitch 60) (VM.toVelocity 60)
                             }
nOff = MIDI.Channel $ CM.Cons { CM.messageChannel = CM.toChannel 1
                              , CM.messageBody = CM.Voice $ VM.NoteOff (VM.toPitch 60) (VM.toVelocity 60)
                              }

bpm :: Int
bpm = 120

subdivision :: Int
subdivision = 64

beatsPerMeasure :: Int
beatsPerMeasure = 2

newtype EventTime = EventTime Int

at :: Int -> Int -> Int -> EventTime
at measure beat subdiv = EventTime $ (measure - 1) * eventsPerMeasure +
                         (beat - 1) * eventsPerBeat +
                         (subdiv - 1)
    where eventsPerBeat = subdivision
          eventsPerMeasure = eventsPerBeat * beatsPerMeasure

type MEL = EventList.T EventTime MIDI.T

eL :: MEL
eL = EventList.fromPairList [ (at 1 1 1, nOn)
                            , (at 2 1 1, nOff)
                            , (at 3 1 1, nOn)
                            , (at 3 2 1, nOff)
                            , (at 3 2 16, nOn)
                            , (at 4 1 1, nOff)
                            ]

toEventLists :: Int -> Int -> MEL -> [EL]
toEventLists sr bs = map EventList.fromPairList .
                     toEventLists' 0 [] [] .
                     EventList.toPairList .
                     EventList.mapTime (\(EventTime t) ->
                                            NFrames $ round $ fpe * (fromIntegral t))
    where bs' = fromIntegral bs
          bps = bpm % 60
          eps = bps * fromIntegral subdivision
          fpe = (fromRational $ fromIntegral sr) / eps
          toEventLists' _ acc cur [] = reverse $ (reverse cur):acc
          toEventLists' off acc cur el@((NFrames time, body):rest)
              | time <= off * bs' = toEventLists' off acc ((NFrames $ time `mod` bs', body):cur) rest
              | otherwise = toEventLists' (off + 1) (cur:acc) [] el

populateEvents :: Int -> Int -> TQueue EL -> IO ()
populateEvents sr bs events = do
  threadDelay 1000000
  atomically $ mapM_ (writeTQueue events) $ toEventLists sr bs eL

main :: IO ()
main = do
  putStrLn "Hello, world!"
  handleExceptions $ withClientDefault "jebediah" $ \client -> do
    (newPorts, events) <- lift $ do
      s <- getSampleRate client
      b <- getBufferSize client
      n <- newTChanIO
      e <- newTQueueIO
      _ <- forkIO $ populateEvents s b e
      return $ (n, e)
    withPortRegistration client (portRegister newPorts) $
      withPort client "midi" $ \out ->
         withProcess client (process out events) $
           withActivation client $ lift $ do
             getPorts client >>= mapM_ (connectTargets targetPorts client out)
             withBreaking $ handleRegistrations targetPorts newPorts client out
