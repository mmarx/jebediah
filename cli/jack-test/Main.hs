module Main (main) where

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
import Data.Monoid ((<>))
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
    putStrLn $ (show cns) <> ": " <> (show pns)
    mapM_ (\tn -> handleExceptions $ connect client on tn) cns

type EL = EventList.T NFrames MIDI.T

process :: JM.Port Output -> TQueue EL -> NFrames -> ExceptionalT Errno IO ()
process out queue nf = do
  mEvents <- lift $ atomically $ tryReadTQueue queue
  case mEvents of
    Just events -> do
                 lift $ putStrLn "foo"
                 writeEventsToPort out nf events
    Nothing -> do
      writeEventsToPort out nf EventList.empty

nOn = MIDI.Channel $ CM.Cons { CM.messageChannel = CM.toChannel 1
                             , CM.messageBody = CM.Voice $ VM.NoteOn (VM.toPitch 60) (VM.toVelocity 60)
                             }
nOff = MIDI.Channel $ CM.Cons { CM.messageChannel = CM.toChannel 1
                              , CM.messageBody = CM.Voice $ VM.NoteOff (VM.toPitch 60) (VM.toVelocity 60)
                              }

eventList :: EL
eventList = EventList.fromPairList [ (NFrames 1, nOn)
                                   ]
eventList' :: EL
eventList' = EventList.fromPairList [ (NFrames 1, nOff)
                                    ]
populateEvents :: TQueue EL -> IO ()
populateEvents events = do
  threadDelay 1000000
  atomically $ do
    writeTQueue events $ eventList
  threadDelay 2500000
  atomically $ do
    writeTQueue events $ eventList'

main :: IO ()
main = do
  putStrLn "Hello, world!"
  handleExceptions $ withClientDefault "jebediah" $ \client -> do
    newPorts <- lift $ newTChanIO
    events <- lift $ newTQueueIO
    _ <- lift $ forkIO $ populateEvents events
    withPortRegistration client (portRegister newPorts) $
      withPort client "midi" $ \out ->
         withProcess client (process out events) $
           withActivation client $ lift $ do
             getPorts client >>= mapM_ (connectTargets targetPorts client out)
             withBreaking $ handleRegistrations targetPorts newPorts client out
