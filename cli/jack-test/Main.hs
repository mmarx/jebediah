module Main (main) where

import Control.Concurrent ( forkIO
                          , threadDelay
                          )
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Data.Monoid ((<>))
import System.Posix.Signals ( installHandler
                            , keyboardSignal
                            , Handler (Catch)
                            )

import Sound.JACK

portRegister :: TChan PortId -> PortId -> Bool -> IO ()
portRegister newPorts pId new = do
  putStrLn $ n <> ": " <> (show pId)
  atomically $ writeTChan newPorts pId
  putStrLn $ "!" <> ": " <> (show pId)
  where n = if new then "+" else "-"

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

handleRegistrations :: TChan PortId -> Client -> IO ()
handleRegistrations newPorts client = do
  pId <- atomically $ readTChan newPorts
  port <- portById client pId
  pn <- portName port
  as <- portAliases port
  putStrLn $ "`" <> pn <> "' " <> show as

main :: IO ()
main = do
  putStrLn "Hello, world!"
  handleExceptions $ withClientDefault "jebediah" $ \client -> do
    newPorts <- lift $ newTChanIO
    withPortRegistration client (portRegister newPorts) $
      withActivation client $ lift $ do
        getPorts client >>= mapM_ (\pn -> do
                                     port <- portByName client pn
                                     as <- portAliases port
                                     putStrLn $ "`" <> pn <> "' " <> show as)
        withBreaking $ handleRegistrations newPorts client
