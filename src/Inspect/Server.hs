{-# LANGUAGE OverloadedStrings #-}

module Inspect.Server where

import Control.Exception (finally, catch, throw)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
import Data.Text (Text, pack, unpack)

import Network.Socket (withSocketsDo)
import Network.WebSockets

address :: String
address = "127.0.0.1"

port :: Int
port = 9160

data ServerState = State { nextId :: Int, clients :: [Client] }

data Client = Client { cId :: Int, cConn :: Connection }

instance Eq Client where
  c1 == c2 = cId c1 == cId c2

initState :: ServerState
initState = State 0 []

addClient :: Connection -> ServerState -> ServerState
addClient conn st =
  let id = nextId st + 1 in State id (Client id conn : clients st)

broadcast :: Text -> ServerState -> IO ()
broadcast graphdata st =
  forM_ (clients st) (\client -> sendTextData (cConn client) graphdata)

serve :: MVar ServerState -> ServerApp
serve st pending =
  do conn <- acceptRequest pending
     msg <- receiveData conn
     case unpack msg of
       "register" ->
         do putStrLn "register"
            register conn st
            keepAlive conn
       "transfer" ->
         do putStrLn "transfer"
            transfer conn st
       str -> putStrLn ("Unknown message: " ++ str)
  where
    register conn st = modifyMVar_ st (return . addClient conn)

transfer :: Connection -> MVar ServerState -> IO ()
transfer conn st =
  do msg <- receiveData conn
     st' <- readMVar st
     broadcast msg st'

keepAlive :: Connection -> IO ()
keepAlive conn = finally (forever (receiveDataMessage conn)) (return ())

close :: Connection -> IO ()
close conn =
  do sendClose conn (pack "closing")
     catch (forever (receiveDataMessage conn)) handler
  where
    handler :: ConnectionException -> IO ()
    handler (CloseRequest _ _) = return ()
    handler err = throw err

defaultMain :: IO ()
defaultMain = withSocketsDo $
  do putStrLn "start server"
     state <- newMVar initState
     runServer address port (serve state)
