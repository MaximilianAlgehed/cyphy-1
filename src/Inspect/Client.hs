module Inspect.Client where


--import Data.Text (Text, pack, unpack, null)
--import qualified Data.Text.IO as T

import Network.Socket (withSocketsDo)
import Network.WebSockets
       ( ByteString
       , ClientApp
       , runClient
       , sendTextData
       , toLazyByteString
       )

import Data.Aeson (ToJSON, encode)
--import Data.ByteString.Lazy (encode)
--import Data.ByteString.Char8 (ByteString, pack)

import Inspect.Server
import Inspect.Graph

inspect :: Graphable a => a -> IO ()
inspect g = do
  let encodedg = toLazyByteString (encode g)
  withSocketsDo (runClient address port "/" (sendData encodedg))

sendData ::  a -> ClientApp ()
sendData g conn =
  do sendTextData conn (toLazyByteString "transfer")
     sendTextData conn g
     close conn
