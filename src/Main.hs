{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network (listenOn, PortID(PortNumber))
import Network.Socket (accept, withSocketsDo, close, Socket)
import Network.Socket.ByteString (recv, sendAll)
import Control.Concurrent (forkIO)
import Data.List.Split
import Data.Map.Lazy ()
import qualified Data.ByteString.Lazy.Char8 as LazyByte (toChunks)
import qualified Data.ByteString as  ByteString (concat)
import qualified Data.ByteString.Char8 as Char8 (unpack)
import qualified Data.Text.Lazy as LazyText (Text, length, pack, concat)
import Data.Text.Lazy.Encoding (encodeUtf8)

main = withSocketsDo $ do
  sock <- listenOn (PortNumber 1919)
  loop sock

loop :: Socket-> IO()
loop sock = do
    (conn, _) <- accept sock
    putStrLn "new Connection"
    forkIO $ respond conn
    loop sock
  where
    respond conn = do
      msg <- recv conn 1024
      request <- getRequest msg
      print request
      let resp = okResp "hello world!"
      sendAll conn (ByteString.concat . LazyByte.toChunks $ toByteString resp)
      close conn
    getRequest msg = do
       let charMsg = Char8.unpack msg
       let requestSplit = splitOn "\r\n" charMsg

       let mainString = requestSplit!!0
       let mainSplit = splitOn " " mainString
       let method = mainSplit!!0
       let path = mainSplit!!1
       let httpVersion = mainSplit!!2

       let requestWithoutMain = tail requestSplit
       let requestTrim = init requestWithoutMain
       let requestTrim2 = init requestTrim
       let headers = map (splitOn ": ") requestTrim2

       let request = HttpRequest{ method = method, path = path, httpVersion = httpVersion, headers = headers}

       return request


data Request = HttpRequest{
    method :: [Char],
    path :: [Char],
    httpVersion :: [Char],
    headers :: [[[Char]]]
} deriving( Show, Eq )

data Response = TextResponse {
  version :: LazyText.Text,
  status :: Int,
  reason :: LazyText.Text,
  text :: LazyText.Text
} deriving (Show, Eq)

okResp text = TextResponse {
  version = "1.1", status = 200, reason = "Ok", text = text
}

notFoundResp text reason = TextResponse {
  version = "1.1", status = 404, reason = reason, text = text
}

toByteString (TextResponse {
  version = ver,
  status = stat,
  reason = reason,
  text = text
}) = encodeUtf8 . LazyText.concat $ [
    "HTTP/", ver, LazyText.pack . show $ stat, reason, "\r\n",
    "Content-Length: ", LazyText.pack . show $ LazyText.length text, "\r\n",
    "\r\n",
    text]