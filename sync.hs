{-# LANGUAGE OverloadedStrings #-}

import Network
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Data.Aeson
import Data.Text.Encoding as TE
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BS


import Control.Monad

import Data.List
import Data.Char  

import System.Environment
import System.Directory

-- readings_dir = "/srv/git/rust/swmSyncService/readings/"

main = do 
  readings_dir <- getArgs
  print $ head readings_dir
  all <- getDirectoryContents ( head readings_dir )
  let filtered = filter (isSuffixOf "txt") all
  mapM_ send filtered


send :: String -> IO ()
send s = do
  readings_dir <- getArgs
  manager <- newManager defaultManagerSettings
  initReq   <- parseRequest "http://httpbin.org/post"
  let request = initReq { method = "POST" }
  multipart <- (formDataBody [
                  partBS "title" (BS.fromString s),
                  partFileSource "file" ( (head readings_dir) ++ s )
                  ] request)
  response <- httpLbs multipart manager
  let Just obj = decode (responseBody response)
  print (obj :: Object)

