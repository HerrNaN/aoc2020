{-# LANGUAGE LambdaCase #-}
module Input(
    getInput
) where

import Cookie ( cookie )
import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseBody)
import Data.ByteString.Lazy.UTF8 (ByteString, toString)
import System.Directory ( doesFileExist )

{-|
  Gets the input for the given day by either:
    1. Reading the chached file of the input (or)
    2. Downloading the input from the website and 
       caching it for future runs
-}
getInput :: Int -> IO String
getInput n = doesFileExist file >>= \case 
                True -> readFile file
                _ -> do
                    res <- downloadInput n
                    let input = toString $ getResponseBody res
                    writeFile file input
                    return input
    where file = getFile n

{-|
  Downloads the input for a given day from adventofcode.com
-}
downloadInput :: Int -> IO (Response ByteString)
downloadInput n = do 
    request' <- parseRequest $ getUrl n
    manager <- newManager tlsManagerSettings
    let request = request' { cookieJar = Just $ createCookieJar [cookie] }
    httpLbs request manager

{-|
  Contruct the url from which the input for a given day can
  be fetched.
-}
getUrl :: Int -> String
getUrl n = "https://adventofcode.com/2020/day/" ++ show n ++ "/input"

{-|
  Contruct the filename from which the input for a given 
  day can be fetched.
-}
getFile :: Int -> String
getFile n | n < 10    = "inputs/0" ++ show n ++ ".txt"
          | otherwise = "inputs/" ++ show n ++ ".txt"