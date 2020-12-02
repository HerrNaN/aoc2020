{-# LANGUAGE LambdaCase #-}
module Input(
    getInput,
    inputAsInts
) where

import Cookie ( cookie )
import Network.HTTP.Conduit
import Network.HTTP.Simple (getResponseBody)
import Data.ByteString.Lazy.UTF8 (ByteString, toString)
import System.Directory ( doesFileExist )


inputAsInts :: String -> [Int]
inputAsInts input = map (read :: String -> Int) $ lines input

getInput :: Int -> IO String
getInput n = doesFileExist file >>= \case 
                True -> readFile file
                _ -> do
                    res <- downloadInput url
                    let input = toString $ getResponseBody res
                    writeFile file input
                    return input
    where file = getFile n
          url = getUrl n

downloadInput :: String -> IO (Response ByteString)
downloadInput url = do 
    request' <- parseRequest url
    manager <- newManager tlsManagerSettings
    let request = request' { cookieJar = Just $ createCookieJar [cookie] }
    httpLbs request manager

getUrl :: Int -> String
getUrl n = "https://adventofcode.com/2020/day/" ++ show n ++ "/input"

getFile :: Int -> String
getFile n | n < 10    = "inputs/0" ++ show n ++ ".txt"
          | otherwise = "inputs/" ++ show n ++ ".txt"