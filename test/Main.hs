module Main where

import System.Environment
-- import Network.HTTP


-- apiPath = "/2018-06-01/runtime/invocation/next"

-- loop :: String -> IO ()
-- loop api = do res <- simpleHTTP $ getRequest $ "http://" ++ api++apiPath
--               getResponseBody res >>= print
--               loop api

main :: IO ()
main = do putStrLn "IT'S WORKING!"
          -- Just api <- lookupEnv "AWS_LAMBDA_RUNTIME_API"
          -- loop api


          
