module Main where

import AWS.LARPI


main :: IO ()
main = do
  putStrLn "IT'S WORKING!"
  runLambdaInterface (LambdaInterface {
                          li_handler = (\e -> print e
                                            >> return (Right ("LARPI-LIB" :: String)))
                        , li_init = return Nothing })
                                        

