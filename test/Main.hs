{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main where

import System.Environment

import Network.HTTP.Req
import Text.URI

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Maybe

import Data.Aeson
import GHC.Generics


-- CONSTANTS
version :: Text
version = "2018-06-01"

invPath :: Url 'Http -> Url 'Http
invPath api = api /~ version /: "runtime" /: "invocation"

errHeader :: Option 'Http
errHeader = header "Lambda-Runtime-Function-Error-Type" "Unhandled"


data Config = Conf { c_base :: Url 'Http
                   , c_opts :: Option 'Http }

data LambdaError =
  LambdaErr { errorMessage :: String
            , errorType :: String }
  deriving (Show, Generic, ToJSON)

data LambdaInvocation =
    LI { li_body :: ByteString
       , li_aws_request_id :: Text
       , li_deadline_ms :: Maybe ByteString
       , li_invoked_function_arn :: Maybe ByteString
       , li_trace_id :: Maybe ByteString
       , li_client_context :: Maybe ByteString
       , li_cognito_identity :: Maybe ByteString}
  deriving (Show, Eq)


invocationResponse :: Config -> Text -> ByteString -> IO ()
invocationResponse Conf{..} req_id resp =
  runReq defaultHttpConfig $ do
    req POST
        (invPath c_base /: req_id /: "response" )
        (ReqBodyBs resp)
        ignoreResponse
        c_opts
    return ()


invocationError :: Config -> Text -> LambdaError -> IO ()
invocationError Conf{..} req_id lerr =
  runReq defaultHttpConfig $ do
    res <- req POST
               (invPath c_base /: req_id /: "error" )
               (ReqBodyJson lerr)
               ignoreResponse
               (c_opts <> errHeader)
    return ()

initializationError :: Config -> LambdaError -> IO ()
initializationError Conf{..} lerr =
  runReq defaultHttpConfig $ do
    res <- req POST
               (c_base /~ version /: "runtime" /: "init" /: "error" )
               (ReqBodyJson lerr)
               ignoreResponse
               (c_opts <> errHeader)
    return ()


nextInvocation :: Config -> IO LambdaInvocation
nextInvocation Conf{..} = runReq defaultHttpConfig $ do
  res <- req GET
             (invPath c_base /: "next")
             NoReqBody
             bsResponse
             (c_opts <> responseTimeout maxBound)
  let rH = responseHeader res

  return $ LI {
      li_body = responseBody res
    , li_aws_request_id = fromJust $
                            T.decodeUtf8 <$> (rH "Lambda-Runtime-Aws-Request-Id")
    , li_deadline_ms = rH "Lambda-Runtime-Deadline-Ms"
    , li_invoked_function_arn = rH "Lambda-Runtime-Invoked-Function-Arn"
    , li_trace_id = rH "Lambda-Runtime-Trace-Id"
    , li_client_context = rH "Lambda-Runtime-Client-Context"
    , li_cognito_identity = rH "Lambda-Runtime-Cognito-Identity" }



data LambdaInterface =
  LambdaInterface { li_handler :: LambdaInvocation
                               -> IO (Either LambdaError ByteString)
                  , li_init :: IO (Maybe LambdaError) }

runLambdaInterface :: LambdaInterface -> IO ()
runLambdaInterface (LambdaInterface {..}) =
    do Just api <- fmap T.pack <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"
       Just (c_base, c_opts) <- useHttpURI <$> mkURI ("http://" <> api)
       let conf = Conf{..}
       i_res <- li_init
       case i_res of
          Just err -> initializationError conf err
          _ -> loop conf li_handler
  where loop conf handler  = do
          li@LI{..} <- nextInvocation conf
          res <- handler li
          case res of
             Left lerr -> invocationError conf li_aws_request_id lerr
             Right resp -> invocationResponse conf li_aws_request_id resp
          loop conf handler

main :: IO ()
main = do
  putStrLn "IT'S WORKING!"
  runLambdaInterface (LambdaInterface {
                          li_handler = const (return $ Right "SUCCESS")
                        , li_init = return Nothing })
                                        

