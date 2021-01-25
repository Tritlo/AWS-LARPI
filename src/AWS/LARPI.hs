{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE RankNTypes, ExistentialQuantification #-}

-- | The AWS.LARPI library defines the interface and the associated data types
--   for running a Haskell program directly on AWS Lambda, and handles most of
--   associated overheads, leaving the user free to focus on the functionality
--   itself.
module AWS.LARPI (
    runLambdaInterface,

    LambdaInterface(..), LambdaInvocation(..), LambdaError(..)

) where


import System.Environment

import Network.HTTP.Req
import Text.URI

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Maybe

import Data.Aeson
import GHC.Generics

import Control.Monad (when)


-- CONSTANTS
version :: Text
version = "2018-06-01"

invPath :: Url 'Http -> Url 'Http
invPath api = api /~ version /: "runtime" /: "invocation"

errHeader :: Option 'Http
errHeader = header "Lambda-Runtime-Function-Error-Type" "Unhandled"


data Config = Conf { c_base :: Url 'Http
                   , c_opts :: Option 'Http }



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

  return $ LambdaInvocation {
      li_body = responseBody res
    , li_aws_request_id = fromJust $
                            T.decodeUtf8 <$> (rH "Lambda-Runtime-Aws-Request-Id")
    , li_deadline_ms = rH "Lambda-Runtime-Deadline-Ms"
    , li_invoked_function_arn = rH "Lambda-Runtime-Invoked-Function-Arn"
    , li_trace_id = rH "Lambda-Runtime-Trace-Id"
    , li_client_context = rH "Lambda-Runtime-Client-Context"
    , li_cognito_identity = rH "Lambda-Runtime-Cognito-Identity" }

--- EXPORTS


-- | A Lambda error consists of a message and an error type, both of which
--   the user can define.
data LambdaError =
  LambdaErr { errorMessage :: String
            , errorType :: String }
  deriving (Show, Generic, ToJSON)

-- | A Lambda invocaton contains the body of the request and a few headers
--   defined by AWS. Only the AWS-Request-Id header is guaranteed to be defined.
--   The headers are the following:
--
-- * li_aws_request_id – The request ID, which identifies the request that triggered
-- the function invocation.
--
--     For example, 8476a536-e9f4-11e8-9739-2dfe598c3fcd.
--
-- * li_deadline_ms – The date that the function times out in Unix time milliseconds.
--
--     For example, 1542409706888.
--
-- * li_invoked_function_arn – The ARN of the Lambda function, version, or alias
-- that's specified in the invocation.
--
--     For example, arn:aws:lambda:us-east-2:123456789012:function:custom-runtime.
--
-- * li_trace_id – The AWS X-Ray tracing
--  header.
--
--     For example, Root=1-5bef4de7-ad49b0e87f6ef6c87fc2e700;Parent=9a9197af755a6419;Sampled=1.
--
-- * li_client_context – For invocations from the AWS Mobile SDK, data about the
-- client application and device.
--
-- * li_cognito_identity – For invocations from the AWS Mobile SDK, data about the
-- Amazon Cognito identity provider.
data LambdaInvocation =
    LambdaInvocation {
         li_body :: ByteString
       , li_aws_request_id :: Text
       , li_deadline_ms :: Maybe ByteString
       , li_invoked_function_arn :: Maybe ByteString
       , li_trace_id :: Maybe ByteString
       , li_client_context :: Maybe ByteString
       , li_cognito_identity :: Maybe ByteString}
  deriving (Show, Eq)


-- | A Lambda interface consists of two functions:
--
--   The li_handler is the function that is run on every Lambda Invocation
--   the results will be dumped as JSON, as this is what most AWS APIs
--   expect. The first argument is the result of the li_init function, such
--   as a database connection or an IORef or similar. Existentially
--   quantified in the same manner as the ST monad to avoid the state
--   escaping.
--
--   The li_init function is called at the start of the program to initialize
--   the Lambda function, and some optional state. The state is then passed
--   to the handler at each invocation, e.g. a database connection or similar.
data LambdaInterface a = forall s.
  LambdaInterface {
      li_handler :: ToJSON a => s
                             -> LambdaInvocation
                             -> IO (Either LambdaError a)
    , li_init :: IO  (Either LambdaError s)
    }

-- | The runLambdaInteface should be provided with the definition of the
--   interface to run, and handles the interaction with the AWS API, such as the
--   waiting for next request, responding to the right request, setting the
--   X-Ray trace id and initializing the function at the start.
runLambdaInterface :: ToJSON a => LambdaInterface a -> IO ()
runLambdaInterface (LambdaInterface {..}) =
    do Just api <- fmap T.pack <$> lookupEnv "AWS_LAMBDA_RUNTIME_API"
       Just (c_base, c_opts) <- useHttpURI <$> mkURI ("http://" <> api)
       let conf = Conf{..}
       i_res <- li_init
       case i_res of
          Left err -> initializationError conf err
          Right init_state -> loop conf init_state li_handler
  where loop conf init_state handler  = do
          li@LambdaInvocation{..} <- nextInvocation conf
          when (isJust li_trace_id) $
            setEnv "_X_AMZN_TRACE_ID" $ B.unpack $ fromJust li_trace_id
          res <- handler init_state li
          case res of
             Left lerr -> invocationError conf li_aws_request_id lerr
             Right resp -> invocationResponse conf li_aws_request_id
                             (BL.toStrict $ encode resp)
          loop conf init_state handler

