{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status (..), hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType (..), Error (..),
                                           RqType (..), mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to Level02.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse (Status status _) ct bs =
  case status of
    200 -> resp200 ct bs
    404 -> resp404 ct bs
    _   -> resp400 ct bs

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 ct =
  responseLBS status200 [(hContentType, renderContentType ct)]

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 ct =
  responseLBS status404 [(hContentType, renderContentType ct)]

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 ct =
  responseLBS status400 [(hContentType, renderContentType ct)]


-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest topic comment =
  case (mkTopic topic
       ,mkCommentText $ lazyByteStringToStrictText comment) of
    (Right t, Right c) -> Right $ AddRq t c
    (Right _, Left e)  -> Left e
    (Left e, _)        -> Left e
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText =
      decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest topic =
  case mkTopic topic of
    Left e  -> Left e
    Right t -> Right $ ViewRq t

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse e =
  case e of
    EmptyTopic   -> resp400 PlainText "Empty Topic"
    EmptyComment -> resp400 PlainText "Empty Comment"
    BadRequest   -> resp400 PlainText "Bad Request"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest r =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (requestMethod r
       ,pathInfo r) of
    ("POST", ["", _]) ->
      pure $ Left $ EmptyTopic
    ("GET", ["", _]) ->
      pure $ Left $ EmptyTopic
    ("POST", [t, "add"]) -> do
      comment <- strictRequestBody r
      if comment == mempty
        then pure $ Left $ EmptyComment
        else pure $ mkAddRequest t comment
    ("GET", [t, "view"]) -> do
      pure $ mkViewRequest t
    ("GET", ["list"]) -> do
      pure $ mkListRequest
    _ -> pure $ Left $ BadRequest

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest req =
  case req of
    AddRq _ _ -> resp "add request not implemented"
    ViewRq _  -> resp "view request not implemented"
    ListRq    -> resp "list request not implemented"
  where
    resp :: LBS.ByteString -> Either Error Response
    resp x = Right $ resp200 PlainText x

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
-- Type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app
  :: Application
app req respond = do
  r <- mkRequest req
  case r of
    Right rq ->
      case handleRequest rq of
        Right r' -> respond r'
        Left e   -> respond $ mkErrorResponse e
    Left e   -> respond $ mkErrorResponse e

runApp :: IO ()
runApp = run 3000 app
