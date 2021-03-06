{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.JsonRpc2.Base where

import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Attoparsec.ByteString as Streams

import Control.Applicative
import Control.Monad.Except

import Data.Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import Data.Attoparsec.ByteString.Char8 ((<?>))
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Builder as BL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import qualified Data.Text as T

import qualified Data.Map as M

import Network.JsonRpc2.Types

----------------------------------------------------------------------
-- JSON/Content-Length packets

parseJsonPacket :: FromJSON a => Atto.Parser a
parseJsonPacket = do
  len <-
    (Atto.string "Content-Length:" *> Atto.skipSpace <?> "content-length keyword")
    *> (Atto.decimal <?> "content-length argument")
  value <- crlf *> crlf *> (Atto.take len <?> "json") <* Atto.skipSpace
  case eitherDecodeStrict' value of
    Left err -> fail $ "malformed request, " ++ err
    Right v  -> return v
  where
    crlf = (Atto.char '\r' <* Atto.char '\n') <?> "CRLF"

parseJsonPacket' :: FromJSON a => Atto.Parser (Maybe a)
parseJsonPacket' = (Nothing <$ Atto.endOfInput) <|> (Just <$> parseJsonPacket)

writeJsonPacket :: ToJSON a => a -> BL.Builder
writeJsonPacket a =
  let json = encode a
      len  = BL.length json
  in BL.byteString "Content-Length: " <> BL.int64Dec (len + 2) <>
     BL.byteString "\r\n\r\n" <>
     BL.lazyByteString json <>
     BL.byteString "\r\n"


packetStream
  :: (FromJSON a, ToJSON b) =>
     (InputStream a -> IO (InputStream b))
  -> InputStream BS.ByteString
  -> OutputStream BS.ByteString
  -> IO ()
packetStream process inp out = do
  requests <- process =<< Streams.parserToInputStream parseJsonPacket' inp
  responds <- Streams.contramap writeJsonPacket =<< Streams.builderStream out
  Streams.connect requests responds

----------------------------------------------------------------------
-- Handlers

data RequestHandler m =
  forall i o e. (FromJSON i, ToJSON o, ToJSON e) =>
  RequestHandler (i -> ExceptT (RpcError e) m o)

data NotificationHandler m =
  forall i. (FromJSON i) => NotificationHandler (i -> m ())

(~:) :: (FromJSON i, ToJSON o, ToJSON e) => k -> (i -> ExceptT (RpcError e) m o) -> M.Map k (RequestHandler m)
(~:) method handler = M.singleton method (RequestHandler handler)

(^:) :: FromJSON i => k -> (i -> m ()) -> M.Map k (NotificationHandler m)
(^:) method handler = M.singleton method (NotificationHandler handler)

handleRequest :: (Monad m) => M.Map Method (RequestHandler m) -> RequestId -> Method -> Maybe Value -> m (Response Value Value)
handleRequest methods requestId method params =
  case M.lookup method methods of
    Nothing -> return (ResponseError requestId (RpcError RpcMethodNotFound "Method not found" Nothing))
    Just (RequestHandler handler) -> do
      case Aeson.parseEither parseJSON (fromMaybe Null params) of
        Left err -> return (ResponseError requestId (RpcError RpcParseError ("Parse error, " <> T.pack err) Nothing))
        Right input -> do
          mresult <- runExceptT $ handler input
          case mresult of
            Left err -> return (ResponseError requestId (fmap toJSON err))
            Right res -> return (ResponseResult requestId (toJSON res))

handleNotification :: (Monad m) => M.Map Method (NotificationHandler m) -> Method -> Maybe Value -> m ()
handleNotification methods method params =
  case M.lookup method methods of
    Nothing -> return ()
    Just (NotificationHandler handler) -> do
      case Aeson.parseEither parseJSON (fromMaybe Null params) of
        Left _ -> return ()
        Right input -> handler input

handleStream
  :: M.Map Method (RequestHandler IO)
  -> M.Map Method (NotificationHandler IO)
  -> InputStream (Request Value)
  -> IO (InputStream (Response Value Value))
handleStream rmethods nmethods requests = Streams.makeInputStream go
  where
    go :: IO (Maybe (Response Value Value))
    go = do
      rq <- Streams.read requests
      case rq of
        Nothing -> pure Nothing
        Just (Notification{..}) -> handleNotification nmethods reqMethod reqParams >> go
        Just (Request{..}) -> Just <$> handleRequest rmethods reqId reqMethod reqParams
