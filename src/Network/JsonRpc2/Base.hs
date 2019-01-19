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

import Control.Monad.Except

import Data.Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
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
    Atto.string "Content-Length:" *> Atto.skipSpace
    *> Atto.decimal
    <* Atto.char '\n' <* Atto.char '\r'
  value <-
    Atto.char '\n' *> Atto.char '\r' *> Atto.take len
  case eitherDecodeStrict' value of
    Left err -> fail $ "malformed request, " ++ err
    Right v  -> return v

writeJsonPacket :: ToJSON a => a -> BL.Builder
writeJsonPacket a =
  let json = encode a
      len  = BL.length json
  in BL.byteString "Content-Length: " <> BL.int64Dec len <>
     BL.byteString "\n\r\n\r" <>
     BL.lazyByteString json

packetStream :: (FromJSON a, ToJSON b) => (InputStream a -> IO (InputStream b)) -> InputStream BS.ByteString -> OutputStream BS.ByteString -> IO ()
packetStream process inp out = do
  requests <- process =<< Streams.parserToInputStream (Just <$> parseJsonPacket) inp
  responds <- Streams.contramap writeJsonPacket =<< Streams.builderStream out
  Streams.connect requests responds

----------------------------------------------------------------------
-- Handlers

data RequestHandler m =
  forall i o e. (FromJSON i, ToJSON o, ToJSON e) =>
  RequestHandler (i -> ExceptT (RpcError e) m o)

data NotificationHandler m =
  forall i. (FromJSON i) => NotificationHandler (i -> m ())

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
