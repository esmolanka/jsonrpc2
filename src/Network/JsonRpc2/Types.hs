{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

{-# OPTIONS_GHC -Wall #-}

module Network.JsonRpc2.Types where

import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable
import Data.Maybe
import Data.String
import Data.Text (Text)

data RequestId
  = StringId Text
  | IntId Int
  deriving (Eq, Ord, Show)

newtype Method = Method Text
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString)

data Request r
  = Request
    { reqId      :: RequestId
    , reqMethod  :: Method
    , reqParams  :: Maybe r
    }
  | Notification
    { reqMethod  :: Method
    , reqParams  :: Maybe r
    } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data ErrorCode
  = RpcParseError      -- -32700
  | RpcInvalidRequest  -- -32600
  | RpcMethodNotFound  -- -32601
  | RpcInvalidParams   -- -32602
  | RpcInternalError   -- -32603
  | RpcServerError Int -- -32000..-32099
  deriving (Eq, Ord, Show)

data RpcError e = RpcError
  { errorCode    :: ErrorCode
  , errorMessage :: Text
  , errorData    :: Maybe e
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

data Response e r
  = ResponseResult {rspId :: RequestId, rspResult :: r}
  | ResponseError  {rspId :: RequestId, rspError  :: RpcError e}
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

----------------------------------------------------------------------
-- Traversing

instance Bifunctor Response where
  bimap f g = \case
    ResponseResult{..} -> ResponseResult {rspId = rspId, rspResult = g rspResult}
    ResponseError{..}  -> ResponseError {rspId = rspId, rspError = fmap f rspError}

instance Bifoldable Response where
  bifoldr f g x0 = \case
    ResponseResult{..} -> rspResult `g` x0
    ResponseError{..}  -> foldr f x0 rspError

instance Bitraversable Response where
  bitraverse f g = \case
    ResponseResult{..} -> ResponseResult <$> pure rspId <*> g rspResult
    ResponseError{..}  -> ResponseError <$> pure rspId <*> traverse f rspError

----------------------------------------------------------------------
-- JSON representation

instance ToJSON RequestId where
  toJSON = \case
    StringId s -> toJSON s
    IntId n -> toJSON n

instance FromJSON RequestId where
  parseJSON v =
    (IntId <$> parseJSON v) <|>
    (StringId <$> parseJSON v)

instance ToJSON r => ToJSON (Request r) where
  toJSON = \case
    Request{..} -> object $ catMaybes
      [ Just $ "jsonrpc" .= toJSON ("2.0" :: Text)
      , Just $ "id"      .= toJSON reqId
      , Just $ "method"  .= toJSON reqMethod
      , ("params" .=) <$> toJSON <$> reqParams
      ]
    Notification{..} -> object $ catMaybes
      [ Just $ "jsonrpc" .= toJSON ("2.0" :: Text)
      , Just $ "method"  .= toJSON reqMethod
      , ("params" .=) <$> toJSON <$> reqParams
      ]

instance FromJSON r => FromJSON (Request r) where
  parseJSON = withObject "JSON-RPC request" $ \obj -> do
    ver <- obj .: "jsonrpc"
    unless (ver == ("2.0" :: Text)) $
      fail "unsupported JSON-RPC version"
    mid <- obj .:? "id"
    meth <- obj .: "method"
    params <- obj .:? "params"
    pure $ case mid of
      Just rid -> Request rid meth params
      Nothing -> Notification meth params

instance ToJSON ErrorCode where
  toJSON = \case
    RpcParseError     -> toJSON (-32700 :: Int)
    RpcInvalidRequest -> toJSON (-32600 :: Int)
    RpcMethodNotFound -> toJSON (-32601 :: Int)
    RpcInvalidParams  -> toJSON (-32602 :: Int)
    RpcInternalError  -> toJSON (-32603 :: Int)
    RpcServerError n  -> toJSON n

instance FromJSON ErrorCode where
  parseJSON v =
    parseJSON v >>= \case
      (-32700) -> pure RpcParseError
      (-32600) -> pure RpcInvalidRequest
      (-32601) -> pure RpcMethodNotFound
      (-32602) -> pure RpcInvalidParams
      (-32603) -> pure RpcInternalError
      other    -> if other > (-32100) && other <= (-32000)
                  then pure (RpcServerError other)
                  else fail "invalid error code"

instance ToJSON e => ToJSON (RpcError e) where
  toJSON RpcError{..} = object $ catMaybes
    [ Just $ "code"    .= toJSON errorCode
    , Just $ "message" .= toJSON errorMessage
    , ("data" .=) <$> toJSON <$> errorData
    ]

instance FromJSON e => FromJSON (RpcError e) where
  parseJSON = withObject "rpc error" $ \obj ->
    RpcError
      <$> obj .:  "code"
      <*> obj .:  "message"
      <*> obj .:? "data"

instance (ToJSON e, ToJSON r) => ToJSON (Response e r) where
  toJSON = \case
    ResponseResult{..} -> object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id"      .= rspId
      , "result"  .= toJSON rspResult
      ]
    ResponseError{..} -> object
      [ "jsonrpc" .= ("2.0" :: Text)
      , "id"      .= rspId
      , "error"   .= toJSON rspError
      ]

instance (FromJSON e, FromJSON r) => FromJSON (Response e r) where
  parseJSON = withObject "JSON-RPC response" $ \obj -> do
    ver <- obj .: "jsonrpc"
    unless (ver == ("2.0" :: Text)) $
      fail "unsupported JSON-RPC version"
    rid <- obj .: "id"
    msum
      [ ResponseResult <$> pure rid <*> obj .: "result"
      , ResponseError  <$> pure rid <*> obj .: "error"
      , fail "unknown JSON-RPC response type"
      ]
