{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Parsing where

import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Language.LSP.Protocol.Message hiding (LookupFunc)


data ServerToClientMessage where
  ServerToClientReq :: forall (m :: Method ServerToClient Request). (HasJSON (TRequestMessage m)) => SMethod m -> TRequestMessage m -> ServerToClientMessage
  ServerToClientNot :: forall (m :: Method ServerToClient Notification). (HasJSON (TNotificationMessage m)) => SMethod m -> TNotificationMessage m -> ServerToClientMessage
  ServerToClientRsp  :: forall (m :: Method ClientToServer Request). (HasJSON (TResponseMessage m)) => SMethod m -> MessageParams m -> TResponseMessage m -> ServerToClientMessage

data ClientToServerMessage where
  ClientToServerReq :: forall (m :: Method ClientToServer Request). (HasJSON (TRequestMessage m)) => SMethod m -> TRequestMessage m -> ClientToServerMessage
  ClientToServerNot :: forall (m :: Method ClientToServer Notification). (HasJSON (TNotificationMessage m)) => SMethod m -> TNotificationMessage m -> ClientToServerMessage
  ClientToServerRsp  :: forall (m :: Method ServerToClient Request). (HasJSON (TResponseMessage m)) => SMethod m -> TResponseMessage m -> ClientToServerMessage

type LookupFunc f = forall (m :: Method f 'Request). LspId m -> Maybe (SMethod m, MessageParams m)

{-# INLINE parseServerMessage #-}
parseServerMessage :: LookupFunc ClientToServer -> Value -> Parser ServerToClientMessage
parseServerMessage lookupId v@(Object o) = do
  methMaybe <- o .:! "method"
  idMaybe <- o .:! "id"
  case methMaybe of
    -- Request or Notification
    Just (SomeServerMethod m) ->
      case splitServerMethod m of
        IsServerNot -> ServerToClientNot m <$> parseJSON v
        IsServerReq -> ServerToClientReq m <$> parseJSON v
        IsServerEither | SMethod_CustomMethod (p :: Proxy s') <- m -> do
          case idMaybe of
            -- Request
            Just _ ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ServerToClient Request))
              in ServerToClientReq m' <$> parseJSON v
            Nothing ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ServerToClient Notification))
              in ServerToClientNot m' <$> parseJSON v
    Nothing -> do
      case idMaybe of
        Just i -> do
          case lookupId i of
            Just (m, initialParams) -> clientResponseJSON m $ ServerToClientRsp m initialParams <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseServerMessage _ v = fail $ unwords ["parseServerMessage expected object, got:",show v]

{-# INLINE parseClientMessage #-}
parseClientMessage :: LookupFunc ServerToClient -> Value -> Parser ClientToServerMessage
parseClientMessage lookupId v@(Object o) = do
  methMaybe <- o .:! "method"
  idMaybe <- o .:! "id"
  case methMaybe of
    -- Request or Notification
    Just (SomeClientMethod m) ->
      case splitClientMethod m of
        IsClientNot -> ClientToServerNot m <$> parseJSON v
        IsClientReq -> ClientToServerReq m <$> parseJSON v
        IsClientEither | SMethod_CustomMethod (p :: Proxy s') <- m -> do
          case idMaybe of
            -- Request
            Just _ ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ClientToServer Request))
              in ClientToServerReq m' <$> parseJSON v
            Nothing ->
              let m' = (SMethod_CustomMethod p :: SMethod (Method_CustomMethod s' :: Method ClientToServer Notification))
              in ClientToServerNot m' <$> parseJSON v
    Nothing -> do
      case idMaybe of
        Just i -> do
          case lookupId i of
            Just (m, _initialParams) -> serverResponseJSON m $ ClientToServerRsp m <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseClientMessage _ v = fail $ unwords ["parseClientMessage expected object, got:",show v]
