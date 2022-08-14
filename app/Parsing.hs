{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Parsing where

import Data.Aeson
import Data.Aeson.Types
import Data.Function (on)
import Data.Type.Equality
import Language.LSP.Types hiding (FromServerMessage'(..), FromServerMessage, FromClientMessage'(..), FromClientMessage, LookupFunc)


data FromServerMessage where
  FromServerReq :: forall t (m :: Method FromServer Request). (ToJSON (RequestMessage m)) => SMethod m -> RequestMessage m -> FromServerMessage
  FromServerNot :: forall t (m :: Method FromServer Notification). (ToJSON (NotificationMessage m)) => SMethod m -> NotificationMessage m -> FromServerMessage
  FromServerRsp  :: forall (m :: Method FromClient Request). (ToJSON (ResponseMessage m)) => SMethod m -> MessageParams m -> ResponseMessage m -> FromServerMessage

data FromClientMessage where
  FromClientReq :: forall t (m :: Method FromClient Request). (ToJSON (RequestMessage m)) => SMethod m -> RequestMessage m -> FromClientMessage
  FromClientNot :: forall t (m :: Method FromClient Notification). (ToJSON (NotificationMessage m)) => SMethod m -> NotificationMessage m -> FromClientMessage
  FromClientRsp  :: forall (m :: Method FromServer Request). (ToJSON (ResponseMessage m)) => SMethod m -> ResponseMessage m -> FromClientMessage

type LookupFunc f = forall (m :: Method f 'Request). LspId m -> Maybe (SMethod m, MessageParams m)

{-# INLINE parseServerMessage #-}
parseServerMessage :: LookupFunc FromClient -> Value -> Parser FromServerMessage
parseServerMessage lookupId v@(Object o) = do
  methMaybe <- o .:! "method"
  idMaybe <- o .:! "id"
  case methMaybe of
    -- Request or Notification
    Just (SomeServerMethod m) ->
      case splitServerMethod m of
        IsServerNot -> FromServerNot m <$> parseJSON v
        IsServerReq -> FromServerReq m <$> parseJSON v
        IsServerEither | SCustomMethod cm <- m -> do
          case idMaybe of
            -- Request
            Just _ ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromServer Request))
              in FromServerReq m' <$> parseJSON v
            Nothing ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromServer Notification))
              in FromServerNot m' <$> parseJSON v
    Nothing -> do
      case idMaybe of
        Just i -> do
          case lookupId i of
            Just (m, initialParams) -> clientResponseJSON m $ FromServerRsp m initialParams <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseServerMessage _ v = fail $ unwords ["parseServerMessage expected object, got:",show v]

{-# INLINE parseClientMessage #-}
parseClientMessage :: LookupFunc FromServer -> Value -> Parser FromClientMessage
parseClientMessage lookupId v@(Object o) = do
  methMaybe <- o .:! "method"
  idMaybe <- o .:! "id"
  case methMaybe of
    -- Request or Notification
    Just (SomeClientMethod m) ->
      case splitClientMethod m of
        IsClientNot -> FromClientNot m <$> parseJSON v
        IsClientReq -> FromClientReq m <$> parseJSON v
        IsClientEither | SCustomMethod cm <- m -> do
          case idMaybe of
            -- Request
            Just _ ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromClient Request))
              in FromClientReq m' <$> parseJSON v
            Nothing ->
              let m' = (SCustomMethod cm :: SMethod (CustomMethod :: Method FromClient Notification))
              in FromClientNot m' <$> parseJSON v
    Nothing -> do
      case idMaybe of
        Just i -> do
          case lookupId i of
            Just (m, initialParams) -> serverResponseJSON m $ FromClientRsp m <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseClientMessage _ v = fail $ unwords ["parseClientMessage expected object, got:",show v]
