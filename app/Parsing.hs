{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parsing where

import Data.Aeson
import Data.Aeson.Types
import Data.Function (on)
import Data.Type.Equality
import Language.LSP.Types hiding (FromServerMessage'(..), FromServerMessage, FromClientMessage'(..), FromClientMessage)


data FromServerMessage' a where
  FromServerReq :: forall t (m :: Method FromServer Request) a. (ToJSON (RequestMessage m)) => SMethod m -> RequestMessage m -> FromServerMessage' a
  FromServerNot :: forall t (m :: Method FromServer Notification) a. (ToJSON (NotificationMessage m)) => SMethod m -> NotificationMessage m -> FromServerMessage' a
  FromServerRsp  :: forall (m :: Method FromClient Request) a. (ToJSON (ResponseMessage m)) => a m -> ResponseMessage m -> FromServerMessage' a

type FromServerMessage = FromServerMessage' SMethod

data FromClientMessage' a where
  FromClientReq :: forall t (m :: Method FromClient Request) a. (ToJSON (RequestMessage m)) => SMethod m -> RequestMessage m -> FromClientMessage' a
  FromClientNot :: forall t (m :: Method FromClient Notification) a. (ToJSON (NotificationMessage m)) => SMethod m -> NotificationMessage m -> FromClientMessage' a
  FromClientRsp  :: forall (m :: Method FromServer Request) a. (ToJSON (ResponseMessage m)) => a m -> ResponseMessage m -> FromClientMessage' a

type FromClientMessage = FromClientMessage' SMethod

{-# INLINE parseServerMessage #-}
parseServerMessage :: LookupFunc FromClient a -> Value -> Parser (FromServerMessage' a)
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
            Just (m,res) -> clientResponseJSON m $ FromServerRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseServerMessage _ v = fail $ unwords ["parseServerMessage expected object, got:",show v]

{-# INLINE parseClientMessage #-}
parseClientMessage :: LookupFunc FromServer a -> Value -> Parser (FromClientMessage' a)
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
            Just (m,res) -> serverResponseJSON m $ FromClientRsp res <$> parseJSON v
            Nothing -> fail $ unwords ["Failed in looking up response type of", show v]
        Nothing -> fail $ unwords ["Got unexpected message without method or id"]
parseClientMessage _ v = fail $ unwords ["parseClientMessage expected object, got:",show v]
