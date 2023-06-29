{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import Data.Time
import Language.LSP.Protocol.Lens as Lens
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types as LSP
import Transform.Common
import Transform.ServerRsp.Hover
import Transform.Util
import UnliftIO.Concurrent


type ServerRspMethod m = SMethod (m :: Method 'ClientToServer 'Request)

transformServerRsp :: (TransformerMonad n, HasJSON (TResponseMessage m)) => ServerRspMethod m -> MessageParams m -> TResponseMessage m -> n (TResponseMessage m)
transformServerRsp meth initialParams msg = do
  case msg ^. result of
    Left _err -> return msg
    Right ret -> do
      start <- liftIO getCurrentTime
      p' <- transformServerRsp' meth initialParams ret
      stop <- liftIO getCurrentTime
      let msg' = set result (Right p') msg
      when (msg' /= msg) $ logDebugN [i|Transforming server rsp #{meth} in #{diffUTCTime stop start}: (#{A.encode msg} --> #{A.encode msg'})|]
      return $ set result (Right p') msg

transformServerRsp' :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> MessageResult m -> n (MessageResult m)

transformServerRsp' SMethod_Initialize _initialParams result = do
  initializeResultVar <- asks transformerInitializeResult
  modifyMVar_ initializeResultVar (\_ -> return $ Just result)
  return result

transformServerRsp' SMethod_TextDocumentCompletion initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    let fixupCompletionEdit Nothing = Nothing
        fixupCompletionEdit (Just (InL edit)) = InL <$> (untransformRanged tx edit)
        fixupCompletionEdit (Just (InR (InsertReplaceEdit newText insert replace))) =
          InR <$> (InsertReplaceEdit newText <$> untransformRanged tx insert <*> untransformRanged tx replace)

    let fixupItem :: CompletionItem -> CompletionItem
        fixupItem item = item
         & over textEdit fixupCompletionEdit
         & over (additionalTextEdits . _Just) (mapMaybe (untransformRanged tx))

    case result of
      (InL items) -> return (InL (fmap fixupItem items))
      (InR (InL completionList)) -> return (InR (InL (completionList & over items (fmap fixupItem))))
      (InR (InR null)) -> return $ InR $ InR null


transformServerRsp' SMethod_TextDocumentDocumentHighlight initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL highlights -> return $ InL $ mapMaybe (untransformRanged tx) highlights
      InR null -> return $ InR null

transformServerRsp' SMethod_TextDocumentHover initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InR null -> return $ InR null
      InL hov -> do
        hov' <- fixupHoverText hov
        return $ case untransformRangedMaybe tx hov' of
          Just ret -> InL ret
          Nothing -> InR LSP.Null

transformServerRsp' SMethod_TextDocumentDocumentSymbol initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL symbolInformations -> return $ InL (symbolInformations & filter (not . ignoreSymbol)
                                                                 & mapMaybe (traverseOf (location . range) (untransformRange tx)))
      InR (InL documentSymbols) -> return $ InR $ InL $ (documentSymbols & filter (not . ignoreSymbol)
                                                                         & mapMaybe (traverseOf range (untransformRange tx))
                                                                         & mapMaybe (traverseOf selectionRange (untransformRange tx)))
      InR (InR LSP.Null) -> return $ InR $ InR LSP.Null
  where
    ignoreSymbol _ = False

transformServerRsp' SMethod_TextDocumentCodeAction _initialParams (InR null) = return $ InR null
transformServerRsp' SMethod_TextDocumentCodeAction initialParams (InL result) = (InL <$>) $
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {}) -> do
    filterM (fmap not . isInternalReferringCodeAction) result
  where
    isInternalReferringCodeAction (InL _command) = pure False
    isInternalReferringCodeAction (InR _codeAction) = pure False

transformServerRsp' SMethod_TextDocumentCodeLens _initialParams (InR null) = return $ InR null
transformServerRsp' SMethod_TextDocumentCodeLens initialParams (InL result) = (InL <$>) $
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    return $ mapMaybe (untransformRanged tx) result

transformServerRsp' _ _ result = return result
