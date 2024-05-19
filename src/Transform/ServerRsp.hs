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
import Language.LSP.Notebook (expressionToDeclarationParams)
import Language.LSP.Notebook.ExpressionToDeclaration
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

transformServerRsp' SMethod_TextDocumentCompletion initialParams result = do
  AppConfig {..} <- asks transformerConfig
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    let fixupCompletionEdit Nothing = Nothing
        fixupCompletionEdit (Just (InL edit)) = InL <$> (untransformRanged appConfigDynFlags tx edit)
        fixupCompletionEdit (Just (InR (InsertReplaceEdit newText insert replace))) =
          InR <$> (InsertReplaceEdit newText <$> untransformRanged appConfigDynFlags tx insert <*> untransformRanged appConfigDynFlags tx replace)

    let fixupItem :: CompletionItem -> CompletionItem
        fixupItem item = item
         & over textEdit fixupCompletionEdit
         & over (additionalTextEdits . _Just) (mapMaybe (untransformRanged appConfigDynFlags tx))

    case result of
      (InL items) -> return (InL (fmap fixupItem items))
      (InR (InL completionList)) -> return (InR (InL (completionList & over items (fmap fixupItem))))
      (InR (InR null)) -> return $ InR $ InR null


transformServerRsp' SMethod_TextDocumentDocumentHighlight initialParams result = do
  AppConfig {..} <- asks transformerConfig
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL highlights -> return $ InL $ mapMaybe (untransformRanged appConfigDynFlags tx) highlights
      InR null -> return $ InR null

transformServerRsp' SMethod_TextDocumentHover initialParams result = do
  AppConfig {..} <- asks transformerConfig
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InR null -> return $ InR null
      InL hov -> do
        hov' <- fixupHoverText hov
        return $ case untransformRangedMaybe appConfigDynFlags tx hov' of
          Just ret -> InL ret
          Nothing -> InR LSP.Null

transformServerRsp' SMethod_TextDocumentDocumentSymbol initialParams result = do
  AppConfig {..} <- asks transformerConfig
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
    case result of
      InL symbolInformations -> return $ InL (symbolInformations & filter (not . ignoreSymbol appConfigDynFlags)
                                                                 & mapMaybe (traverseOf (location . range) (untransformRange appConfigDynFlags tx)))
      InR (InL documentSymbols) -> return $ InR $ InL (documentSymbols & filter (not . ignoreSymbol appConfigDynFlags)
                                                                       & mapMaybe (traverseOf range (untransformRange appConfigDynFlags tx))
                                                                       & mapMaybe (traverseOf selectionRange (untransformRange appConfigDynFlags tx)))
      InR (InR LSP.Null) -> return $ InR $ InR LSP.Null
  where
    ignoreSymbol ghcLibPath x = isExpressionVariable (expressionToDeclarationParams ghcLibPath) (x ^. name)
                                -- Ignore imports symbol as it doesn't make much sense in notebooks, where imports can appear anywhere.
                                -- Also, it gives away our hidden unsafePerformIO import at the top of the file (for statement handling)
                                || (x ^. name) == "imports"


transformServerRsp' SMethod_TextDocumentCodeAction _initialParams (InR null) = return $ InR null
transformServerRsp' SMethod_TextDocumentCodeAction initialParams (InL result) = (InL <$>) $
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {}) -> do
    filterM (fmap not . isInternalReferringCodeAction) result
  where
    isInternalReferringCodeAction (InL _command) = pure False
    isInternalReferringCodeAction (InR _codeAction) = pure False

transformServerRsp' SMethod_TextDocumentCodeLens _initialParams (InR null) = return $ InR null
transformServerRsp' SMethod_TextDocumentCodeLens initialParams (InL result) = (InL <$>) $ do
  AppConfig {..} <- asks transformerConfig
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    return $ mapMaybe (untransformRanged appConfigDynFlags tx) result

transformServerRsp' _ _ result = return result
