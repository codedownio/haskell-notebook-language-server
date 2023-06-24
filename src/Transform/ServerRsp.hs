{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.Maybe
import Data.String.Interpolate
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Common
import Transform.ServerRsp.Hover
import Transform.Util


type ServerRspMethod m = SMethod (m :: Method 'FromClient 'Request)

transformServerRsp :: (TransformerMonad n, HasJSON (ResponseMessage m)) => ServerRspMethod m -> MessageParams m -> ResponseMessage m -> n (ResponseMessage m)
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

transformServerRsp' :: (TransformerMonad n) => ServerRspMethod m -> MessageParams m -> ResponseResult m -> n (ResponseResult m)

transformServerRsp' STextDocumentCompletion initialParams result =
  whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
    let fixupCompletionEdit Nothing = Nothing
        fixupCompletionEdit (Just (CompletionEditText edit)) = CompletionEditText <$> (untransformRanged tx edit)
        fixupCompletionEdit (Just (CompletionEditInsertReplace (InsertReplaceEdit newText insert replace))) =
          CompletionEditInsertReplace <$> (InsertReplaceEdit newText <$> untransformRanged tx insert <*> untransformRanged tx replace)

    let fixupItem :: CompletionItem -> CompletionItem
        fixupItem item = item
         & over textEdit fixupCompletionEdit
         & over (additionalTextEdits . _Just) (mapMaybeList (untransformRanged tx))

    let fixupItems :: List CompletionItem -> List CompletionItem
        fixupItems (List xs) = List (fmap fixupItem xs)

    case result of
      (InL items) -> return (InL (fixupItems items))
      (InR completionList) -> return (InR (completionList & over items fixupItems))

transformServerRsp' STextDocumentDocumentHighlight initialParams result@(List inner) = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  return $ List $ mapMaybe (untransformRanged tx) inner

transformServerRsp' STextDocumentHover initialParams result = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
  case result of
    Nothing -> return Nothing
    Just hov -> do
      hov' <- fixupHoverText hov
      return $ untransformRangedMaybe tx hov'

transformServerRsp' STextDocumentDocumentSymbol initialParams result = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
  case result of
    InL (List documentSymbols) -> return $ InL $ List (documentSymbols & filter (not . ignoreSymbol)
                                                                       & mapMaybe (traverseOf range (untransformRange tx))
                                                                       & mapMaybe (traverseOf selectionRange (untransformRange tx)))
    InR (List symbolInformations) -> return $ InR $ List (symbolInformations & filter (not . ignoreSymbol)
                                                                             & mapMaybe (traverseOf (location . range) (untransformRange tx)))
  where
    ignoreSymbol x = isExpressionVariable expressionToDeclarationParams (x ^. name)
                     -- Ignore imports symbol as it doesn't make much sense in notebooks, where imports can appear anywhere.
                     -- Also, it gives away our hidden unsafePerformIO import at the top of the file (for statement handling)
                     || (x ^. name) == "imports"

transformServerRsp' STextDocumentCodeAction initialParams result@(List xs) = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {}) -> do
  List <$> filterM (fmap not . isInternalReferringCodeAction) xs
  where
    isInternalReferringCodeAction (InL command) = containsExpressionVariable expressionToDeclarationParams (command ^. title)
    isInternalReferringCodeAction (InR codeAction) = containsExpressionVariable expressionToDeclarationParams (codeAction ^. title)

transformServerRsp' STextDocumentCodeLens initialParams result@(List xs) = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  return $ List $ mapMaybe (untransformRanged tx) xs

transformServerRsp' _ _ result = return result
