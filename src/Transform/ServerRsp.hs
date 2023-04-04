{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Transform.ServerRsp where

import Control.Lens hiding (List)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Aeson as A
import Data.String.Interpolate
import Data.Time
import Language.LSP.Notebook
import Language.LSP.Types
import Language.LSP.Types.Lens as Lens
import Transform.Common
import Transform.ServerRsp.Hover
import Transform.Util


type ServerRspMethod m = SMethod (m :: Method FromClient Request)

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

transformServerRsp' STextDocumentDocumentHighlight initialParams result = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  return $ fmap (untransformRanged tx) result

transformServerRsp' STextDocumentHover initialParams result = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
  case result of
    Nothing -> return Nothing
    Just hov -> do
      logErrorN [i|fixupHoverText tx: #{tx}|]
      logErrorN [i|fixupHoverText before: #{hov}|]
      hov' <- fixupHoverText hov
      logErrorN [i|fixupHoverText after: #{hov'}|]
      logErrorN [i|fixupHoverText after untransformed: #{untransformRangedMaybe tx hov'}|]
      return $ Just $ untransformRangedMaybe tx hov'

transformServerRsp' STextDocumentDocumentSymbol initialParams result = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) ->
  case result of
    InL (List documentSymbols) -> return $ InL $ List (documentSymbols & filter (not . isInternalSymbol)
                                                                       & fmap (over range (untransformRange tx)
                                                                              . over selectionRange (untransformRange tx)))
    InR (List symbolInformations) -> return $ InR $ List (symbolInformations & filter (not . isInternalSymbol)
                                                                             & fmap (over (location . range) (untransformRange tx)))
  where
    isInternalSymbol x = isExpressionVariable expressionToDeclarationParams (x ^. name)

transformServerRsp' STextDocumentCodeAction initialParams result@(List xs) = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {}) -> do
  List <$> filterM (fmap not . isInternalReferringCodeAction) xs
  where
    isInternalReferringCodeAction (InL command) = containsExpressionVariable expressionToDeclarationParams (command ^. title)
    isInternalReferringCodeAction (InR codeAction) = containsExpressionVariable expressionToDeclarationParams (codeAction ^. title)

transformServerRsp' STextDocumentCodeLens initialParams result@(List xs) = whenNotebookByInitialParams initialParams result $ withTransformer result $ \(DocumentState {transformer=tx}) -> do
  return $ List $ fmap (untransformRanged tx) xs

transformServerRsp' _ _ result = return result
