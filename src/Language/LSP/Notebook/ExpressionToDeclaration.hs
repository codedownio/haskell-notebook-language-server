{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# HLINT ignore "Redundant multi-way if" #-}

module Language.LSP.Notebook.ExpressionToDeclaration where

import Data.Text (Text)
import Data.Vector as V hiding (zip)
import Language.LSP.Transformer
import Language.LSP.Types


newtype ExpressionToDeclaration = ExpressionToDeclaration (Vector Int)
  deriving Show

instance Transformer ExpressionToDeclaration where
  type Params ExpressionToDeclaration = ()

  project :: Params ExpressionToDeclaration -> [Text] -> ([Text], ExpressionToDeclaration)
  project () ls = undefined

  handleDiff :: Params ExpressionToDeclaration -> [Text] -> [Text] -> [TextDocumentContentChangeEvent] -> ExpressionToDeclaration -> ([Text], [Text], [TextDocumentContentChangeEvent], ExpressionToDeclaration)
  handleDiff () before after changes x@(ExpressionToDeclaration indices) = undefined

  transformPosition :: Params ExpressionToDeclaration -> ExpressionToDeclaration -> Position -> Maybe Position
  transformPosition () (ExpressionToDeclaration indices) (Position l c) = undefined

  untransformPosition :: Params ExpressionToDeclaration -> ExpressionToDeclaration -> Position -> Position
  untransformPosition () (ExpressionToDeclaration indices) (Position l c) = undefined
