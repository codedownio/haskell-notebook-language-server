# Changelog for haskell-notebook-language-server

## 0.5.8.0

* Transform the call and type hierarchies, whose answers named the shadow document and so couldn't be followed
* Replace a document symbol we can't place with its children, instead of dropping both

## 0.5.7.0

* Recognize the shadow document in an answer however the server escaped its path
* Rewrite the shadow document's name and line inside a hover's own text, which until now kept the shadow document's name

## 0.5.6.0

* Transform workspace/symbol answers, which named the shadow document instead of the notebook

## 0.5.5.0

* Transform textDocument/selectionRange, textDocument/semanticTokens and the goto family, which reached the client in shadow coordinates
* Transform textDocument/rename, prepareRename, references, signatureHelp and declaration, which reached the server naming the notebook

## 0.5.4.1

* Untransform workspace/applyEdit, which named the shadow document and so was refused by the client

## 0.5.4.0

* Support textDocument/inlayHint
* Drop GHC 9.2
* Map code action edits back to the notebook instead of the shadow file
* Transform codeLens/resolve, so lens titles that HLS defers resolve correctly
* Drop code lenses that land on a generated binding, which had no notebook position
