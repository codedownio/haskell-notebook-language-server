# Changelog for haskell-notebook-language-server

## 0.5.4.0

* Support textDocument/inlayHint
* Drop GHC 9.2
* Map code action edits back to the notebook instead of the shadow file
* Transform codeLens/resolve, so lens titles that HLS defers resolve correctly
* Drop code lenses that land on a generated binding, which had no notebook position
