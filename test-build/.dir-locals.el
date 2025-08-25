(
 (haskell-mode
  .
  (
   (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans"
                                       "--stack-yaml" "stack-9.2.8.yaml"
                                       "--no-build"
                                       "--no-load"
                                       "haskell-notebook-language-server:test:test-build"
                                       )))
  )
 )
