{
  extras = hackage:
    {
      packages = {
        haskell-notebook-language-server = ./haskell-notebook-language-server.nix;
        lsp-transformer = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "nightly-2022-03-03";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }