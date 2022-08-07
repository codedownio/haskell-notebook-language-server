{
  extras = hackage:
    {
      packages = {
        haskell-notebook-language-server = ./haskell-notebook-language-server.nix;
        lsp-transformer = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-19.15";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }