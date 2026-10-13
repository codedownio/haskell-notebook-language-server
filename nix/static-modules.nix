{ pkgs }:

{
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.enableShared = false;
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = [
    ''--ghc-options="-pgml g++ -optl=-fuse-ld=gold -optl-Wl,--allow-multiple-definition -optl-Wl,--whole-archive -optl-Wl,-Bstatic -optl-Wl,-Bdynamic -optl-Wl,--no-whole-archive"''
  ];
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.libs = [];
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.build-tools = [pkgs.pkgsCross.musl64.gcc];
}
