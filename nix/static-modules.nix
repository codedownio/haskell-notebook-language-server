# cxx: the C++ driver GHC links with. The musl cross wrapper that haskell.nix puts on PATH is
# unprefixed on the older pin and prefixed on the newer one, so 9.14 has to pass its own.
#
# linker: gold segfaults in the binutils 2.46 that comes with the newer pin, so 9.14 uses bfd.
{ pkgs
, cxx ? "g++"
, linker ? "gold"
}:

{
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.enableShared = false;
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = [
    ''--ghc-options="-pgml ${cxx} -optl=-fuse-ld=${linker} -optl-Wl,--allow-multiple-definition -optl-Wl,--whole-archive -optl-Wl,-Bstatic -optl-Wl,-Bdynamic -optl-Wl,--no-whole-archive"''
  ];
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.libs = [];
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.build-tools = [pkgs.pkgsCross.musl64.gcc];
}
