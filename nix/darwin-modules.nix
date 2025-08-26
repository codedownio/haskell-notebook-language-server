{ pkgs }:

{
  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.postInstall = ''
    ${builtins.readFile ./fix-dylib.sh}

    fix_dylib "$out/bin/haskell-notebook-language-server" libiconv.2.dylib libiconv.dylib
    fix_dylib "$out/bin/haskell-notebook-language-server" libffi.8.dylib libffi.dylib
    fix_dylib "$out/bin/haskell-notebook-language-server" libncursesw.6.dylib libncurses.dylib
    check_no_nix_refs "$out/bin/haskell-notebook-language-server"

    strip "$out/bin/haskell-notebook-language-server"
  '';

  packages.haskell-notebook-language-server.components.exes.haskell-notebook-language-server.configureFlags = let
    # Nixpkgs can't currently give us a cross-compiled x86_64-darwin libffi.a when we're building on aarch64-darwin.
    # So, we bundle one in the repo.
    # Tried to also detect if we're on aarch64-darwin, so it can work normally if the build machine is x86_64-darwin,
    # but that is deliberately difficult here (builtins.currentSystem is considered an "impure builtin".)
    libffi = if pkgs.stdenv.targetPlatform.system == "x86_64-darwin"
             then "${../assets/libffi.a}"
             else "${pkgs.pkgsStatic.libffi}/lib/libffi.a";
  in
    [
      ''--ghc-options="-optl-Wl,-dead_strip -optl-Wl,-dead_strip_dylibs -optl-Wl,-force_load,${libffi}"''
    ];
}
