system:

final: prev:

let
  # hadrian's binary-dist-dir step deadlocks under aarch64 user-mode emulation at any
  # parallelism above one: every thread parks in futex_wait and the process makes no
  # progress. Reproduced on qemu-user 10.2.2 and 11.0.1, with -j32 and -j4.
  #
  # cachedDeps goes in passthru so it survives the .override that haskell.nix applies
  # to the compiler; attached from the outside it is dropped there, and the fallback in
  # make-config-files.nix resolves to the compiler itself rather than to its deps.
  serialBindist = ghc:
    let
      self = ghc.overrideAttrs (old: {
        installPhase = builtins.replaceStrings [" -j "] [" -j1 "] old.installPhase;

        passthru = (old.passthru or {}) // {
          cachedDeps = (prev.haskell-nix.haskellLib.makeCompilerDeps self).cachedDeps;
        };
      });

    in self;

in

prev.lib.optionalAttrs (system == "aarch64-linux") {
  haskell-nix = prev.haskell-nix // {
    compiler = prev.haskell-nix.compiler // {
      ghc9141 = serialBindist prev.haskell-nix.compiler.ghc9141;
    };
  };
}
