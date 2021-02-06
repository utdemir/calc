let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  commonOverrides = hself: hsuper: {
    "calc-core" =
      hself.callCabal2nix
        "calc-core"
        (gitignore ./calc-core)
        { };
    "calc-cli" =
      hself.callCabal2nix
        "calc-cli"
        (gitignore ./calc-cli)
        { };
    "calc-js" =
      hself.callCabal2nix
        "calc-js"
        (gitignore ./calc-js)
        { };

    # others
    "ghcjs-base" =
      hself.ghcjs-base-stub;
  };

  # native GHC
  nativeHaskellOverrides = {
    overrides = commonOverrides;
  };
  nativeHaskellPackages = pkgs.haskell.packages.ghc8103.override nativeHaskellOverrides;

  shell = nativeHaskellPackages.shellFor {
    packages = p: [
      p."calc-core"
      p."calc-cli"
      p."calc-js"
    ];
    buildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = false;
  };

  calc-cli = pkgs.haskell.lib.justStaticExecutables (nativeHaskellPackages."calc-cli");

  # GHCJS
  pkgsGhcjs = import sources.nixpkgs-ghcjs { };
  ghcjsHaskellOverrides = {
    overrides = hself: hsuper:
      commonOverrides hself hsuper // {
        "ghcjs-base" = pkgs.haskell.lib.appendPatch
          hsuper.ghcjs-base
          (pkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/051c81c4f1b1f3af3b4b8d09d6538fc8e632e1b1.patch";
            sha256 = "sha256-ycGIkdKXFDTHR4sCRNBKSvvEM1IhylR3mys6JxPnUr8=";
          });

        # don't run tests on ghcjs since quickcheck is hard to compile.
        mkDerivation = expr:
          hsuper.mkDerivation (expr // { doCheck = false; });
      };
  };
  jsHaskellPackages = pkgsGhcjs.haskell.packages.ghcjs.override ghcjsHaskellOverrides;
  calc-js-orig = jsHaskellPackages.calc-js;
  calc-js = pkgs.runCommand "calc-js-optimised" { buildInputs = [ pkgs.closurecompiler ]; } ''
    mkdir "$out"
    set -o xtrace
    closure-compiler \
      --js ${calc-js-orig}/bin/calc-js.jsexe/all.js \
      --js_output_file "$out/calc.js"
      # --externs ${calc-js-orig}/bin/calc-js.jsexe/all.js.externs \
      # --compilation_level advanced \
      # --jscomp_off="*" \
  '';
in
{
  inherit shell;
  inherit calc-cli;
  inherit calc-js;
}
