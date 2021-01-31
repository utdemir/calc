{ compiler ? "ghc8103" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
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
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."calc-core"
      p."calc-cli"
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

  exe = pkgs.haskell.lib.justStaticExecutables (myHaskellPackages."calc-cli");
in
{
  inherit shell;
  inherit exe;
  inherit myHaskellPackages;
}
