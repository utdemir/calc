let
  sources = import ../nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  # Beware, this is Profpatsch/yarn2nix, not the common one.
  # yarn2nix = import sources.yarn2nix {};
  yarn2nix = import ../../yarn2nix {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  npmDepsNix = pkgs.runCommand "calc-js-npm-deps.nix" { buildInputs = [yarn2nix]; }
    "yarn2nix ${./yarn.lock} > $out";
  npmPackageNix = pkgs.runCommand "calc-js-npm-package.nix" { buildInputs = [yarn2nix]; }
    "yarn2nix --template ${./package.json} > $out";

  nodeModules =
    let deps = yarn2nix.nixLib.buildNodeDeps (pkgs.callPackage npmDepsNix {});
        pkg = yarn2nix.nixLib.callTemplate npmPackageNix deps;
    in  yarn2nix.nixLib.linkNodeDeps {
          name = "calc-web-node_modules";
          dependencies = pkg.nodeBuildInputs;
        };

  calc-web = pkgs.stdenv.mkDerivation {
    name = "calc-web";
    buildInputs = [ pkgs.yarn ];
    src = gitignore ./.;
    buildPhase = ''
      export HOME=$(mktemp -d)
      ln -s ${nodeModules} node_modules
      yarn --offline build
    '';
  };
in
{
  inherit calc-web;
  shell = pkgs.mkShell {
    name = "calc-web-shell";
    buildInputs = [
      pkgs.yarn
    ];
  };
}
