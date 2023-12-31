{
  description = "day24";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      perSystem = system:
        let
          overlay = final: prev: {
            haskell = prev.haskell // {
              packageOverrides = hfinal: hprev:
                prev.haskell.packageOverrides hfinal hprev // {
                  day24 = hfinal.callCabal2nix "day24" ./. { z3 = z3haskell; };
                };
            };
            day24 = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.day24;
          };
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
          z3pkgs = import ./z3.nix {
            lib = pkgs.lib;
            stdenv = pkgs.stdenv;
            fetchFromGitHub = pkgs.fetchFromGitHub;
            python = pkgs.python3Full;
            fixDarwinDylibNames = pkgs.fixDarwinDylibNames;
            pythonBindings = true;
            writeScript = pkgs.writeScript;
          };
          z3haskell = import ./haskell-z3.nix {
            pkgs = pkgs;
            z3 = z3pkgs.z3;
          };
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.day24 { haskell-z3 = z3haskell; } ];
            buildInputs = [
              hspkgs.Cabal
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              z3pkgs.z3
              z3haskell
              pkgs.bashInteractive
              pkgs.python3Full
            ];
          };
          defaultPackage = pkgs.day24;
        };
    in
    { } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
