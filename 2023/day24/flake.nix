{
  description = "day24";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              day24 = hfinal.callCabal2nix "day24" ./. { };
            };
        };
        day24 = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.day24;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.day24 ];
            buildInputs = [
              hspkgs.Cabal
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.ormolu
              pkgs.z3
              pkgs.bashInteractive
              pkgs.python3Full
              pkgs.python311Packages.z3-solver
            ];
          };
          defaultPackage = pkgs.day24;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
