{
  description = "day06";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              day06 = hfinal.callCabal2nix "day06" ./. { };
            };
        };
        day06 = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.day06;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.day06 ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.fourmolu
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.day06;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
