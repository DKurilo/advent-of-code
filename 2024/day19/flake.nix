{
  description = "day19";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              day19 = hfinal.callCabal2nix "day19" ./. { };
            };
        };
        day19 = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.day19;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShell = hspkgs.shellFor {
            withHoogle = true;
            packages = p: [ p.day19 ];
            buildInputs = [
              hspkgs.cabal-install
              hspkgs.haskell-language-server
              hspkgs.hlint
              hspkgs.hindent
              pkgs.bashInteractive
            ];
          };
          defaultPackage = pkgs.day19;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
