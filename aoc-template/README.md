# Origin

This template is slightly modified template from this repo:
https://github.com/jonascarpay/template-haskell

## Template for Haskell + Nix projects.

Nix-based Haskell template containing `ghc`, `cabal`, `hoogle`, `ormolu`, and `haskell-language-server`.

### Usage

Clone/copy this repo to the intended package location, and run the wizard.
```bash
git clone https://github.com/jonascarpay/template-haskell <my-project>
cd <my-project>
./wizard.sh
```

You can enter the Nix shell with `nix develop`.
Running `cabal` from inside this shell should use the packages that have been pre-built by Nix.

You can also build your project entirely with nix using `nix build`.
