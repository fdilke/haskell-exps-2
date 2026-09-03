{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-2605";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        overlays = [
          haskellNix.overlay
          (final: _prev: {
            hixProject = final.haskell-nix.hix.project {
              src = ./.;
              evalSystem = "x86_64-linux";
              name = "haskell-exps";
              compiler-nix-name = "ghc9124";
              shell.tools.cabal = "latest";
              shell.withHoogle = false;
              # shell.tools.haskell-language-server = "latest";
              shell.tools.haskell-language-server = {
                 src = builtins.fetchGit {
                 url = "https://github.com/haskell/haskell-language-server.git";
                 rev = "1b4b3c6bdd2bf8d1e1182e2e770f5dea9198db80";
                };
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        haskell = pkgs.hixProject.flake { };
      in
      {
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            haskell.devShells.default
          ];
          packages = with pkgs; [
            ghcid
          ];
        };
        packages.default = haskell.packages."haskell-exps:exe:haskell-exps";
      }
    );
}

