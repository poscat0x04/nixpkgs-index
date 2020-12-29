{
  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
  inputs.flake-utils.url = github:poscat0x04/flake-utils;

  outputs = { self, nixpkgs, flake-utils, ... }: with flake-utils;
    eachDefaultSystem (
      system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlay ]; };
        in
          with pkgs;
          {
            devShell = nixpkgs-index-dev.envFunc { withHoogle = true; };
            defaultPackage = nixpkgs-index;
          }
    ) // {
      overlay = self: super:
        let
          hpkgs = super.haskellPackages;
          nixpkgs-index = hpkgs.callCabal2nix "nixpkgs-index" ./. {};
        in
          with super; with haskell.lib;
          {
            inherit nixpkgs-index;
            nixpkgs-index-dev = addBuildTools nixpkgs-index [
              haskell-language-server
              cabal-install
            ];
          };
    };
}
