{
  description = "Pitboss: A Blackjack Toolkit and Simulator";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.treefmt-nix.url = "github:numtide/treefmt-nix";

  outputs =
    {
      self,
      nixpkgs,
      treefmt-nix,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];
      eachSystem =
        f:
        nixpkgs.lib.genAttrs systems (
          system:
          let
            pkgs = import nixpkgs {
              inherit system;
              config.allowUnfree = true;
            };
          in
          f pkgs
        );
      treefmtEval = eachSystem (pkgs: treefmt-nix.lib.evalModule pkgs ./treefmt.nix);
    in
    {
      formatter = eachSystem (pkgs: treefmtEval.${pkgs.system}.config.build.wrapper);
      # for `nix flake check`

      checks = eachSystem (pkgs: {
        formatting = treefmtEval.${pkgs.system}.config.build.check self;
      });

      devShells = eachSystem (
        pkgs:
        let
          inherit (pkgs) cabal2nix treefmt;
          inherit (pkgs.haskellPackages)
            ghc
            cabal-fmt
            cabal-install
            ormolu
            haskell-language-server
            ;

        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              cabal2nix
              treefmt

              # from haskellPackages
              ghc
              cabal-fmt
              cabal-install
              ormolu
              haskell-language-server
            ];
          };
        }
      );

      packages = eachSystem (pkgs: {
        default = pkgs.haskellPackages.callCabal2nix "pitboss" ./. { }; # no overrides needed unless you have C bindings
      });
    };
}
