{ pkgs, ... }:
{
  enableDefaultExcludes = true;

  projectRootFile = "flake.nix";

  programs.nixfmt.enable = true;
  programs.nixfmt.includes = [ "*.nix" ];

  programs.fourmolu.enable = true;
  programs.fourmolu.includes = [
    "src/**/*.hs"
    "app/**/*.hs"
    "test/**/*.hs"
    "lib/**/*.hs"
  ];

  programs.shfmt.enable = true;

  settings.formatter = {
    "fourmolu" = {
      command = pkgs.haskellPackages.fourmolu;
      options = [
        "--comma-style"
        "leading"
        "--ghc-opt"
        "-XImportQualifiedPost"
        "--ghc-opt"
        "-XTypeApplications"
      ];
    };
  };
}
