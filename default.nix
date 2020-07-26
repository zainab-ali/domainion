let
  # Fetch the latest haskell.nix and import it's default.nix
  haskellNix = import (fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/4219882774edb7a5e8382c3c6c421f2e907ee262.tar.gz") {};
  # Get the nixpkgs and patch using haskellNix
  pkgs = import <nixpkgs> haskellNix.nixpkgsArgs;
in with pkgs.haskell-nix; import ./project.nix {
  inherit project;
  inherit haskellLib;
}
