# shell.nix
let
  pkgs = import <nixpkgs> {};
  hsPkgs = import ./default.nix;
  fmt = with pkgs; writeScriptBin "fmt"
  ''
    #!${stdenv.shell}
    ormolu --mode inplace "$@"
  '';
in hsPkgs.shellFor {
  # Some common tools can be added with the `tools` argument
  withHoogle = true;
  tools = {
    cabal = "3.2.0.0";
    ormolu = "0.1.2.0";
    hsinspect = "0.0.13"; # FIXME: This doesn't seem to work properly
    hpack = "0.34.2";
  };
  buildInputs = [ fmt ];
}
