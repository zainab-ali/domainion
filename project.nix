{ project, haskellLib }:
project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = haskellLib.cleanGit {
    name = "domainion";
    src = ./.;
  };
  compiler-nix-name = "ghc883";
}
