let pkgs = (import ./.ci/nixpkgs.nix).unstable;
in
  pkgs.mkShell {
    name = "gargoyle";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.postgresql
      pkgs.redis
    ];
    inputsFrom = [
      (import ./release.nix).ghc8107.gargoyle.env
      (import ./release.nix).ghc8107.gargoyle-postgresql.env
      (import ./release.nix).ghc8107.gargoyle-postgresql-connect.env
      (import ./release.nix).ghc8107.gargoyle-postgresql-nix.env
      (import ./release.nix).ghc8107.gargoyle-redis.env
      (import ./release.nix).ghc8107.gargoyle-redis-nix.env
      (import ./release.nix).ghc8107.gargoyle-redis-connect.env
    ];
  }
