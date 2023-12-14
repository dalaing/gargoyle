{ haskellPackages
, pkgs ? haskellPackages.callPackage ({pkgs}: pkgs) {}
, postgresql ? pkgs.postgresql
, redis ? pkgs.redis
, ...
}: {
  gargoyle = haskellPackages.callCabal2nix "gargoyle" ./gargoyle {};
  gargoyle-postgresql = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-postgresql" ./gargoyle-postgresql {})
    (drv: {
      testSystemDepends = (drv.testSystemDepends or []) ++ [ (if postgresql == null then pkgs.postgresql else postgresql) ];
    });

  gargoyle-postgresql-nix = haskellPackages.callCabal2nix "gargoyle-postgresql-nix" ./gargoyle-postgresql-nix {
    # TODO: libpq will become standalone in https://github.com/NixOS/nixpkgs/pull/359659
    libpq = postgresql;
  };

  gargoyle-postgresql-connect = haskellPackages.callCabal2nix "gargoyle-postgresql-connect" ./gargoyle-postgresql-connect {};
  gargoyle-redis = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-redis" ./gargoyle-redis {})
    (drv: {
      testSystemDepends = (drv.testSystemDepends or []) ++ [ (if redis == null then pkgs.redis else redis) ];
    });
  gargoyle-redis-nix  = pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "gargoyle-redis-nix" ./gargoyle-redis-nix {})
    (drv: {
      librarySystemDepends = (drv.librarySystemDepends or []) ++ [ (if redis == null then pkgs.redis else redis) ];
    });
  gargoyle-redis-connect = haskellPackages.callCabal2nix "gargoyle-redis-connect" ./gargoyle-redis-connect {};
}
