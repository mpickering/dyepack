let
  pin = import ((import ./nix/sources.nix).nixpkgs) {} ;
  # Import the Haskell.nix library,
  haskell = import ((import ./nix/sources.nix)."haskell.nix") { pkgs = pin; };

  pkgPlan = haskell.callCabalProjectToNix
              { index-state = "2019-05-10T00:00:00Z"
              ; src = pin.lib.cleanSource ./.;};

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import pkgPlan;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs.dyepack.components.all
