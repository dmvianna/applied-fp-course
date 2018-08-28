{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage ./applied-fp-course.nix {};

  devTools = [
    haskellPackages.ghcid
  ];

  shellDrv =
    pkgs.haskell.lib.overrideCabal
      drv
      (drv': {
        buildDepends = (drv'.buildDepends or []) ++
          [ (haskellPackages.hoogleLocal {
              packages =
                (drv'.libraryHaskellDepends or []) ++
                (drv'.executableHaskellDepends or [])++
                (drv'.testHaskellDepends or []);
              })
          ] ++
          devTools;
});

in
  if pkgs.lib.inNixShell then shellDrv.env else drv
