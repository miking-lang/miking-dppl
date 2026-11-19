{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    relocatable.url = "github:elegios/relocatable.nix";
    flake-utils.url = "github:numtide/flake-utils";
    miking.url = "github:miking-lang/miking?dir=misc/packaging";
    miking.inputs.nixpkgs.follows = "nixpkgs";
    mi-stats.url = "github:miking-lang/mi-stats?dir=misc/packaging";
    mi-stats.inputs.nixpkgs.follows = "nixpkgs";
    mi-stats.inputs.miking.follows = "miking";
  };

  outputs = { self, nixpkgs, miking, mi-stats, flake-utils, relocatable }:
    let
      mkPkg = system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.pkgs;
          mpkgs = miking.packages.${system};
          mspkgs = mi-stats.packages.${system};
        in
          rec {
            packages.miking-dppl-lib = pkgs.callPackage ./miking-dppl-lib.nix {
              inherit (mpkgs) miking-lib;
              inherit (mspkgs) mi-stats;
            };
            packages.miking-dppl-unwrapped = pkgs.callPackage ./miking-dppl-unwrapped.nix {
              inherit (mpkgs) miking-lib miking-unwrapped;
              inherit (mspkgs) mi-stats;
            };
            packages.cppl-tmp-tar-gz = relocatable.bundlers.${system}.fixedLocationTarGz {
              drv = packages.miking-dppl-unwrapped;
              path = "/tmp/cppl/";
              extraSetup = storePath: ''
                # Include this too ${packages.miking-dppl-lib}/lib
                for site in ${storePath}*/lib/ocaml/*/site-lib; do
                  OCAMLPATH="$site''${OCAMLPATH:+:''${OCAMLPATH}}"
                done
                export OCAMLPATH
                for mcore in ${storePath}*/lib/mcore/*; do
                  MCORE_LIBS="$(basename "$mcore")=$mcore''${MCORE_LIBS:+:''${MCORE_LIBS}}"
                done
                export MCORE_LIBS
              '';
              runtimeInputs = [
                pkgs.ocamlPackages.findlib
                pkgs.ocamlPackages.owl
                pkgs.stdenv.cc
              ];
            };
            # packages.miking-dppl = pkgs.callPackage ./miking-dppl.nix {
            #   inherit (mpkgs) miking-lib;
            #   inherit (packages) miking-dppl-lib miking-dppl-unwrapped;
            # };
            devShells.default = pkgs.mkShell {
              name = "Miking dev shell";
              inputsFrom = [ packages.miking-dppl-lib packages.miking-dppl-unwrapped ];
              nativeBuildInputs = [ pkgs.ocamlPackages.owl pkgs.gdb pkgs.tup ];
            };
          };
    in flake-utils.lib.eachDefaultSystem mkPkg // rec {
      overlays.miking = final: prev: {
        # miking-dppl = final.callPackage ./miking-dppl.nix {};
        miking-dppl-lib = final.callPackage ./miking-dppl-lib.nix {};
        miking-dppl-unwrapped = final.callPackage ./miking-dppl-unwrapped.nix {};
      };
      overlays.default = overlays.miking;
    };
}
