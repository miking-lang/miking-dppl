{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    miking.url = "github:elegios/miking/update-nix-packaging-stuff?dir=misc/packaging";
    miking.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, miking, flake-utils }:
    let
      mkPkg = system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.pkgs;
          mpkgs = miking.packages.${system};
        in
          rec {
            packages.miking-dppl-lib = pkgs.callPackage ./miking-dppl-lib.nix {
              inherit (mpkgs) miking-lib;
            };
            packages.miking-dppl-unwrapped = pkgs.callPackage ./miking-dppl-unwrapped.nix {
              inherit (mpkgs) miking-lib miking-unwrapped;
            };
            # packages.miking-dppl = pkgs.callPackage ./miking-dppl.nix {
            #   inherit (mpkgs) miking-lib;
            #   inherit (packages) miking-dppl-lib miking-dppl-unwrapped;
            # };
            devShells.default = pkgs.mkShell {
              name = "Miking dev shell";
              inputsFrom = [ packages.miking-dppl-lib packages.miking-dppl-unwrapped ];
              nativeBuildInputs = [ pkgs.ocamlPackages.owl ];
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
