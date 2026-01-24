{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
	haskl = p: [p.random p.vector p.cmdargs];
        hask = pkgs.ghc.withPackages (haskl);
	haskapp = pkgs.writers.writeHaskellBin "simulate" { libraries = haskl pkgs.haskellPackages;} ./Main.hs;
        py = pkgs.python3.withPackages (p: [p.matplotlib p.pandas]);
        visualizer =
          pkgs.writers.writePython3 "visualizer" {
            libraries = with pkgs.python3Packages; [matplotlib pandas];
          }
	  (pkgs.lib.readFile ./plot.py);
	run = pkgs.writers.writeBash "run" ''
	    ${haskapp}/bin/simulate "$@" | ${visualizer}
	'';
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [hask pkgs.ghcid pkgs.youplot py ];
        };
        apps."vis" = {
          type = "app";
          program = "${visualizer}";
        };
        apps."simulate" = {
          type = "app";
          program = "${haskapp}/bin/simulate";
        };
        apps."main" = {
          type = "app";
          program = "${run}";
        };
	
        formatter = pkgs.alejandra;
      }
    );
}
