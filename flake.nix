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
        hask = pkgs.ghc.withPackages haskl;
        myR = pkgs.rWrapper.override {packages = with pkgs.rPackages; [ggplot2 plotly pandoc];};
        haskapp = pkgs.writers.writeHaskellBin "simulate" {libraries = haskl pkgs.haskellPackages;} ./Main.hs;
        py = pkgs.python3.withPackages (p: [p.matplotlib p.pandas]);

        # python code bundled with dependencies to create app to visualize from stdin
        visualizer =
          pkgs.writers.writePython3 "visualizer" {
            libraries = with pkgs.python3Packages; [matplotlib pandas];
          }
          (pkgs.lib.readFile ./plot.py);
        # convenience - pass by default to python visualizer
        pyvis = pkgs.writers.writeBash "pyvis" ''
          ${haskapp}/bin/simulate --output ${visualizer} "$@"
        '';
        rvis = pkgs.writers.writeBash "rvis" ''
          ${haskapp}/bin/simulate --output "${myR}/bin/Rscript ${./r_visualizer.r}" "$@"
          open interactive_csv_plot.html
        '';
        termvis = pkgs.writers.writeBash "termvis" ''
          ${haskapp}/bin/simulate --output "${pkgs.youplot}/bin/uplot lines -d," "$@"
        '';
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [hask pkgs.ghcid pkgs.youplot py pkgs.ormolu myR pkgs.pandoc];
        };
        apps."raw_data" = {
          type = "app";
          program = "${haskapp}/bin/simulate";
        };
        apps."pyvis" = {
          type = "app";
          program = "${pyvis}";
        };
        apps."termvis" = {
          type = "app";
          program = "${termvis}";
        };
        apps."rvis" = {
          type = "app";
          program = "${rvis}";
        };

        formatter = pkgs.alejandra;
      }
    );
}
