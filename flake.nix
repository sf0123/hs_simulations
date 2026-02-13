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
        haskl = p: [p.random p.vector p.cmdargs p.diagrams p.random-fu];
        hask = pkgs.ghc.withPackages haskl;
        add_rpackages = x: x.override {packages = with pkgs.rPackages; [ggplot2 plotly pandoc tidyverse];};

        # r with r.pandoc package can not work, if 'pandoc' not available on system.
        # So we add pandoc package into runtime
        myR_pandoc = pkgs.symlinkJoin {
          name = "r-with-pandoc";
          paths = [(add_rpackages pkgs.rWrapper)];
          buildInputs = [pkgs.makeWrapper];
          postBuild = ''
            wrapProgram $out/bin/R \
              --prefix PATH : ${pkgs.lib.makeBinPath [pkgs.pandoc]}
          '';
        };
        myR_utils = add_rpackages pkgs.rstudioWrapper;
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
        crossopen =
          if pkgs.system == "aarch64-darwin"
          then "open"
          else "${pkgs.xdg-utils}/bin/xdg-open";
        rvis = pkgs.writers.writeBash "rvis" ''
          ${haskapp}/bin/simulate --output "${myR_pandoc}/bin/Rscript ${./r_visualizer.r}" "$@"
          ${crossopen} csv_plot.html
        '';
        termvis = pkgs.writers.writeBash "termvis" ''
          ${haskapp}/bin/simulate --output "${pkgs.youplot}/bin/uplot lines -d," "$@"
        '';
        termvis_histogram = pkgs.writers.writeBash "termvis_histogram" ''
          ${haskapp}/bin/simulate --output "${pkgs.gawk}/bin/awk '{print \''$2}' | ${pkgs.youplot}/bin/uplot hist --nbins 100" "$@"
        '';
        rvis_histogram = pkgs.writers.writeBash "rvis_histogram" ''
          ${haskapp}/bin/simulate --output "${myR_pandoc}/bin/Rscript ${./probability_density.r}" "$@"
          ${crossopen} densities.html
        '';
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [hask pkgs.ghcid pkgs.youplot py pkgs.ormolu myR_utils myR_pandoc pkgs.haskell-language-server ];
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
        apps."termvis_histogram" = {
          type = "app";
          program = "${termvis_histogram}";
        };
        apps."rvis" = {
          type = "app";
          program = "${rvis}";
        };
        apps."rvis_histogram" = {
          type = "app";
          program = "${rvis_histogram}";
        };

        formatter = pkgs.alejandra;
      }
    );
}
