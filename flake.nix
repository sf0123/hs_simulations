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
          ''
            import sys
            import pandas as pd
            import matplotlib.pyplot as plt

            # Read the data
            df = pd.read_csv(sys.argv[1])

            # Basic plot with all lines
            plt.figure(figsize=(10, 6))
            plt.ylim(0, 10)           # Fixed range from 0 to 10

            for column in df.columns[1:]:  # Skip 'turns' column
                plt.plot(df['turns'], df[column], label=column, marker='o')

            plt.xlabel('Turns')
            plt.ylabel('Values')
            plt.title('Multiple Lines Comparison')
            plt.legend()
            plt.grid(True, alpha=0.3)
            plt.tight_layout()
            plt.show()
          '';
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [hask pkgs.ghcid pkgs.youplot py];
        };
        apps."vis" = {
          type = "app";
          program = "${visualizer}";
        };
        apps."simulate" = {
          type = "app";
          program = "${haskapp}/bin/simulate";
        };
        formatter = pkgs.alejandra;
      }
    );
}
