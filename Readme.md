# what is it
This repo has utility to run some simulations in haskell, generate csv, and render it using preconfigured plot scripts.

# examples
```bash
# default simulation with parameters, csv output
nix run .#raw_data 

# study cmdline args
nix run .#raw_data --help

nix run .#raw_data --output "<shell line ready to visualize csv from stdin>"

# some predefined examples wi

# render in your terminal
nix run .#termvis

# simple matplotlib rendering script 
nix run .#pyvis

# generates .html file and tries to open it using 'open' utility. 
# intended to be viewed in your browser
nix run .#rvis


# change some simulation params
nix run .#rvis --ensemble 10 --totalturns 1000 
```
