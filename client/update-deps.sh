#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash elm2nix

elm2nix convert > elm-srcs.nix
elm2nix snapshot > versions.dat
