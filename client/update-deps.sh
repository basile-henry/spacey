#!/bin/sh

elm2nix convert > elm-srcs.nix
elm2nix snapshot > versions.dat
