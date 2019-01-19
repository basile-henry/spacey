let
  nixpkgs = import ./nixpkgs.nix;

  server = import ./server/default.nix { inherit nixpkgs; };
  client = import ./client/default.nix { inherit nixpkgs; };

  run-spacey =
    nixpkgs.pkgs.writeScript "run-spacey.sh"
      "cd ${client} && ${server}/bin/server";

in run-spacey
