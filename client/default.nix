{ nixpkgs ? import ../nixpkgs.nix
}:

with nixpkgs;

let
  uglifyjs = "${nodePackages.uglify-js}/bin/uglifyjs";

  mkDerivation =
    { srcs ? ./elm-srcs.nix
    , src
    , name
    , srcdir ? "./src"
    , targets ? []
    , versionsDat ? ./versions.dat
    }:
    stdenv.mkDerivation {
      inherit name src;

      buildInputs = [ elmPackages.elm ];

      buildPhase = pkgs.elmPackages.fetchElmDeps {
        elmPackages = import srcs;
        inherit versionsDat;
      };

      installPhase = let
        elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      in ''
        mkdir $out
        cp -r site/ $out/
        cp -r res/ $out/res/
        ${lib.concatStrings (map (module: ''
          echo "compiling ${elmfile module}"
          elm make ${elmfile module} --optimize --output tmp.js

          ${uglifyjs} tmp.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' |
            ${uglifyjs} --mangle --output=$out/site/elm.js
        '') targets)}
      '';
    };
in mkDerivation {
  name = "elm-app-0.1.0";
  srcs = ./elm-srcs.nix;
  src = ./.;
  targets = ["Main"];
  srcdir = "./src";
}

