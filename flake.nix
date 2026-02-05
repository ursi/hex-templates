{
  inputs = {
    lint-utils = {
      url = "github:homotopic/lint-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ps-tools.follows = "purs-nix/ps-tools";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    shelpers.url = "gitlab:platonic/shelpers";
    utils.url = "github:numtide/flake-utils";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ lint-utils, ... }:
      with builtins;
      inputs.utils.lib.eachDefaultSystem
        (system:
        let
          p = import inputs.nixpkgs {
            inherit system;
            overlays = [ inputs.purescript-overlay.overlays.default ];
          };

          l = p.lib;

          fs = l.fileset;
          onlyExts = exts: path: fs.toSource {
            root = path;
            fileset = fs.fileFilter (f: foldl' l.or false (map f.hasExt exts)) path;
          };

          lu = lint-utils.linters.${system};
          lu-pkgs = lint-utils.packages.${system};

          ps-tools = inputs.ps-tools.legacyPackages.${system};
          purs-nix =
            inputs.purs-nix {
              inherit system;
              overlays = [ (import ./ps-overlay.nix p) ];
            };

          ps = purs-nix.purs {
            dependencies = [
              "deku-core"
              "deku-dom"
              "js-timers"
              "nonempty"
              "string-parsers"
              "ursi.prelude"
            ];

            dir = ./.;
          };

          inherit (inputs.shelpers.lib p) eval-shelpers shelp;
          shelpers =
            eval-shelpers [
              ({ shelp, ... }: {
                shelpers."." = {
                  General = {
                    inherit shelp;
                    watch = {
                      description = "Watch for changes and bundle the purescript code";
                      script = "find src | entr -s 'echo bundling; purs-nix bundle'";
                    };
                  };
                };
              })
            ];
        in
        {
          packages.default = ps.bundle { };

          devShells.default = p.mkShell {
            packages = with p; [
              entr
              nodejs
              (ps.command {
                compile.codegen = "corefn,docs,js";
                bundle.esbuild.format = "iife";
              })
              (ps.command {
                name = "dce";
                output = "dce-output";
                compile.codegen = "corefn,js";
                bundle.esbuild.format = "iife";
              })
              ps-tools.purescript-language-server
              ps-tools.purescript-backend-optimizer
              ps-tools.purs-tidy
              purs-nix.esbuild
              purs-nix.purescript
            ];

            shellHook = ''
              ${shelpers.functions}
              shelp
            '';
          };
          inherit (shelpers) apps;

          formatter = lu-pkgs.nixpkgs-fmt;
          shelpers = shelpers.files;
        });
}
