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
  };

  outputs = inputs:
    with builtins;
    inputs.utils.lib.eachDefaultSystem
      (system:
        let
          p = inputs.nixpkgs.legacyPackages.${system};
          lu-pkgs = inputs.lint-utils.packages.${system};
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

          inherit (inputs.shelpers.lib p) eval-shelpers;
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
                    optimized-bundle = {
                      description = "Bundle the app with purs-backend-es";
                      script = "purs-backend-es bundle-app --int-tags -y -t main.js";
                    };
                    deploy = {
                      description = "deploy the app to GitHub Pages";
                      script = ''
                        git checkout -B pages
                        purs-backend-es bundle-app --int-tags -y -t main.js
                        git add -f main.js
                        git commit -m deploy
                        git push -f
                        git checkout -
                        purs-nix bundle
                      '';
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
