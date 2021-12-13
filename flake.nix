{
  description = "hasql-interpolate";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.11";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, flake-utils, nixpkgs, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        compiler = "ghc8107";
        pkgs = nixpkgs.legacyPackages."${system}".extend
          self.overlay;
        ghc = pkgs.haskell.packages."${compiler}";
      in {
        apps.repl = flake-utils.lib.mkApp {
          drv = nixpkgs.legacyPackages."${system}".writeShellScriptBin "repl" ''
            confnix=$(mktemp)
            echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
            trap "rm $confnix" EXIT
            nix repl $confnix
          '';
        };

        devShell = ghc.shellFor {
          withHoogle = true;
          buildInputs = [
            pkgs.postgresql_13
          ];
          packages = hpkgs:
            with hpkgs;
            with pkgs.haskell.lib;
            [ hasql-interpolate ];
        };

        packages = { hasql-interpolate = ghc.hasql-interpolate; };
        pkgs = pkgs;

        defaultPackage = self.packages."${system}".hasql-interpolate;
      }) // {
        overlay = final: prev: {
          haskell = with prev.haskell.lib;
            prev.haskell // {
              packages = let
                ghcs = prev.lib.filterAttrs
                  (k: v: prev.lib.strings.hasPrefix "ghc" k)
                  prev.haskell.packages;
                patchedGhcs = builtins.mapAttrs patchGhc ghcs;
                patchGhc = k: v:
                  prev.haskell.packages."${k}".extend (self: super:
                    with prev.haskell.lib;
                    with builtins;
                    with prev.lib.strings;
                    let
                      cleanSource = pth:
                        let
                          src' = prev.lib.cleanSourceWith {
                            filter = filt;
                            src = pth;
                          };
                          filt = path: type:
                            let bn = baseNameOf path;
                                isHiddenFile = hasPrefix "." bn;
                                isFlakeLock = bn == "flake.lock";
                                isNix = hasSuffix ".nix" bn;
                            in !isHiddenFile && !isFlakeLock && !isNix;
                        in src';
                    in {
                      hasql-interpolate = let
                        p = self.callCabal2nix "hasql-interpolate"
                          (cleanSource ./.) { };
                      in p;
                    });
              in prev.haskell.packages // patchedGhcs;
            };
        };
      };
}
