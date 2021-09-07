{
  description = "hasql-interpolate";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  };

  outputs = { self, flake-utils, nixpkgs }:
    let
      compiler = "ghc8104";
      pkgs =
        nixpkgs.legacyPackages.x86_64-linux.extend self.overlays.haskellOverlay;
      ghc = pkgs.haskell.packages."${compiler}";
    in {
      overlays = {
        haskellOverlay = final: prev: {
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
                            let isHiddenFile = hasPrefix "." (baseNameOf path);
                            in !isHiddenFile;
                        in src';
                    in {
                      hasql-interpolate =
                        let p  = self.callCabal2nix "hasql-interpolate" (cleanSource ./src) { };
                        in dontCheck p;
                    });
              in prev.haskell.packages // patchedGhcs;
            };
        };
      };

      apps.x86_64-linux.repl = flake-utils.lib.mkApp {
        drv = nixpkgs.legacyPackages.x86_64-linux.writeShellScriptBin "repl" ''
          confnix=$(mktemp)
          echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
          trap "rm $confnix" EXIT
          nix repl $confnix
        '';
      };

      devShell.x86_64-linux = ghc.shellFor {
        withHoogle = true;
        packages = hpkgs: with hpkgs; with pkgs.haskell.lib; [ (doCheck hasql-interpolate) ];
      };

      packages.x86_64-linux = { };

      defaultPackage.x86_64-linux = self.devShell.x86_64-linux;
    };
}
