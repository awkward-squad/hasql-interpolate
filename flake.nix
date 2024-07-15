{
  description = "hasql-interpolate";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, flake-utils, nixpkgs, flake-compat }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          compiler = "ghc9101";
          pkgs = nixpkgs.legacyPackages."${system}".extend self.overlay;
          ghc = pkgs.haskell.packages."${compiler}";
        in
        {
          apps.repl = flake-utils.lib.mkApp {
            drv = nixpkgs.legacyPackages."${system}".writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
            '';
          };

          devShells.default = ghc.shellFor {
            withHoogle = false;
            packages = hpkgs:
              with hpkgs;
              with pkgs.haskell.lib;
              [
                hasql-interpolate
              ];
            buildInputs = [
              pkgs.cabal-install
            ];
          };

          inherit pkgs;

          packages = { hasql-interpolate = ghc.hasql-interpolate; };

          nixpkgs = pkgs;

          defaultPackage = self.packages."${system}".hasql-interpolate;

          checks = pkgs.lib.attrsets.genAttrs [ "ghc965" "ghc982" "ghc9101" ]
            (ghc-ver: pkgs.haskell.packages."${ghc-ver}".hasql-interpolate);
        }) // {
      overlay = final: prev: {
        haskell = with prev.haskell.lib;
          prev.haskell // {
            packages =
              let
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
                            let
                              bn = baseNameOf path;
                              isHiddenFile = hasPrefix "." bn;
                              isFlakeLock = bn == "flake.lock";
                              isNix = hasSuffix ".nix" bn;
                            in
                            !isHiddenFile && !isFlakeLock && !isNix;
                        in
                        src';
                    in
                    {
                      tmp-postgres =
                        let
                          src = prev.fetchFromGitHub {
                            owner = "jfischoff";
                            repo = "tmp-postgres";
                            rev = "7f2467a6d6d5f6db7eed59919a6773fe006cf22b";
                            hash = "sha256-dE1OQN7I4Lxy6RBdLCvm75Z9D/Hu+9G4ejV2pEtvL1A=";
                          };
                          pkg = self.callCabal2nix "tmp-postgres" src { };
                        in
                        overrideCabal pkg (drv: {
                          libraryToolDepends = drv.libraryToolDepends or [ ] ++ [ final.postgresql ];
                          doCheck = false;
                        });
                      hasql = dontCheck (super.callHackageDirect
                        {
                          pkg = "hasql";
                          ver = "1.8";
                          sha256 = "01kfj0dan0qp46r168mqz3sbsnj09mwbc0zr72jdm32fhi6ck57r";
                        }
                        { });
                      postgresql-binary = dontCheck (super.callHackageDirect
                        {
                          pkg = "postgresql-binary";
                          ver = "0.14";
                          sha256 = "0h3islag95f7rlxzr38ixhv2j9g18gp17jqypk8fax39f9xy3mcm";
                        }
                        { });
                      postgresql-libpq = dontCheck (super.callHackageDirect
                        {
                          pkg = "postgresql-libpq";
                          ver = "0.10.1.0";
                          sha256 = "1zhmph5g1nqwy1x7vc6r6qia6flyzr0cfswgjhi978mw4fl8qwxm";
                        }
                        { });
                      hasql-interpolate =
                        let
                          p = self.callCabal2nix "hasql-interpolate"
                            (cleanSource ./.)
                            { };
                        in
                        overrideCabal p (drv: {
                          testToolDepends = drv.libraryToolDepends or [ ] ++ [ final.postgresql ];
                          # tmp-postgres is failing to initialize a db in the
                          # nix env now, but I haven't had time to figure out
                          # why. Once resolved we can reenable the test suite in
                          # CI.
                          doCheck = false;
                          revision = null;
                          editedCabalFile = null;
                        });
                    });
              in
              prev.haskell.packages // patchedGhcs;
          };
      };
    };
}
