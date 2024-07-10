{
  description = "hasql-interpolate";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    tmp-postgres-src = {
      url = "github:jfischoff/tmp-postgres";
      flake = false;
    };
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
    flake-compat,
    tmp-postgres-src,
  }:
    flake-utils.lib.eachDefaultSystem
    (system: let
      compiler = "ghc963";
      pkgs = nixpkgs.legacyPackages."${system}".extend self.overlay;
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

      devShells.default = ghc.shellFor {
        withHoogle = false;
        packages = hpkgs:
          with hpkgs;
          with pkgs.haskell.lib; [
            hasql-interpolate
          ];
        buildInputs = [
          pkgs.cabal-install
        ];
      };

      packages = {hasql-interpolate = ghc.hasql-interpolate;};

      nixpkgs = pkgs;

      defaultPackage = self.packages."${system}".hasql-interpolate;

      checks =
        pkgs.lib.attrsets.genAttrs ["ghc928" "ghc947" "ghc963"]
        (ghc-ver: pkgs.haskell.packages."${ghc-ver}".hasql-interpolate);
    })
    // {
      overlay = final: prev: {
        haskell = with prev.haskell.lib;
          prev.haskell
          // {
            packages = let
              ghcs =
                prev.lib.filterAttrs
                (k: v: prev.lib.strings.hasPrefix "ghc" k)
                prev.haskell.packages;
              patchedGhcs = builtins.mapAttrs patchGhc ghcs;
              patchGhc = k: v:
                prev.haskell.packages."${k}".extend (self: super:
                  with prev.haskell.lib;
                  with builtins;
                  with prev.lib.strings; let
                    cleanSource = pth: let
                      src' = prev.lib.cleanSourceWith {
                        filter = filt;
                        src = pth;
                      };
                      filt = path: type: let
                        bn = baseNameOf path;
                        isHiddenFile = hasPrefix "." bn;
                        isFlakeLock = bn == "flake.lock";
                        isNix = hasSuffix ".nix" bn;
                      in
                        !isHiddenFile && !isFlakeLock && !isNix;
                    in
                      src';
                  in {
                    generic-monoid = doJailbreak super.generic-monoid;
                    tagged = doJailbreak super.tagged;
                    tmp-postgres = dontCheck (super.callCabal2nix "tmp-postgres" tmp-postgres-src {});
                    hasql = dontCheck (super.callHackageDirect {
                      pkg = "hasql";
                      ver = "1.8";
                      sha256 = "sha256-+ZTJTIROjNqkOPkDtnhNQFq99Pi4IhOyIRcDqxqQbgY=";
                    } {});
                    postgresql-binary = dontCheck (super.callHackageDirect {
                      pkg = "postgresql-binary";
                      ver = "0.14";
                      sha256 = "sha256-ldXhe3JpdOXQvB7LE+5D4SUpNuwRjfw7zceV9BTVcUA=";
                    } {});
                    postgresql-libpq = dontCheck (super.callHackageDirect {
                      pkg = "postgresql-libpq";
                      ver = "0.10.1.0";
                      sha256 = "sha256-tXOMqCO8opMilI9rx0D+njqjIjbZsH168Bzb8Aq8Ff4=";
                    } {});
                    hasql-interpolate = let
                      p =
                        self.callCabal2nix "hasql-interpolate"
                        (cleanSource ./.)
                        {};
                    in
                      overrideCabal p (drv: {
                        testToolDepends = [prev.postgresql];
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
