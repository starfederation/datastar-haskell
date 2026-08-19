{
  description = "Datastar Haskell SDK";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, ... }:
        {
          haskellProjects.default = {
            packages = {
              # `haskell-flake` ignores `source-repository-package` defined in `cabal.project`.
              # That's why `hs-zstd` needs to be pinned here. All because of https://github.com/starfederation/datastar-haskell/issues/3
              # Note: `fetchSubmodules` is needed -> `zstd.cabal` provides `zstd` in a submodule.
              zstd.source = pkgs.fetchgit {
                url = "https://github.com/luispedro/hs-zstd";
                rev = "59c0d21ab65a6296e06446c8368d78b9b4c7e64d";
                fetchSubmodules = true;
                hash = "sha256-jGRiFZf5M/mNOXeWjtEkDQOZEi0Vry+pYRLutJkHuT0=";
              };

              # Note: `WAI.hs` needs `hAcceptEncoding` (from `Network.HTTP.Types`),
              # which is available in `http-types` >= `v0.12.5` only.
              # Side note: nixpkgs pins to `0.12.4` only. `source = "0.12.5"` does not work either (not available in frozen Hackage index).
              http-types.source = pkgs.fetchzip {
                url = "https://hackage.haskell.org/package/http-types-0.12.5/http-types-0.12.5.tar.gz";
                hash = "sha256-Y1/wrRFPIVxgTGWgPboRDUht+fzvl3e1jazj+G1pTw0=";
              };
            };

            devShell = {
              tools = hp: {
                # cabal-install, haskell-language-server, ghcid and hlint are defaults.
                inherit (hp) fourmolu cabal-fmt;
                inherit (pkgs) pkg-config nixfmt nixd;
              };

              # compressor sub-packages link against system C libs:
              # datastar-hs-brotli -> brotli
              # datastar-hs-zlib -> zlib
              # datastar-hs-zstd -> none (vendors its own C sources via hs-zstd and its git submodule)
              mkShellArgs.buildInputs = [
                pkgs.brotli
                pkgs.zlib
              ];
            };
          };

          formatter = pkgs.nixfmt;

          packages.default = self'.packages.datastar-hs;

          # `nix flake check` builds / tests everything (similar to `cabal build all && cabal test all`)
          checks.all = pkgs.linkFarmFromDrvs "datastar-hs-all" (
            builtins.attrValues (removeAttrs self'.packages [ "default" ])
          );
        };
    };
}
