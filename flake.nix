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
              # datastar-hs-zstd needs `zstd >= 0.1.4` (streaming `flushStream` FFI,
              # https://github.com/starfederation/datastar-haskell/issues/3), but nixpkgs
              # still ships zstd-0.1.3.0, so pull the release tarball from Hackage.
              zstd.source = pkgs.fetchzip {
                url = "https://hackage.haskell.org/package/zstd-0.1.4.0/zstd-0.1.4.0.tar.gz";
                hash = "sha256-+FbP4zIRfSJr3EKuO1i0+y6CAwBdPKPr/N/mFDzWOQ4=";
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
              # datastar-hs-zstd -> none (the zstd package bundles the zstd C sources)
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
