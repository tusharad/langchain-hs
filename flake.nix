{
  description = "langchain-hs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      systems = [ "aarch64-darwin" ];
    in
    {
      devShells = nixpkgs.lib.genAttrs systems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };

          fourmolu = pkgs.stdenvNoCC.mkDerivation {
            pname = "fourmolu";
            version = "0.17.0.0";

            src = pkgs.fetchurl {
              url = "https://github.com/fourmolu/fourmolu/releases/download/v0.17.0.0/fourmolu-0.17.0.0-osx-arm64";
              hash = "sha256-kOLVVmRCZCiOP1bzJU/h7AX0X6zM3bvjzBU160zQHcI=";
            };

            dontUnpack = true;

            installPhase = ''
              mkdir -p "$out/bin"
              cp "$src" "$out/bin/fourmolu"
              chmod +x "$out/bin/fourmolu"
            '';
          };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              ghc
              zlib
              stack
              cabal-install
              haskell-language-server
              haskellPackages.implicit-hie
              haskellPackages.hoogle
              fourmolu
              hlint
            ];
          };
        }
      );
    };
}
