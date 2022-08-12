{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    reflex-arc = {
      url = "github:ners/reflex-arc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
      inputs.flake-compat.follows = "flake-compat";
    };
    web-font-mdi = {
      url = "github:ners/web-font-mdi";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    with builtins;
    let
      pkgs = import inputs.nixpkgs { inherit system; };
      haskellPackages = pkgs.haskellPackages;
      haskellDeps = drv: concatLists (attrValues drv.getCabalDeps);
      reflex-arc = inputs.reflex-arc.packages.${system}.default;
      reflex-slides = haskellPackages.callCabal2nix "reflex-slides" ./. {
        inherit reflex-arc;
        inherit (inputs.web-font-mdi.packages.${system}) web-font-mdi;
      };
    in
    {
      packages = {
        inherit reflex-slides;
        default = reflex-slides;
      };

      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [
          (haskellPackages.ghcWithPackages (ps: haskellDeps reflex-slides))
          haskellPackages.cabal-install
          haskellPackages.haskell-language-server
          haskellPackages.hpack
        ];
      };
    });
}
