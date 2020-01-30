let
  src = builtins.fetchTarball {
    name = "nixos-unstable";
    url = https://github.com/nixos/nixpkgs/archive/8a9807f1941d046f120552b879cf54a94fca4b38.tar.gz;
    # git ls-remote https://github.com/nixos/nixpkgs-channels nixos-unstable
    sha256 = "0s8gj8b7y1w53ak138f3hw1fvmk40hkpzgww96qrsgf490msk236";
  };
  pkgs = import src {};
  haskellPackagesAug = pkgs.haskellPackages.override {
    overrides = self: super: {
      gloss-rendering = super.gloss-rendering.overrideAttrs (oldAttrs: oldAttrs // {
        patches = [ ./gloss-image-blend.patch ];
      });
    };
  };

in pkgs.haskellPackagesAug.callPackage ./production.nix {}
