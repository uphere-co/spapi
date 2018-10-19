{ revision }:

let
  reflex-platform-src =
     (import (revision.uphere-nix-overlay + "/nix/web-modules/reflex.nix") {}).reflex-platform-src;
  servant-reflex-src =
     (import (revision.uphere-nix-overlay + "/nix/web-modules/reflex.nix") {}).servant-reflex-src;
  semantic-reflex-src =
     (import (revision.uphere-nix-overlay + "/nix/web-modules/reflex.nix") {}).semantic-reflex-src;


  reflex-platform = import reflex-platform-src {};

in

reflex-platform.project (

{ pkgs, ... }:

let
  fasttext = import (revision.uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit (pkgs) stdenv fetchgit; };
  corenlp_pkgs =
    import (revision.uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
      inherit (pkgs) fetchurl fetchzip srcOnly;
    };

  hsconfig =
    pkgs.lib.callPackageWith
      (pkgs//revision)
      (revision.uphere-nix-overlay + "/nix/haskell-modules/configuration-spapi.nix")
      {
        inherit fasttext;
        inherit (corenlp_pkgs) corenlp corenlp_models;
      };

  env-hook-gen =
    haskellPackages:
      import (revision.uphere-nix-overlay + "/nix/env/corenlp.nix") {
        inherit (pkgs) makeSetupHook writeText;
        inherit haskellPackages;
        corenlp = corenlp_pkgs.corenlp;
        corenlp_models = corenlp_pkgs.corenlp_models;
      };

in

{

  packages = {
    spapi-common = ./spapi-common;
    spapi-server = ./spapi-server;
    semantic-reflex = semantic-reflex-src + "/semantic-reflex";
    servant-reflex = servant-reflex-src;
    spapi-reflex = ./spapi-reflex;
  };

  overrides =
    self: super:

    hsconfig self super
    //
    {
      reflex-dom-contrib = pkgs.haskell.lib.doJailbreak (
        self.callCabal2nix
        "reflex-dom-contrib"
        (pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "10818e345c4cb34c6a1282add2c26e05c6007ad6";
          sha256 = "17ki3vnq4r4rhhl4yxkg82k2ybvw9xli950q8h6pmjb0qfmg27i9";
        })
        {}
      );

      reflex-dom-nested-routing = self.callCabal2nix
        "reflex-dom-nested-routing"
        (pkgs.fetchFromGitHub {
          owner = "3noch";
          repo = "reflex-dom-nested-routing";
          rev = "c49c75c693de8516d1b19314be500482bea9426c";
          sha256 = "00bmakqm9893h8l3w7l1r1fjkpyffifcaicqmj2q5wwlfvm96hbf";
        }) {};

      wai-middleware-etag = pkgs.haskell.lib.doJailbreak super.wai-middleware-etag;

      servant = pkgs.haskell.lib.dontCheck super.servant;

    };

  tools = ghc: let env-hook = env-hook-gen ghc;
               in if ghc.ghc.isGhcjs or false
                  then []
                  else [ env-hook ];  # NOTE: you cannot have non-variable in this list.

  shells = {
    ghc = ["spapi-common" "spapi-server" ];
    ghcjs = ["spapi-common" "semantic-reflex" "spapi-reflex" "servant-reflex" ];
  };

})
