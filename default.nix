{ revision }:



(import ./reflex-platform {}).project ({ pkgs, ... }:

let
  fasttext = import (revision.uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit (pkgs) stdenv fetchgit; };
  res_corenlp = import (revision.uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit (pkgs) fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = pkgs.lib.callPackageWith (pkgs//revision) (revision.uphere-nix-overlay + "/nix/haskell-modules/configuration-spapi.nix")
               { inherit corenlp corenlp_models fasttext; };

in


{
  packages = {
    spapi-common = ./spapi-common;
    spapi-server = ./spapi-server;
    semantic-reflex = ./semantic-reflex/semantic-reflex;
    servant-reflex = ./servant-reflex;
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


  #android.frontend = {
  #  executableName = "frontend";
  #  applicationId = "org.example.frontend";
  #  displayName = "Example Android App";
  #};

  #ios.frontend = {
  #  executableName = "frontend";
  #  bundleIdentifier = "org.example.frontend";
  #  bundleName = "Example iOS App";
  #};

  tools = ghc:
            # TODO: move this code to uphere-nix-overlay
            let corenlpenv = pkgs.makeSetupHook { }
                  (pkgs.writeText "setup-hook.sh" ''
                     export CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${ghc.HCoreNLP}/share/x86_64-linux-ghc-8.4.3/HCoreNLP-0.1.0.0/HCoreNLPProto.jar"
                  '');
            in if ghc.ghc.isGhcjs or false then [] else [ corenlpenv ] ;

  shells = {
    ghc = ["spapi-common" "spapi-server" ];
    ghcjs = ["spapi-common" "semantic-reflex" "spapi-reflex" "servant-reflex" ];
  };
})
