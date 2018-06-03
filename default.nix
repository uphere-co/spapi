{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    semantic-reflex = ./semantic-reflex;
    semantic-reflex-example = ./semantic-reflex-example;
  };


  overrides = self: super: {

    reflex-dom-contrib = self.callCabal2nix
      "reflex-dom-contrib"
      (pkgs.fetchFromGitHub {
        owner = "reflex-frp";
        repo = "reflex-dom-contrib";
        rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
        sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
      }) {};

    reflex-dom-nested-routing = self.callCabal2nix
      "reflex-dom-nested-routing"
      (pkgs.fetchFromGitHub {
        owner = "3noch";
        repo = "reflex-dom-nested-routing";
        rev = "c49c75c693de8516d1b19314be500482bea9426c";
        sha256 = "00bmakqm9893h8l3w7l1r1fjkpyffifcaicqmj2q5wwlfvm96hbf";
      }) {};

    #wai-middleware-etag = doJailbreak super.wai-middleware-etag;

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

  shells = {
    ghc8_2_1 = ["common" "backend" ];
    ghcjs = ["common" "semantic-reflex" "semantic-reflex-example" ];
  };
})
