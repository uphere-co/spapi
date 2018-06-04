{ revision }:

with revision;

let pkgs = import nixpkgs { config.allowUnfree = true; };

in

with pkgs;

let

  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

in

# TODO: change absolute path to appropriate hsenv

stdenv.mkDerivation {
  name = "semantic-parser-api-compute-dev";
  buildInputs = [ jdk ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:/nix/store/469zpr8nbiy47x7y9dfz44cc4m1zqimc-ghc-8.2.1-with-packages/share/x86_64-linux-ghc-8.2.1/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}
