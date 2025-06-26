{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: with nixpkgs.legacyPackages.${system}; {
      devShells.default = mkShell {
        buildInputs = [
          sbt
          scala
          jre8
          verilator
	  gtkwave
          zlib
         (python311.withPackages (p: with p; [pycryptodome pycrypto]))

        ];
      };
    });
}

#example
#run `nix develop'
# run 'sbt "runMain decim.DecIMv1Verilog" --java-home /nix/store/kb41c4clzp8c6lr55cwylhjk826zw7if-openjdk-8u412-ga-jre/lib/openjdk/jre'
