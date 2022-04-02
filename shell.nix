let pkgs = import ( # A recent `master` to have a newer `secp256k1` with Schnorr support for Plutus V2.
  builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/990469ae68976a1a2e60785923ebf44b5642f1dc.tar.gz"
) {};
in with pkgs;
mkShell {
  name = "plutus-apps";
  buildInputs = [
    git
    haskell.compiler.ghc8107
    haskellPackages.cabal-install
    libsodium
    pkgconfig
    R
    secp256k1
    zlib
  ];
  GIT_SSL_CAINFO = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
}
