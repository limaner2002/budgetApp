{ghc}:
with (import <nixpkgs> {});

let nativeLibs = [ zlib ] ++ (with darwin.apple_sdk.frameworks; [
    Cocoa
    CoreServices
  ]);

in haskell.lib.buildStackProject {
  inherit ghc;
  name = "diagrams-report";
  buildInputs = nativeLibs;
}
