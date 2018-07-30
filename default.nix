{pkgs ? import <nixpkgs> {}}:
let all = (import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    some-board-game = ./. ;
  };

  shells = {
    ghc = ["some-board-game"];
    ghcjs = ["some-board-game"];
  };

  useWarp = true;
  
  overrides = self: super: {
    some-board-game-rules = self.callCabal2nix "some-board-game-rules" (pkgs.fetchFromGitHub {
      owner = "martin-kolinek";
      repo = "some-board-game-rules";
      rev = "43e31aaa160f952cb99beb9d5074b6678f8b7d8c";
      sha256 = "1w6h3hfzcg6kxdxdyfks3id0h8mwi81jshakx0rc4mibp2wpdgnn";
    }) {};
  };
});
    dataDir = ./data;
    withData = pkgs.runCommand "some-board-game-with-data" {passthru = all; preferLocalBuild = true; } ''
  mkdir $out
  cp -rv ${dataDir} $out/data
  cp -rv ${all.ghcjs.some-board-game}/bin/some-board-game.jsexe/. $out
'';
in all // withData