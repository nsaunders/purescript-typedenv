{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import
    (pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "c766802fa7a6194fd80c31d8907f07ebacc4869c";
      sha256 = "0f0b3bhf8id1vnga0vaj7bldfykis8vxfiyll3rc003b21sx9w4n";
    }) {
    inherit pkgs;
  };
in

pkgs.mkShell {
  buildInputs = [
    pkgs.nodejs-14_x
    easy-ps.purs-0_14_1
    easy-ps.spago
  ];
}
