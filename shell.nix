(import (
  let
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
    nodeName = lock.nodes.root.inputs.flake-compat;
  in
  builtins.fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${
      lock.nodes.${nodeName}.locked.rev
    }.tar.gz";
    sha256 = lock.nodes.${nodeName}.locked.narHash;
  }
) { src = ./.; }).shellNix
