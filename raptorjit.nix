# raptorjit.nix - compile RaptorJIT with reference toolchain

{ pkgs, source, version }:

with pkgs;
#with llvmPackages_4.stdenv;  # Use clang 4.0
with stdenv;

mkDerivation rec {
  name = "raptorjit-${version}";
  inherit version;
  src = source;
  buildInputs = [ luajit ];  # LuaJIT to bootstrap DynASM
  installPhase = ''
    mkdir -p $out/bin
    cp src/raptorjit $out/bin/raptorjit
  '';
  dontStrip = true;
  enableParallelBuilding = true;  # Do 'make -j'
}

