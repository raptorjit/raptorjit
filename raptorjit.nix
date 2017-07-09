# raptorjit.nix - compile RaptorJIT with reference toolchain

{ pkgs, source, version }:

with pkgs;
with llvmPackages_4.stdenv;  # Use clang 4.0

mkDerivation rec {
  name = "raptorjit-${version}";
  inherit version;
  src = source;
  buildInputs = [
      luajit                # LuaJIT to bootstrap DynASM
      gcc6                  # GCC for generating DWARF info
    ];
  dontStrip = true;         # No extra stripping (preserve debug info)
  installPhase = ''
    install -D src/luajit $out/bin/raptorjit
    install -D src/lj_dwarf.dwo $out/lib/raptorjit.dwo
  '';

  enableParallelBuilding = true;  # Do 'make -j'
}

