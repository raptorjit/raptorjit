# raptorjit.nix - compile RaptorJIT with reference toolchain

{ pkgs, source, version }:

with pkgs;
#with llvmPackages_4.stdenv;  # Use clang 4.0
with stdenv;

mkDerivation rec {
  name = "raptorjit-${version}";
  inherit version;
  src = source;
  buildInputs = [ luajit nasm ];  # LuaJIT to bootstrap DynASM
  dontStrip = true;
  patchPhase = ''
    substituteInPlace Makefile --replace "/usr/local" "$out"
  '';
  configurePhase = ''
    make reusevm
  '';
  installPhase = ''
    make install PREFIX="$out"
  '';
  # Simple inventory test.
  installCheckPhase = ''
    for file in bin/raptorjit lib/libraptorjit-5.1.so \
                lib/pkgconfig/raptorjit.pc; do
      echo "Checking for $file"
      test -f $out/$file
    done
  '';
  doInstallCheck = true;
  enableParallelBuilding = true;  # Do 'make -j'
}

