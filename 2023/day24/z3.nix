{ lib
, stdenv
, fetchFromGitHub
, python
, fixDarwinDylibNames
, javaBindings ? false
, ocamlBindings ? false
, pythonBindings ? true
, jdk ? null
, ocaml ? null
, findlib ? null
, zarith ? null
, writeScript
}:

assert javaBindings -> jdk != null;
assert ocamlBindings -> ocaml != null && findlib != null && zarith != null;

with lib;

let common = { patches ? [ ] }:
  stdenv.mkDerivation rec {
    name = "myz3";
    pname = "z3";
    inherit patches;
    src = fetchFromGitHub {
      owner = "Z3Prover";
      repo = pname;
      rev = "master";
      sha256 = "sha256-selQp+VWhyYZ9idFzMrT/Erpq/wHyvwo41F72Ycchj0=";
    };

    strictDeps = true;

    nativeBuildInputs = [ python ]
      ++ optional stdenv.hostPlatform.isDarwin fixDarwinDylibNames
      ++ optional javaBindings jdk
      ++ optionals ocamlBindings [ ocaml findlib ]
    ;
    propagatedBuildInputs = [ python.pkgs.setuptools ]
      ++ optionals ocamlBindings [ zarith ];
    enableParallelBuilding = true;

    postPatch = optionalString ocamlBindings ''
      export OCAMLFIND_DESTDIR=$ocaml/lib/ocaml/${ocaml.version}/site-lib
      mkdir -p $OCAMLFIND_DESTDIR/stublibs
    '';

    configurePhase = concatStringsSep " "
      (
        [ "${python.pythonOnBuildForHost.interpreter} scripts/mk_make.py --prefix=$out" ]
          ++ optional javaBindings "--java"
          ++ optional ocamlBindings "--ml"
          ++ optional pythonBindings "--python --pypkgdir=$out/${python.sitePackages}"
      ) + "\n" + "cd build";

    doCheck = true;
    checkPhase = ''
      make test
      ./test-z3 -a
    '';

    postInstall = ''
      mkdir -p $dev $lib
      mv $out/lib $lib/lib
      mv $out/include $dev/include
    '' + optionalString pythonBindings ''
      mkdir -p $python/lib
      mv $lib/lib/python* $python/lib/
      ln -sf $lib/lib/libz3${stdenv.hostPlatform.extensions.sharedLibrary} $python/${python.sitePackages}/z3/lib/libz3${stdenv.hostPlatform.extensions.sharedLibrary}
    '' + optionalString javaBindings ''
      mkdir -p $java/share/java
      mv com.microsoft.z3.jar $java/share/java
      moveToOutput "lib/libz3java.${stdenv.hostPlatform.extensions.sharedLibrary}" "$java"
    '';

    outputs = [ "out" "lib" "dev" "python" ]
      ++ optional javaBindings "java"
      ++ optional ocamlBindings "ocaml";
  };
in
{
  z3 = common { };
}
