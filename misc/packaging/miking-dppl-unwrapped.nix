{ lib, stdenv, nix-gitignore,
  ocamlPackages,
  miking-lib, miking-unwrapped,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "miking-dppl-unwrapped";
  version = "0.0.0+git";

  src = nix-gitignore.gitignoreSource "/misc/packaging\n/result\n" ../..;

  nativeBuildInputs = [
    miking-unwrapped
    miking-lib
    ocamlPackages.findlib
    ocamlPackages.ocaml
    ocamlPackages.linenoise
  ];

  makeFlags = [ "BIN_PATH=$(out)/bin" ];

  installTargets = [ "install-cppl" ];

  meta = with lib; {
    mainProgram     = "cppl";
    description     = "Meta language system for creating embedded DSLs";
    homepage        = "https://miking.org";
    license         = licenses.mit;
    longDescription = ''
      Miking (Meta vIKING) is a meta language system for creating
      embedded domain-specific and general-purpose languages.  The
      system features a polymorphic core calculus and a DSL definition
      language where languages can be extended and composed from
      smaller fragments.

      Note: Depending on the target runtime, miking requires the presence of
      additional packages within an environment, such as dune, ocaml, findlib
      and a C compiler for native builds, node for javascript, and a suitable JDK
      when targeting the JVM.
    '';
  };
})
