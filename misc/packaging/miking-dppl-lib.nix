{ lib, stdenvNoCC, nix-gitignore,
  miking-lib, mi-stats,
}:

stdenvNoCC.mkDerivation {
  pname = "miking-dppl-lib";
  version = "0.0.0+git";

  src = nix-gitignore.gitignoreSource "/misc/packaging\n/result\n" ../..;

  propagatedBuildInputs = [ miking-lib mi-stats ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/mcore
    cp -r coreppl/src $out/lib/mcore/coreppl
    runHook postInstall
  '';

  meta = with lib; {
    description     = "The DPPL fragments for the Miking framework";
    homepage        = "https://miking.org";
    license         = licenses.mit;
    longDescription = ''
      Miking (Meta vIKING) is a meta language system for creating
      embedded domain-specific and general-purpose languages.  The
      system features a polymorphic core calculus and a DSL definition
      language where languages can be extended and composed from
      smaller fragments.

      This package contains the library adding support for DPPL constructs.
    '';
  };
}
