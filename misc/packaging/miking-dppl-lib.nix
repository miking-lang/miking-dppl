{ lib, runCommand, nix-gitignore,
  miking-lib,
}:

let
  args = {
    propagatedNativeBuildInputs = [ miking-lib ];
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
  };
in

runCommand "miking-dppl-lib" args ''
  mkdir -p $out/lib/mcore
  cp -r ${nix-gitignore.gitignoreSource [] ../..}/coreppl/src $out/lib/mcore/coreppl
''
