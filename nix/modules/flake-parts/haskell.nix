{ root, inputs, ... }:
{
  imports = [
    inputs.haskell-flake.flakeModule
  ];
  perSystem = { self', lib, config, pkgs, ... }: {
    # Our only Haskell project. You can have multiple projects, but this template
    # has only one.
    # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
    haskellProjects.default = {
      # To avoid unnecessary rebuilds, we filter projectRoot:
      # https://community.flake.parts/haskell-flake/local#rebuild
      projectRoot = builtins.toString (lib.fileset.toSource {
        inherit root;
        fileset = lib.fileset.unions [
          (root + /src)
          (root + /medea.cabal)
          (root + /LICENSE.md)
          (root + /README.md)
          (root + /.hspec)
          (root + /CHANGELOG.md)
          (root + /SPEC.md)
          (root + /TUTORIAL.md)
          (root + /conformance)
          (root + /test)
        ];
      });

      packages = {
      };

      # Add your package overrides here
      settings = {
        smash.broken = false;
        smash.jailbreak = true;
      };

      autoWire = [ "packages" "checks" ]; # Wire all but the devShell
    };

    packages.default = self'.packages.medea;
  };
}
