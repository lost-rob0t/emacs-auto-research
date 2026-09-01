{
  description = "Generic Emacs auto-research control plane";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          epkgs = pkgs.emacsPackages;
          autoResearch = epkgs.trivialBuild {
            pname = "auto-research";
            version = "0.1.0";
            src = ./lisp;
            packageRequires = [ epkgs.org ];

            meta = {
              description = "Generic project-scoped Emacs UI for research documents and human gates";
              homepage = "https://github.com/lost-rob0t/emacs-auto-research";
              license = pkgs.lib.licenses.gpl3Only;
              platforms = pkgs.lib.platforms.all;
            };
          };
        in
        {
          default = autoResearch;
          auto-research = autoResearch;
        }
      );

      checks = forAllSystems (system: {
        package = self.packages.${system}.default;
      });

      overlays.default = final: _prev: {
        emacs-auto-research = self.packages.${final.stdenv.hostPlatform.system}.default;
      };
    };
}
