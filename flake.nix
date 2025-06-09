{
  description = "Zig project flake";

  inputs = {
    zig2nix.url = "github:Cloudef/zig2nix";
  };

  outputs = { zig2nix, ... }: let
    flake-utils = zig2nix.inputs.flake-utils;
  in (flake-utils.lib.eachDefaultSystem (system: let
      # Zig flake helper
      # Check the flake.nix in zig2nix project for more options:
      # <https://github.com/Cloudef/zig2nix/blob/master/flake.nix>
			env = zig2nix.outputs.zig-env.${system} { zig = zig2nix.outputs.packages.${system}.zig-0_14_1; };
			pkgs = env.pkgs;
    in with builtins; with pkgs.lib; rec {
      # Produces clean binaries meant to be ship'd outside of nix
      # nix build .#foreign
      packages.foreign = env.package {
        src = cleanSource ./.;

        # Packages required for compiling
        nativeBuildInputs = with pkgs; [
					git
					alsa-lib
				];

        # Packages required for linking
        buildInputs = with pkgs; [
					xorg.libX11
					xorg.libXi
					xorg.libXcursor
					xorg.libXext
					xorg.libXfixes
					libGL
					alsa-lib
					alsa-lib.dev
				];

        # Smaller binaries and avoids shipping glibc.
        zigPreferMusl = true;
      };

      # nix build .
      packages.default = packages.foreign.override (attrs: {
        # Prefer nix friendly settings.
        zigPreferMusl = false;

        # Executables required for runtime
        # These packages will be added to the PATH
        zigWrapperBins = with pkgs; [
				];

        # Libraries required for runtime
        # These packages will be added to the LD_LIBRARY_PATH
        zigWrapperLibs = attrs.buildInputs or [
					alsa-lib
					alsa-lib.dev
			 	];

				zigBuildFlags = [
					"--search-prefix ${pkgs.alsa-lib}"
					"--search-prefix ${pkgs.alsa-lib.dev}"
					"--search-prefix ${pkgs.xorg.libXext.dev}"
					"--search-prefix ${pkgs.xorg.libXfixes.dev}"
				];
      });

      # For bundling with nix bundle for running outside of nix
      # example: https://github.com/ralismark/nix-appimage
      apps.bundle = {
        type = "app";
        program = "${packages.foreign}/bin/default";
      };

      # nix run .
      apps.default = env.app [] "zig build run -- \"$@\"";

      # nix run .#build
      apps.build = env.app [] "zig build \"$@\"";

      # nix run .#test
      apps.test = env.app [] "zig build test -- \"$@\"";

      # nix run .#docs
      apps.docs = env.app [] "zig build docs -- \"$@\"";

      # nix run .#zig2nix
      apps.zig2nix = env.app [] "zig2nix \"$@\"";

      # nix develop
      devShells.default = env.mkShell {
        # Packages required for compiling, linking and running
        # Libraries added here will be automatically added to the LD_LIBRARY_PATH and PKG_CONFIG_PATH
        nativeBuildInputs = []
          ++ packages.default.nativeBuildInputs
          ++ packages.default.buildInputs
          ++ packages.default.zigWrapperBins
          ++ packages.default.zigWrapperLibs;
      };
    }));
}
