args: import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixos-unstable-2020-08-02";
  url = "https://github.com/nixos/nixpkgs-channels/";
  # Commit hash for nixos-unstable as of 2020-08-02
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixpkgs-unstable`
  ref = "refs/heads/nixpkgs-unstable";
  rev = "ccd458053b0eaf66c1f639ee1396248d8f4431e2";
}) args
