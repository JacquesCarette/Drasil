# A (nearly) fully featured development environment for working with Drasil. One
# missing requirement is an installation of the `Latin Modern` and `Latin Modern
# Math` font families (nix-shell understandably does not support installing
# fonts).
{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Stack is our full Haskell toolchain manager
    stack

    # Raw GHC + HLS
    haskell.compiler.ghc927
    haskell.packages.ghc927.haskell-language-server

    # Makefile processor
    gnumake

    # Document-related compilers needed for examples
    graphviz
    inkscape
    imagemagick
    texlive.combined.scheme-full

    # Extra compilers needed for examples
    gcc # C/C++
    mono # C#
    jdk8 # Java
    swift # Swift

    # Python + dependencies
    (python310.withPackages (ps: with ps; [pandas numpy scipy]))
  ];

  # NOTE: If fonts are ever allowed here, we will want them.
  # # Font requirements for TeX compilation
  # fonts = with pkgs; [
  #   lmodern # Latin Modern Font
  #   lmmath  # Latin Modern Math Font
  # ];
}
