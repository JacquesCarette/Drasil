# Drasil development requirements
# Installs everything except `Latin Modern` and `Latin Modern Math` font families.

{ pkgs ? import (builtins.fetchGit {
    name = "pinned-pkgs";   # Pinned at around ~Stack v2.5.1                       
    url = "https://github.com/NixOS/nixpkgs/";                       
    ref = "refs/heads/nixos-unstable";                     
    rev = "046f8835dcb9082beb75bb471c28c832e1b067b6";                           
  }) {}
}:

let
  # Examples require some dependencies, these should eventually move into their respective artifact files
  py-deps = python38-packages: with python38-packages; [
    pandas
    numpy
    scipy
  ]; 
  py-with-deps = pkgs.python38.withPackages py-deps;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    # Stack is our full Haskell toolchain manager
    stack

    # Makefile processor
    gnumake
    
    # Printing-related requirements (TeX + Graphs/Analysis)
    graphviz
    inkscape
    imagemagick
    texlive.combined.scheme-medium
    
    # Compilers
    gcc            #  C/C++
    mono           #  C#
    jdk8           #  Java
    swift          #  Swift
    py-with-deps   #  Python + dependencies for examples
  ];

  # NOTE: If fonts are ever allowed here, we will want them.
  # # Font requirements for TeX compilation
  # fonts = with pkgs; [
  #   lmodern # Latin Modern Font
  #   lmmath  # Latin Modern Math Font
  # ];
}
