# Nix Archive

This is a library for generating Nix Archive (NAR) files. This package is currently incomplete
and only satisfies one particular need, being able to generate Nix Sha256 hashes for a git
checkout at a specified git hash (without actually having Nix installed).

It is written as a reusable library and command line wrapper around the library.

## What works

* Generating a NAR file from a git repo either at HEAD of the repo or at a specified git hash.
* Generate the Nix SHA256 hash from a git repo either at HEAD of the repo or at a specified git
  hash (without actually generating the NAR file).
* Generate a Nix SHA256 hash for a given file.

## Required to make this complete

This is what is needed for this to become complete enough to upload to Hackage:

* Generate a NAR file from a specified directory.
* Generate a Nix SHA256 hash from a specified directory.
* Unpack a NAR file.
* Tests.
