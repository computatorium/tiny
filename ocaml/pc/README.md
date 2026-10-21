# Skiena & Revilla's *Programming Challenges*

This directory contains an OCaml catalog and harness for the 112 problems in *Programming Challenges* by Steven S. Skiena and Miguel A. Revilla.
No solutions are included yet.

## Usage

With OCaml 5 and Dune 3.22 installed:

```sh
dune build
dune exec pc -- list
dune exec pc -- init 1.6.1
dune exec pc -- run 1.6.1 < input.txt
dune exec pc -- test --chapter 1
dune runtest
```

- Catalog titles and sections are taken from the [publisher's contents](https://link.springer.com/content/pdf/bfm:978-0-387-22081-9/1)
- UVa mapping are from the [official chapter catalog](https://onlinejudge.org/index.php?Itemid=8&category=28&option=com_onlinejudge).
