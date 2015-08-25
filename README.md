# Haskell UNO Binding

This project aims at providing a binding to LibreOffice's Universal Network
Objects (UNO) for the Haskell language.

It consists on three parts:

   - `hs_unoidl` to generate code from IDL specifications to call the API;
   - `hs_uno` that is the core of the binding, providing base types and
     necessary code to call the API; and,
   - `examples` showing different ways to use the binding.

Information on how to build a cabal package that uses the UNO API is provided
in the `README` of the `hs-uno` package.

## Running an Example

To run an example, `hs_unoidl` needs to be compiled first and then the example
can be compiled and run. The steps, from cloning the repository to running the
example *macro-expander*, are:

```bash
# required environment variables
export LO_INSTDIR=/path/to/libreoffice/installation/directory
export LO_SRC=/path/to/libreoffice/source/code
# at least with the GCC toolchain on Fedora 22, also set:
export LD_RUN_PATH=$LO_INSTDIR/program
# clone the repository
git clone git@github.com:jorgecunhamendes/haskell-uno-binding.git
# compile hs_unoidl
cd haskell-uno-binding/hs_unoidl/
make
cd ..
# compile the example
cd examples/macro_expander/
cabal sandbox init                    # create a sandbox for the example's dependencies
cabal sandbox add-source ../../hs_uno # add hs_uno to the sandbox
cabal install --only-dependencies     # install the dependencies
 # if the above fails with "The package list for 'hackage.haskell.org' does not
 # exist" then run "cabal update"
cabal configure                       # configure the example
cabal build                           # build it
cabal run                             # run it
```

**Note 1**: Do not forget to set `LO_INSTDIR` and `LO_SRC` to the correct
values.

**Note 2**: These steps were tested with Ubuntu 14.04, gcc 4.8.4, GHC 7.6.3,
cabal-install-1.22.2.0, and text-1.2.1.1.

**Note 3**: You need LibreOffice's source code (sufficiently recent).

**Note 4**: If the binaries in the installation directory were not made with
`--dbg-utils` enabled, remove the line containing `-D_GLIBCXX_DEBUG` from
`hs_unoidl/Makefile`.

---

*Original repository:* https://github.com/jorgecunhamendes/haskell-uno-binding
