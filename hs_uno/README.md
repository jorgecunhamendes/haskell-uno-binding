# Haskell UNO Binding Package

This package contains a bridge between the UNO framework and Haskell code.

## Setup

Create a cabal package.

Copy the `Setup.hs` from one of the examples and modify it to meet the program's
requirements. One part that may need changes is the type databases to use
(`typeDbs`).

In the cabal package file, change the build type to `Custom`. Also, the current
setup generates code to the `gen` directory. This information can be added to
the cabal file in the `extra-tmp-files` field. The generated files are added
automatically during configuration, thus there is no need to add this
information manually. The global fields that need to be added/modified are:

```
build-type:          Custom
extra-tmp-files:     gen
```

The executable specification in the cabal file should have `hs-uno` has a
dependency. Moreover, for the configuration phase, the list of types required by
the program should be specified in the custom field `x-lo-sdk-types` of the
executable. For the _macro expander_ example, the relevant part is:

```
executable macro-expander
  [...]
  build-depends:       base >=4.6, text, hs-uno
  [...]
  x-lo-sdk-types:
    com.sun.star.util.theMacroExpander
    com.sun.star.util.XMacroExpander
    com.sun.star.uno.XInterface
    com.sun.star.uno.XComponentContext
```

## Usage

In the main module, import `UNO` and the required generated modules. If the
Haskell type `XComponentContext` from the `Com.Sun.Star.Uno` (corresponding to
the UNO type `com.sun.star.uno.XComponentContext`) is needed, import the
`Com.Sun.Star.Uno` module. If functions on the `XComponentContext` type are
needed, import the `Com.Sun.Star.Uno.XComponentContext` module. That is, the
types are specified in their parent module, while their implementation is in
their own module.

## UNO Types

The equivalence of UNO types with Haskell types is specified in the table below.
Note that this work is not yet complete, thus some equivalences are not yet
fully defined.

| UNO Type       | Haskell Type |
|----------------|--------------|
| void           | ()           |
| boolean        | Bool         |
| byte           | Word8        |
| short          | Int16        |
| unsigned short | Word16       |
| long           | Int32        |
| unsigned long  | Word32       |
| hyper          | Int64        |
| unsigned hyper | Word64       |
| float          | Float        |
| double         | Double       |
| char           | Char         |
| string         | Text         |
| type           | _NA_         |
| any            | Any          |
| sequence       | [a]          |
| enum           | _NA_         |
| struct         | Ptr a        |
| exception      | _NA_         |
| interface      | Reference a  |

The conversion between values from one system to another is automatic. However,
some considerations must be made for certain types.

### Any

When putting/retrieving a value into/from an `Any`, some types need conversions
that require operations in the `IO` monad. Thus, two kind of creation/retrieval
operations are available: `toAny`/`fromAny` that are pure, and
`toAnyIO`/`fromAnyIO` that operate in the IO monad. Most of the time, the choice
falls on the former. The latter is required when extracting an interface and a
query must be made in order to get the correct one.
