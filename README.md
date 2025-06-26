# Markdown doctest

## Installation

```
$ cabal install markdown-doctest
```

## Usage

```
$ ./markdown-doctest <filename.md>
```

Note: The ghc environment file should exist.

## Syntax

3 type of codeblocks are parsed,

- `haskell`: All `haskell` blocks are tangled into a common file and checked.
- `haskell unshared`: These blocks are loaded independently and dont share anything.
- `haskell docspec`: These blocks share the syntax of docspec and share the env.

## Example

`markdown-doctest` will successfully validate this file.

```haskell
import qualified Prelude as P
```

```haskell unshared
import qualified Prelude as P

main =
    P.print "Hello, World!"
```

```haskell docspec
>>> import qualified Prelude as P
```

```haskell
main =
    P.print "Hello, World!"
```

```haskell docspec
>>> P.filter P.odd [1..10]
[1,3,5,7,9]
```
