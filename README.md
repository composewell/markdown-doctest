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

3 type of codeblocks are permitted,

- haskell
- ghci
- docspec

All the codeblocks share the same ghci session

Example,

````

# Examples

## Example 1,

Multiple `ghci` blocks share the same ghci session.

```ghci
import Data.Function ((&))
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent as Concur

f1 x =
    Prelude.map ($ x) [return . (+ 1), return . (+ 2)] -- List of actions
        & Stream.fromList                              -- Stream of actions
        & Concur.sequenceWith (Concur.ordered True)    -- Sequence the actions concurrently
        & Stream.fold Fold.toList                      -- Fold the resulting stream into a list
```

```ghci
import Data.Maybe (fromJust)

f2 x =
  let actionStream1 = Stream.fromPure (return $ show x)
      actionStream2 = Stream.fromPure (return $ x + 1)
      actionStream3 = Stream.fromPure (return $ fromIntegral x / 2)
   in Stream.zipWith (\a (b, c) -> (a, b, c))
          (Concur.sequence actionStream1)
          (Stream.zipWith (,)
               (Concur.sequence actionStream2)
               (Concur.sequence actionStream3))
          & Stream.fold Fold.one
          & fmap fromJust
```

## Example 2,

`haskell` blocks are not run in ghci directly but loaded in as a file.  And
hence, these blocks cannot share the context and should be complete on their
own.

```haskell
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Function ((&))
import Data.Maybe (fromJust)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream.Concurrent (MonadAsync)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Stream
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Stream.Concurrent
import qualified Streamly.Data.Stream.Concurrent as Concur
import qualified Streamly.Internal.Data.Stream.TypeGen as StreamTypeGen

applyConcurrent :: MonadAsync m => Stream m (a -> b) -> Stream m a -> Stream m b
applyConcurrent = Concur.zipWith ($)

$(StreamTypeGen.mkZippingType "ZipConcurrent" "applyConcurrent" True)

f2 x =
  let actionStream1 = Stream.fromEffect (return $ show x)
      actionStream2 = Stream.fromEffect (return $ x + 1)
      actionStream3 = Stream.fromEffect (return $ fromIntegral x / 2)
   in ((,,,)
           <$> actionStream1
           <*> actionStream2
           <*> actionStream3)
          & Stream.fold Fold.one
          & fmap fromJust
```

## Example 3,

`docspec` blocks can be used to check results after execution. The `ghci` block
is a simpler version of `docspec` block.

```docspec
>>> import qualified Streamly.Internal.Data.Array as Array
>>> :{
  Stream.fromList [1,2,3,4,5::Int]
& Stream.scan (Array.writeLastN 2)
& Stream.fold Fold.toList
:}
[fromList [],fromList [1],fromList [1,2],fromList [2,3],fromList [3,4],fromList [4,5]]

>>> g f2 [1,2,3,4::Int]
[("1",2,0.5),("2",3,1.0),("3",4,1.5),("4",5,2.0)]
```

````
