# Curry 2 -> 3 Migration Guide

Between version 2 and 3 the standard library has undergone some
notable changes, like aligning module and function names more closely
with their Haskell equivalents and moving more specialized
functionality into separate packages.

This migration guide aims to provide a comprehensive outline of the
changes to make the transition of existing packages more seamless.
In order to find the modules and packages containing a given
operation, one should try the
[Currygle](https://cpm.curry-lang.org/currygle/)
search tool.

## Standard Library

### Renamed Modules

| Old           | New                           | Notes                       |
| ------------- | ----------------------------- | --------------------------- |
| `Char`        | `Data.Char`                   |                             |
| `Debug`       | `Debug.Trace`                 |                             |
| `Distribution`| `Curry.Compiler.Distribution` | `rcFileName` in `currypath` |
| `Either`      | `Data.Either`                 |                             |
| `Function`    | `Data.Function`               |                             |
| `GetOpt`      | `System.Console.GetOpt`       |                             |
| `IO`          | `System.IO`                   |                             |
| `IOExts`      | `Data.IORef`                  | Only includes the `IORef`   |
| `List`        | `Data.List`                   |                             |
| `Maybe`       | `Data.Maybe`                  | Now using `Monad` instance. |
| `ReadNumeric` | `Numeric`                     | Types changed.              |
| `ShowS`       | `Text.Show`                   |                             |

### Moved and Renamed Modules

| Old                    | New                             | New Package     |
| ---------------------- | ------------------------------- | --------------- |
| `AllSolutions`         | `Control.AllSolutions`          | `searchtree`    |
| `AnsiCodes`            | `System.Console.ANSI.Codes`     | `ansi-terminal` |
| `Array`                | `Data.Array`                    | `array`         |
| `CPNS`                 | `Network.CPNS`                  | `cpns`          |
| `Dequeue`              | `Data.Queue`                    | `queue`         |
| `Directory`            | `System.Directory`              | `directory`     |
| `ErrorState`           | `Control.Monad.Trans.Error`     | `transformers`  |
| `FilePath`             | `System.FilePath`               | `filepath`      |
| `FiniteMap`            | `Data.Map`                      | `containers`    |
| `Findall`              | `Control.Findall`               | `searchtree`    |
| `FunctionInversion`    | `Data.Function.Inversion`       | `inversion`     |
| `Nat`                  | `Data.Nat`                      | `peano`         |
| `NamedSocket`          | `Network.NamedSocket`           | `cpns`          |
| `Profile`              | `Debug.Profile`                 | `profiling`     |
| `Random`               | `System.Random`                 | `random`        |
| `RedBlackTree`         | `Data.RedBlackTree`             | `redblacktree`  |
| `SearchTree`           | `Control.SearchTree`            | `searchtree`    |
| `SearchTreeGenerators` | `Control.SearchTree.Generators` | `searchtree`    |
| `SearchTreeTraversal`  | `Control.SearchTree.Traversal`  | `searchtree`    |
| `SCC`                  | `Data.SCC`                      | `scc`           |
| `SetRBT`               | `Data.Set.RBTree`               | `redblacktree`  |
| `Socket`               | `Network.Socket`                | `socket`        |
| `State`                | `Control.Monad.Trans.State`     | `transformers`  |
| `TableRBT`             | `Data.Table.RBTree`             | `redblacktree`  |
| `Traversal`            | `Data.Traversal`                | `traversal`     |
| `Time`                 | `Data.Time`                     | `time`          |
| `ValueSequence`        | `Control.ValueSequence`         | `searchtree`    |

### Moved Modules

| Old                     | New Package     |
| ----------------------- | --------------- |
| `Combinatorial`         | `combinatorial` |
| `Control.Monad.Extra`   | `extra`         |
| `Control.Monad.Trans.*` | `transformers`  |
| `Data.Tuple.Extra`      | `extra`         |
| `Global`                | `global`        |
| `System.Process`        | `process`       |
| `ReadShowTerm`          | `read-legacy`   |
| `Test.*`                | `easycheck`     |

### Deleted Modules

| Old                       | Notes                                                |
| ------------------------- | ---------------------------------------------------- |
| `Sort`                    |                                                      |
| `FileGoodies`             | Migrated to `System.FilePath` and `System.Directory` |
| `System`                  | Split into `System.CPUTime` and `System.Environment` |
| `Integer`                 | Removed, important functions moved to `Prelude`.     |
| `Float`                   | Removed, important functions moved to `Prelude`.     |
| `Read`                    | Use `Read` instance                                  |

### Renamed Functions

#### Prelude

| Old          | New            |
| ------------ | -------------- |
| `liftIO`     | `fmap`         |
| `mapIO`      | `mapM`         |
| `mapIO_`     | `mapM_`        |
| `showError`  | `show`         |

#### System (previously)

| Old            | New            | Notes                                                    |
| -------------- | -------------- | -------------------------------------------------------- |
| `setEnviron`   | `setEnv`       |                                                          |
| `unsetEnviron` | `unsetEnv`     |                                                          |
| `getEnviron`   | `getEnv`       | `getEnvironment` was added, but has a different purpose. |
| `setEnviron`   | `setEnv`       |                                                          |

### Moved and Renamed Functions

#### Prelude

| Old          | New                    |
| ------------ | ---------------------- |
| `when`       | `Control.Monad.when`   |
| `unless`     | `Control.Monad.unless` |

#### Float (previously)

| Old          | New               |
| ------------ | ----------------- |
| `i2f`        | `Prelude.fromInt` |

#### FileGoodies (previously)

| Old                      | New                                   | Notes     |
| ------------------------ | ------------------------------------- | ----------|
| `separatorChar`          |                                       | removed   |
| `pathSeparatorChar`      | `System.FilePath.pathSeparator`       |           |
| `suffixSeparatorChar`    | `System.FilePath.extSeparator`        |           |
| `dirName`                | `System.FilePath.takeDirectory`       |           |
| `baseName`               | `System.FilePath.takeBaseName`        |           |
| `splitDirectoryBaseName` | `System.FilePath.splitFileName`       |           |
| `stripSuffix`            | `System.FilePath.dropExtension`       |           |
| `fileSuffix`             | `System.FilePath.takeExtension`       |           |
| `splitBaseName`          | `System.FilePath.splitExtension`      |           |
| `splitPath`              | `System.FilePath.splitPath`           |           |
| `lookupFileInPath`       | `System.Directory.findFileWithSuffix` |           |
| `getFileInPath`          | `System.FilePath.getFileWithSuffix`   |           |

#### IO (previously)

| Old                   | New                                   | New Package |
| --------------------- | ------------------------------------- | ----------- |
| `hWaitForInputOrMsg`  | `Network.Sockets.hWaitForInputOrMsg`  | `ports`     |
| `hWaitForInputsOrMsg` | `Network.Sockets.hWaitForInputsOrMsg` | `ports`     |


### Added Functions

#### Prelude

| New            |
| -------------- |
| `pi`           |
| `(^)`          |
| `(<$>)`        |
| `(<$)`         |

## Finite Map Package

Note that the explicit ordering function is not necessary anymore. Instead, an `Ord` context is needed.

### Renamed Types and Functions

#### Map

| Old                   | New                     | Notes                   |
| --------------------- | ----------------------- | ----------------------- |
| `FM`                  | `Map`                   |                         |
| `emptyFM`             | `empty`                 |                         |
| `unitFM`              | `singleton`             |                         |
| `listToFM`            | `fromList`              |                         |
| `addToFM`             | `insert`                | argument order changed! |
| `addToFM_C`           | `insertWith`            | argument order changed! |
| `addListToFM`         | `insertList`            | argument order changed! |
| `addListToFM_C`       | `insertListWith`        | argument order changed! |
| `delFromFM`           | `delete`                | argument order changed! |
| `delListFromFM`       | `deleteAll`             | argument order changed! |
| `updFM`               | `adjust`                | argument order changed! |
| `splitFM`             |                         | removed, can be user-defined (split lookup is different) |
| `plusFM`              | `union`                 | first two arguments are swapped! |
| `plusFM_C`            | `unionWith`             |                         |
| `minusFM`             | `difference`            |                         |
| `intersectFM`         | `intersection`          |                         |
| `intersectFM_C`       | `intersectionWith`      |                         |
| `foldFM`              | `foldrWithKey`          |                         |
| `mapFM`               | `mapWithKey`            |                         |
| `filterFM`            | `filterWithKey`         |                         |
| `sizeFM`              | `size`                  |                         |
| `eqFM`                |                         | Map has "Eq" context instead |
| `isEmptyFM`           | `null`                  |                         |
| `elemFM`              | `member`                | argument order changed! |
| `lookupFM`            | `lookup`                | argument order changed! |
| `lookupWithDefaultFM` | `findWithDefault`       | argument order changed! |
| `keyOrder`            |                         | removed, because "Ord" context is used now |
| `fmToList`            | `toList`                |                         |
| `keysFM`              | `keys`                  |                         |
| `eltsFM`              | `elems`                 |                         |
| `fmSortBy`            | `sortWithMap`           | Will sort given by "<" of "Ord" context |
| `minFM`               | `lookupMin`             |                         |
| `maxFM`               | `lookupMax`             |                         |
| `fmToListPreOrder`    | `toPreOrderList`        |                         |

#### Set

| Old          | New            |
| ------------ | -------------- |
| `FiniteSet`  | `Set`          |
| `emptySet`   | `empty`        |
| `mkSet`      | `fromList`     |
| `isEmptySet` | `null`         |
| `elementOf`  | `member`       |
| `minusSet`   | `difference`   |
| `setToList`  | `toList`       |
| `union`      | `union`        |

## Directory Package

### Renamed Functions

| Old          | New            |
| ------------ | -------------- |
| `fileSize`   | `getFileSize`  |

## Transformers Package

### Renamed Functions

#### State

| Old            | New                        |
| -------------- | -------------------------- |
| `bindS`        | Monad instance, `(>>=)`    |
| `bindS_`       | Monad instance, `(>>)`     |
| `returnS`      | Monad instance, `return`   |
| `getS`         | `get`                      |
| `putS`         | `put`                      |
| `modifyS`      | `modify`                   |
| `sequenceS`    | `sequence`                 |
| `sequenceS_`   | `sequence_`                |
| `mapS`         | `mapM`                     |
| `mapS_`        | `mapM_`                    |
| `liftS`        | `fmap`                     |
| `liftM`        | `fmap`                     |
| `liftS2`       | `liftM2`                   |

#### ErrorState

| Old            | New                              |
| -------------- | -------------------------------- |
| `returnES`     | Monad instance, `return`         |
| `>+=`          | Monad instance, `(>>=)`          |
| `>+`           | `(>>)`                           |
| `getS`         | `lift . get`                     |
| `putS`         | `lift . put`                     |
| `modifyS`      | `lift . modify`                  |
| `mapS`         | `mapM`                           |
| `<*>,<*,`      | `Applicative instance`           |

### Moved and Renamed Functions

#### ErrorState

| Old            | New                              | New Package |
| -------------- | -------------------------------- | ----------- |
| `concatMapES`  | `Control.Monad.Extra.concatMapM` | `extra`     |
| `mapAccumES`   | `Control.Monad.Extra.mapAccumM`  | `extra`     |

## Socket Package

### Renamed Functions

| Old            | New      |
| -------------- | -------- |
| `sClose`       | `close`  |
| `socketAccept` | `accept` |

## RedBlackTree Package

### Renamed Functions

| Old            | New      |
| -------------- | -------- |
| `tree2list`    | `toList` |
