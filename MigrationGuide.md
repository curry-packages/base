# Curry 2 -> 3 Migration Guide

Between version 2 and 3 the standard library has undergone some notable changes, like aligning module and function names more closely with their Haskell equivalents and moving more specialized functionality into separate packages.

This migration guide aims to provide a comprehensive outline of the changes to make the transition of existing packages more seamless.

## Standard Library

### Renamed Modules

| Old         | New                     |
| ----------- | ----------------------- |
| `List`      | `Data.List`             |
| `Maybe`     | `Data.Maybe`            |
| `Either`    | `Data.Either`           |
| `Char`      | `Data.Char`             |

### Moved and Renamed Modules

| Old            | New                           | New Package    |
| -------------- | ----------------------------- | -------------- |
| `FiniteMap`    | `Data.Map`                    | `finite-map`   |
| `Distribution` | `Language.Curry.Distribution` | `distribution` |
| `Directory`    | `System.Directory`            | `directory`    |

### Moved Modules

| Old                     | New Package    |
| ----------------------- | -------------- |
| `Control.Monad.Extra`   | `directory`    |
| `Control.Monad.Trans.*` | `transformers` |

### Deleted Modules

| Old                       |
| ------------------------- |
| `Data.Function.Inversion` |
| `Data.Global`             |

## Finite Map

Note that the explicit ordering function is not necessary anymore. Instead, an `Ord` context is needed.

### Renamed Types and Functions

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
