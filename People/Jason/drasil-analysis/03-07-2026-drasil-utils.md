# Per-File Shallow Analysis

Issues are noted with checkmark lists.

## `drasil-utils`

### `Utils.Drasil`

Re-exports all exports from all other modules in `drasil-utils`.

### `Utils.Drasil.Directory`

Contains a single function: `createDirIfMessing`. This is used to guard against errors thrown in debugging logs when we run `make debug`.

### `Utils.Drasil.Document`

Contains `Doc`-related utilities, with 2 particularly dubious ones:

* [x] `filterEmpty`: Removing "empty" `Doc`s from a list.
* [ ] `listToDoc`: Converts a list of `String`s into a `Doc` of form: `listToDoc [a,b,c,...] == a, b, c, ...`. Why dubious? Because it assumes a specific format, and should likely need an "and" before the last entry. This smells like it should be specialized to `Sentence` (and the [natural] language being generated).

### `Utils.Drasil.English`

Contains two functions:

1. `capitalize :: String -> String`: Capitalizes first character and puts the remaining characters in lowercase.
2. `stringList :: [String] -> String`: Merges a list of items into a comma separated list with an "and" before the last item.

1. [ ] `stringList` could be named better. `commaList`? `commaSepList`? `inlineList`?

### `Utils.Drasil.FileData`

See `.TypeClasses` as well.

Contains a single datatype definition:

```haskell
-- | The underlying data type for auxiliary files in all renderers.
data FileAndContents = FileAndContents {filePath :: FilePath, fileDoc :: Doc}
```

What this looks like is an entry for file generation. What it looks like it really should be: `Map RelativeFilePath Doc`. I'm sure there's more that can be done here, too.

### `Utils.Drasil.FileIO`

Contains a single function: `createFile` that accepts a filepath and `String`, and writes the `String` to said filepath. `String` is likely not the type we want here. We use a lot of `render`s (`Doc -> String`) around our `createFile` usage sites. Perhaps having the type signature as `FilePath -> Doc -> IO ()` would be better.

### `Utils.Drasil.FilePath`

Contains an encoding for a `RelativeFile`. This type is good and should be used more throughout Drasil for file generation.

This file along with `FileData`, `FileIO`, `TypeClasses`, and `Document` should be moved to a new package that all other packages import for anything related to artifact generation. e.g., `drasil-artifacts`.

### `Utils.Drasil.Lists`

Contains a number of functions that operate on lists:

1. `replaceAll`: Replaces things any occurrence of entries in a list with a specific value in a list of values. Exclusively used for `toPlainName` in `Utils.Drasil.Strings`.
2. `subsetOf`: Does as the name suggests. Dubious because it operates on lists. It has a single use-case that looks like it should be operating on `Set`s rather than lists.
3. `nubSort`: Similar to (2), dubious in that its usage sites all look like they should be using `Set`s rather than lists.
4. `weave`: A function that interweaves "two" (but supports arbitrary number of lists interweaving) together.
5. `foldle` and `foldle1`: These are just fold functions that operate on lists. They look like they can be generalized further (similar to functions in `Data.Foldable`), but is not likely worth the effort.
6. `toColumn`: Converts a row "vector" (a list) to a column "vector" (a list).
7. `mkTable`: Creates a table (`[[b]]`) of data by projecting (or calculating new data) based on list of elements (`a`) and a list of calculation functions (`a -> b`).

1. [ ] `subsetOf` and `nubSort` usage sites should be reevaluated for types used. These look like they should be deleted.
2. [x] `weave` can (should) be simplified to:
  ```haskell
  weave2 :: [a] -> [a] -> [a]
  weave2 [] ys = ys
  weave2 (x:xs) ys = x : weave2 ys xs
  ```

### `Utils.Drasil.Strings`

Contains two functions:

1. `toPlainName`: Replaces "special" characters (``,~`-=!@#$%^&*+[]\\;'/|\"<>? ``) with underscores in a `String`. `toASCII` might be more appropriate of a name if the function were complete.
2. `repUnd`: Replaces underscores in a name with periods. This is exclusively used for document reference links and prepending abbreviations. Both very dubious use-cases.

1. [ ] Rename `toPlainName` to `toASCII` and make complete.
2. [ ] Investigate `repUnd`'s use.

### `Utils.Drasil.TypeClasses`

Contains a single typeclass:

```haskell
class HasPathAndDoc a b | a -> b where
  getPath :: a -> FilePath
  getDoc :: a -> b
```

It has a single instantiation (below) that looks like it could be simplified by replacing `b` in the typeclass with `Doc` (as it appears to have a single use-case).

```haskell
instance HasPathAndDoc FileData Doc where
  getPath = filePath
  getDoc = modDoc . fileMod
```

Note that this single instantiation is also in `drasil-utils`. There is some good design work that can be done here to capture what this typeclass and data type (`FileData`) try to capture.
