### Summary of Folder Structure and File Contents

_Last updated: Dec. 4th, 2025_

* `Chunk.hs`: Defines a generic "chunk" wrapper and establishes a Haskll-based criteria for what a chunk is.
* `ChunkDB.hs`: Want to aggregate all your chunks? Use this database.
* `Dump.hs`: Simple tool for dumping all known chunks in a database (grouped by type).
* `UID.hs`: Defines the structure of universally unique identifiers we use for our chunk database.
* `TH.hs`: Defines an automation tool for declaring that a 'chunk type' has 'chunk references'.
* `UIDRef.hs`: `UID`s are great! But _untyped._ Creates a chunk reference data type that carries type information at the Haskell-type-level for type-safe `UID` references! Also carries a data type for treating chunks as being _unityped_.
