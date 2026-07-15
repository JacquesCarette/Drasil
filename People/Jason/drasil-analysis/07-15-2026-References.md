# Analysis of `Reference`s

## What is a `Reference`?

A `Reference` corresponds to one of three distinct concepts (which are captured with `LbLType` constructors):

1. Document-internal cross-references (`RP`), e.g., things that go in `\ref` and `<a href="#..">`.
2. Bibliographic Citations, e.g., `\cite` keys (which are not all that different from (1)!).
3. External Hyperlinks, e.g., `../../../Image.png`.

We use `Reference`s in two ways:

1. As a chunk at times.
2. As a packet of information we pull out from other, "real" chunks that are meant to be rendered in a document and contain their own local-document-referrable labels.

The first way is to encode `URI`s, which we also (1) cheat with (encoding invalid file and http URIs) and (2) insert into the `ChunkDB`. The second way is _sometimes_ inserted into the `ChunkDB` in place of the actual chunks themselves.
