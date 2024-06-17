`Maybe` can be used:

1. in the data representation
2. in what the data accessors return

So we'd have `HasX` classy-lenses and `MayHaveX` classy-lenses. We could have instances of `MayHaveX` for all sorts of things where we already know there is no X but where asking the question isn't silly. **We do need to be careful to not implement `MayHaveX` where the question should not be asked.**

From the point of view of our usage, lenses are just polymorphic getters. We want to be able to "get X" from some representation without caring how X is embedded in the data we've been handed, as long as we're promised that X is in there somewhere.
