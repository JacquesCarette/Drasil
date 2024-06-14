**Note:** Everything in this page seems to be obsolete. Will likely get deleted soon.

## The Requirements ##

We have, in our specifications, tables of quantities.
Some of these quantities have units, some don't. Some of these
quantities are indeed unitless, as they are 'raw' numbers.  But we
legitimately want them in the same table.  And it is wrong to have a
'unitless unit'.  In some ways, there are some things which are
'non-dimensional' whose 7-vector of units is all zeroes; these could be
considered as 'unitless' in some sense. But there are also other
quantities that are just numbers (and other things without units) that
belong in these tables.

So we really want to be able to put some quantities together in a
data-structure, while NOT making their units invisible.  The datatype Q
is a non-solution as, while it allows all sorts of quantities to be put
together, it hides the units.

## Solution Design ##
### Problems ###

I had thought of Prisms as a technique to allow us to solve this problem.  It is, and it isn't.  A prism allows you to deconstruct a (generalization of) a sum type, i.e. a set of alternatives. [Note that they might come in quite handy for some of our ASTs like LaTeX and HTML - but that is a digression].
So let's look at MUChunk:

    data MUChunk where --May have Unit chunk
       Has :: (Quantity h, Unit h) => h -> MUChunk
       HasNot :: Quantity c => c -> MUChunk

Great, a sum type!  Nope, not really.  Not as far as Prism is concerned
anyways.  That's because, morally speaking, the data-type above is
equivalent to

    data MUChunk where
       Has :: Unit u => Q -> u -> MUChunk
       HasNot :: Q -> MUChunk

The point here is that: 1. The 'Has' component is a product, and 2.
neither Has nor HasNot can be reconstructed given just a "Maybe USymb".
To have a Prism into a data-structure requires the structure to be
rebuildable from just its payload, which is most definitely not the case
here.

The key to this puzzle is that what we really want to see inside an
MUChunk is a 'Maybe USymb'.  Which we sure do have: the Has gives us a
Just, and HasNot a Nothing.  So an MUChunk really is best seen as being
essentially the same as

    data MUChunk where
       MUC :: Unit u => Q -> Maybe u -> MUChunk

However, that data-type is not really what we want, as it forces us to
split up types (such as FundUnit and DerUChunk) that don't need to be
split.  So the original design of MUChunk is best.  So how do we get there?

### Solution ###

1. Define a new way to see units:

        class Quantity u => Unit' u where
           unit' :: Simple Lens u (Maybe USymb)

2. Define an instance of MUChunk of this different lens:

        instance Unit' MUChunk where
           unit' f (Has    h) = fmap (Has . maybe h (\t -> set unit t h)) (f $ Just $ h^.unit)
           unit' f (HasNot h) = fmap (HasNot . maybe h (\_ -> h)) (f $ Nothing)

And that's it.  That instance was rather difficult to write, even though
the final answer looks small.  The main point is that MUChunk is still a
product, even though it does not necessarily look like it: it is a
product of a Quantity and of a (Maybe Unit) [where I mix types and
constraints in that explanation, on purpose].

So when making such tables, unit' should be called, and Nothing should
of course be printed as "".