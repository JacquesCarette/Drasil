The name _nugget_ is not necessarily meant to stick, or even to mean something small.

The idea is that we need to declare that some knowledge is _atomic_. In other words, while it may have internal structure, we largely ignore it, and consider it as an indivisible whole. The clearest example is the notion of _term_: it is purely nominal (i.e. is _has_ a name, but that's it). The natural analogy is with _natural language_ (such as English): the words of English are a convention that we agree with as speakers of the language, and the meaning associated to each word is _also_ an agreed-upon convention. [Aside: yes, those conventions shift over time; let's ignore that for now.] (TODO: find references)

So we're going to do the exact same thing, and adopt a convention about various things. In particular, we'll simply declare that some knowledge is _atomic_. Not because we're **right** in any sensible way, but because that convention is *useful* for us to get things done. That also tells us when to change a convention: when it gets in the way of getting work done (in either sensible or efficient / effective ways).

How do we identify nuggets? Basically by spotting that some abstract notion occurs a lot in our work, and that we need to process it in uniform ways. Another common way of spotting them is inside _collections of knowledge_ (which we sometimes encode as _chunks_ and sometimes as _theories_). _Nuggets_ are the atoms that make up small molecules (chunks) and also larger organic molecules (theories).

Let's do some kind of rational reconstruction of why chunks and theory kinds helped: not only are certain kinds of knowledge collection important for us, but we need to be able to write quite specific **transformers** (interpreters, compilers, symbol shufflers, etc) between one representation and another. Without some extra information, those transformers couldn't do their jobs at all. So, in a sense, our knowledge collections are **typed** and our transformers are too.

As another analogy, JSON/XML/YAML are all essentially untyped (in their syntax) and very weakly typed “contextual trees” (tags have meaning in-context only and not in the absolute).

So this gives us yet another way to spot important collections, as well as nuggets: we know what output we want (our softifacts) and we do have a collection of transformers that produce those outputs. How do we get the 'right' inputs to those transformers? [Note that we should consider our transformers' input API to be changeable.] This march backwards from softifacts to _the data that humans **must** provide_ is particularly relevant.

Aside: yes, processors themselves could be nuggets or information collections. We could then write a higher-level processor for a (encoded-processor, encoded-input) pair, i.e. what is usually called an _interpreter_.

So, what next? We need to take yet another look at our examples' associated softifacts, and again ask "what is this?" while pointing to every single piece.

---

Stuff to add in. I asked for some leads on references on Mastodon, and I got:
- some terms that might help you find what you're looking for: usage-based, cognitive-functional, (trans)languaging, linguistic repertoire
- look into studies around the bouba/kiki effect 
- On language as a convention, I can recommend Millikan (1998) "Language conventions made simple"
- David Lewis, "Conventions: A Philosophical Study"
- Lakoff and Johnson, [Metaphors we live by](https://en.wikipedia.org/wiki/Metaphors_We_Live_By)
