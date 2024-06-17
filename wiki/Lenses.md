Perhaps a more programming-oriented way of thinking about [chunks](Chunks) is to view them as [lenses](https://hackage.haskell.org/package/lens). In functional programming (and Haskell specifically), [lenses](https://stackoverflow.com/questions/8307370/functional-lenses) are a popular method of getting and setting information in a [record type](http://learnyouahaskell.com/making-our-own-types-and-typeclasses). Although complex, these are used to make programming much easier and concise, as developers and users alike should not need to fiddle around with the record type syntax more than necessary. Instead, this provides an easy way of reading and writing information to chunks (which are all record types). Here is a great summary of lenses to get started: [Basic Lensing](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/basic-lensing).

## Defining Lenses
So how do we actually make a lens? Most of the time, the `Control.Lens` package does all of the hard work for you! We first start with our record type, for simplicity, we will use something like:
```Haskell
data MyFirstProgram = FirstProgram {
    _hello :: String,
    _world :: String
}
makeLenses ''MyFirstProgram
```
Defining this type so we can use it in a variable looks something like this:
```Haskell
myFirstVariable :: MyFirstProgram
myFirstVariable = FirstProgram {_hello = "First field", _world = "Second field"}
```
Of course, there are other ways to define record types in Haskell (Drasil does use those too), but this is one of the most straightforward methods. The only problem with this is code can become easily cluttered and messy very quickly. It doesn't look too bad right now, but sometimes we see record types that have a lot more than two fields. When those show up, field names and all the syntactic clutter become very difficult to read. This is especially true when we only want to change one field at a time. So, we use lenses to help 'see' through the fields and access the information we want, with minimal hassle.

You may also be wondering why there are underscores before each field declarations. Well, the `Control.Lens` package automatically creates lenses only for fields that begin with an underscore. The names of those lenses (used for setters and getters - we'll get to those in a minute) will be exactly the same as those used for the fields, except without the underscore. This way, the process of using lenses is still straightforward, and the user will be able to tell if something funky is going on just by checking if there is an underscore in front of a record field.

Haskell allows programmers to access a record field just by calling the field on the variable. For example,
```Haskell
_hello myFirstVariable
```
will return `"First field"`. In a sense, these are already similar to getters, just not as pretty. Since we are making the lenses already, we should still use getters over direct field functions whenever possible to keep the style of the code consistent.

The `makeLenses''` function is really what does the heavy lifting. Calling this after a data type will tell the compiler to make lenses as described above, and users can just treat them like regular functions.

## Getters
To users, getter lenses are probably one of the most useful tools when dealing with record-style syntax in data types. Getters are what we use to extract information from a record data type. Most often, these types of lenses are used with [`view`](https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-Getter.html#v:view) and [`(^.)`](https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-Getter.html#v:-94-.) functions (see [below](#lenses-in-drasil) for an example). They are used to extract information from a given field of a record type. The name of a lens often corresponds to the name of the field, without the underscores. Getters are extremely useful in Typeclass instance declarations, since record field functions don't really play nicely here. To get some required piece of information from a record field, just call `view fieldNameWithoutUnderscore recordType`. Or, to use the inline version, do `recordType ^. fieldNameWithoutUnderscore`. Or consider a more concrete example (where `>>>` denotes the output):
```Haskell
view hello myFirstVariable
>>> "First field"

myFirstVariable ^. hello
>>> "First field"

_hello myFirstVariable
>>> "First field"
```
So these all essentially mean the same thing, but the first two fit the rest of the lens style and are a lot more powerful. For more information on getters, visit the [Control.Lens.Getter](https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-Getter.html) package.

## Setters
While developers of Drasil may not see or use these very often, they are one of the best aspects of using lenses. When we want to set something, we essentially just want to modify the field of a record. We also deal with fairly large record types regularly in Drasil (over 10 fields normally), so having a method to easily modify a single record field without pattern matching on the full record becomes much more important. Setters are basically the opposite of getters, but they still use very similar syntax.

Without lenses, setting a record field would have to look something like this:
```Haskell
mySecondVariable :: MyFirstProgram
mySecondVariable = myFirstVariable{_hello = "First field but different"}
```
But if we use a setter, this becomes:
```Haskell
mySecondVariable :: MyFirstProgram
mySecondVariable = set hello "First field but different" myFirstVariable
```
Which reads a lot clearer. Again, it seems almost pointless to learn a whole new method of using record types, but setters **really** help when dealing with larger record types. Now, if we call our getter on `mySecondVariable`, we receive the following:
```Haskell
view hello mySecondVariable
>>> "First field but different"

view world mySecondVariable
>>> "Second field"
```
For more information on setters, visit the [Control.Lens.Setter](https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-Setter.html) package.

## Lenses in Drasil

In Drasil, it is especially useful to have the getter functionality for use in retrieving UIDs, terms, symbols, and units inside of larger chunk types. Since each chunk type builds upon other chunk types, accessors still need to be able to fetch the wanted information inside the idea. Perhaps instead of an opaque wrapper, a chunk (with the power of lenses) would be better described as objects wrapped in glass boxes with doors. A lens is pretty much a door into any one of the glass boxes. Thankfully, lenses use the record names when attempting to observe information, so an accurate intuition would be that every box has its door and a nametag. All the information is easily viewable from the largest wrapper, yet the observer can make sure all the proper types are grouped together. For example, we do not want to be adding [`UIDs`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UID) for every wrapper we use on an idea, nor do we want to add [`NamedChunks`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:NamedChunk) at the level of a [`QuantityDict`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:QuantityDict). Instead, we keep the `UID` encased in its lower-level chunk and instead associate that `UID` to the now-complex chunk type.

Users of Drasil might not even need to interact with concepts of lensing and chunks in a more complex way, so giving an intuition like the glass boxes or wrappers may help. Most chunks are created using smart constructors, meaning a user will almost never have to interact with setter lenses. This allows the user to input all the information needed for that chunk without actually needing to know the specifics of how each chunk is made. Drasil automatically makes the lower-level chunks and wraps them as needed for higher-level ones. And lenses help in this respect as well. By combining lenses with Haskell class methods, any chunk type can easily display any information in any box, so long as the user gives the correct label for Drasil to look in to. For example, a reference in Drasil can be defined as the following:
```Haskell
data Reference = Reference
  { _ui :: UID -- unique identifier
  ,  ra :: LblType -- reference address
  ,  sn :: ShortName -- display name
  }
```
and created using the [`ref`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#v:ref) constructor (applied with the proper arguments). Then, using lenses, retrieving information like the [`UID`](https://jacquescarette.github.io/Drasil/docs/full/drasil-lang-0.1.60.0/Language-Drasil.html#t:UID) is as simple as calling:
```Haskell
yourReference :: UID
yourReferenceUID = yourReference ^. uid
```
So the only lensing the user gets to do is the getter function (`^.`), which is much better than manually pattern matching on a huge amount of possible record types.
