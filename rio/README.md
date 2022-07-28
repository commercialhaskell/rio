# The rio library

*A standard library for Haskell*

![Rio](https://camo.githubusercontent.com/fc162fb0024699c85f00eae769085a5fe528153e/68747470733a2f2f7777772e61687374617469632e636f6d2f70686f746f732f636974792f76692d76363837315f30305f31343030783434322e6a7067)

![Tests](https://github.com/commercialhaskell/rio/workflows/Tests/badge.svg)

The goal of the `rio` library is to make it easier to adopt Haskell
for writing production software.  It is intended as a cross between:

* Collection of well-designed, trusted libraries
* Useful `Prelude` replacement
* A set of best practices for writing production quality Haskell code

This repository contains the `rio` library and other related
libraries, such as `rio-orphans`. There is a [tutorial on how to use
`rio`](https://haskell.fpcomplete.com/library/rio) available on FP
Complete's Haskell site. This README discusses project goals and
collects other reference information.

## Standard library

While GHC ships with a `base` library, as well as a number of other
common packages like `directory` and `transformers`, there are large
gaps in functionality provided by these libraries. This choice for a
more minimalistic `base` is by design, but it leads to some
unfortunate consequences:

* For a given task, it's often unclear which is the right library to
  use
* When writing libraries, there is often concern about adding
  dependencies to any libraries outside of `base`, due to creating a
  heavier dependency footprint
* By avoiding adding dependencies, many libraries end up
  reimplementing the same functionality, often with incompatible types
  and type classes, leading to difficulty using libraries together

This library attempts to define a standard library for Haskell. One
immediate response may be [XKCD #927](https://xkcd.com/927/):

![XKCD Standards](https://imgs.xkcd.com/comics/standards.png)

To counter that effect, this library takes a specific approach: __it
reuses existing, commonly used libraries__. Instead of defining an
incompatible `Map` type, for instance, we standardize on the commonly
used one from the `containers` library and reexport it from this
library.

This library attempts to define a set of libraries as "standard,"
meaning they are recommended for use, and should be encouraged as
dependencies for other libraries. It does this by depending on these
libraries itself, and reexporting their types and functions for easy
use.

Beyond the ecosystem effects we hope to achieve, this will hopefully
make the user story much easier. For a new user or team trying to get
started, there is an easy library to depend upon for a large
percentage of common functionality.

See the dependencies of this package to see the list of packages
considered standard. The primary interfaces of each of these packages
is exposed from this library via a `RIO.`-prefixed module reexporting
its interface.

## Prelude replacement

The `RIO` module works as a prelude replacement, providing more
functionality and types out of the box than the standard prelude (such
as common data types like `ByteString` and `Text`), as well as
removing common "gotchas", like partial functions and lazy I/O. The
guiding principle here is:

* If something is safe to use in general and has no expected naming
  conflicts, expose it from `RIO`
* If something should not always be used, or has naming conflicts,
  expose it from another module in the `RIO.` hierarchy.

## Best practices

Below is a set of best practices we recommend following. You're
obviously free to take any, all, or none of this. Over time, these
will probably develop into much more extensive docs. Some of these
design decisions will be catered to by choices in the `rio` library.

For Haskellers looking for a set of best practices to follow: you've
come to the right place!

### Import practices

This library is intended to provide a fully loaded set of basic
functionality. You should:

* Enable the `NoImplicitPrelude` language extension (see below)
* Add `import RIO` as your replacement prelude in all modules
* Use the `RIO.`-prefixed modules as necessary, imported using the
  recommended qualified names in the modules themselves. For example,
  `import qualified RIO.ByteString as B`. See the module documentation
  for more information.
* Infix operators may be imported unqualified, with a separate import
  line if necessary. For example, `import RIO.Map ((?!), (\\))`. Do
  this only if your module contains no overlapping infix names,
  regardless of qualification. For instance, if you are importing both
  `RIO.Map.\\` and `RIO.List.\\` do not import either one unqualified.

In the future, we may have editor integration or external tooling to
help with import management.

### Language extensions

Very few projects these days use bare-bones Haskell 98
or 2010. Instead, almost all codebases enable some set of additional
language extensions. Below is a list of extensions we recommend as a
good default, in that these are:

* Well accepted in the community
* Cause little to no code breakage versus leaving them off
* Are generally considered safe

Our recommended defaults are:

```
AutoDeriveTypeable
BangPatterns
BinaryLiterals
ConstraintKinds
DataKinds
DefaultSignatures
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveTraversable
DoAndIfThenElse
EmptyDataDecls
ExistentialQuantification
FlexibleContexts
FlexibleInstances
FunctionalDependencies
GADTs
GeneralizedNewtypeDeriving
InstanceSigs
KindSignatures
LambdaCase
MonadFailDesugaring
MultiParamTypeClasses
MultiWayIf
NamedFieldPuns
NoImplicitPrelude
OverloadedStrings
PartialTypeSignatures
PatternGuards
PolyKinds
RankNTypes
RecordWildCards
ScopedTypeVariables
StandaloneDeriving
TupleSections
TypeFamilies
TypeSynonymInstances
ViewPatterns
```

Notes on some surprising choices:

* `RecordWildCards` is really up for debate. It's widely used, but
  rightfully considered by many to be dangerous. Open question about
  what we do with it.
* Despite the fact that `OverloadedStrings` can break existing code,
  we recommend its usage to encourage avoidance of the `String` data
  type. Also, for new code, the risk of breakage is much lower.
* `MonadFailDesugaring` helps prevent partial pattern matches in your
  code, see [#85](https://github.com/commercialhaskell/rio/issues/85)

Due to concerns about tooling usage (see [issue
#9](https://github.com/commercialhaskell/rio/issues/9)), we recommend
adding these extensions on-demand in your individual source modules
instead of including them in your `package.yaml` or `.cabal` files.

There are other language extensions which are perfectly fine to use as
well, but are not recommended to be turned on by default:

```
CPP
TemplateHaskell
ForeignFunctionInterface
MagicHash
UnliftedFFITypes
TypeOperators
UnboxedTuples
PackageImports
QuasiQuotes
DeriveAnyClass
DeriveLift
StaticPointers
```

### GHC Options

We recommend using these GHC compiler warning flags on all projects, to catch
problems that might otherwise go overlooked:

* `-Wall`
* `-Wcompat`
* `-Widentities`
* `-Wincomplete-record-updates`
* `-Wincomplete-uni-patterns`
* `-Wpartial-fields`
* `-Wredundant-constraints`

You may add them per file, or to your `package.yaml`, or pass them on
the command line when running ghc. We include these in the project
template's `package.yaml` file.

For code targeting production use, you should also use the flag that turns all
warnings into errors, to force you to resolve the warnings before you ship your
code:

* `-Werror`

Further reading:

* Alexis King explains why these are a good idea in [her blog
post](https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/)
which was the original inspiration for this section.
* Max Tagher gives an in-depth overview of these flags, and more,
[in his blog post](https://medium.com/mercury-bank/enable-all-the-warnings-a0517bc081c3).

### Monads

A primary design choice you'll need to make in your code is how to
structure your monads. There are many options out there, with various
trade-offs. Instead of going through all of the debates, we're going
to point to
[an existing blog post](https://www.fpcomplete.com/blog/2017/07/the-rio-monad),
and here just give recommendations.

* If your code is going to perform I/O: it should live in the `RIO`
  monad. `RIO` is "reader IO." It's the same as `ReaderT env IO`, but
  includes some helper functions in this library and leads to nicer
  type signatures and error messages.

* If you need to provide access to specific data to a function, do it
  via a typeclass constraint on the `env`, _not_ via a concrete
  env. For example, this is bad:

  ```haskell
  myFunction :: RIO Config Foo
  ```

  This is good:

  ```haskell
  class HasConfig env where
    configL :: Lens' env Config -- more on this in a moment
  myFunction :: HasConfig env => RIO env Foo
  ```

  Reason: by using typeclass constraints on the environment, we can
  easily compose multiple functions together and collect up the
  constraints, which wouldn't be possible with concrete
  environments. We _could_ go more general with mtl-style typeclasses,
  like `MonadReader` or `MonadHasConfig`, but `RIO` is a perfect
  balance point in the composability/concreteness space (see blog post
  above for more details).

* When defining `Has`-style typeclasses for the environments, we use
  lenses (which are exposed by `RIO`) because it provides for easy
  composability. We also leverage superclasses wherever possible. As
  an example of how this works in practice:

  ```haskell
  -- Defined in RIO.Logger
  class HasLogFunc env where
    logFuncL :: Lens' env LogFunc

  class HasConfig env where
    configL :: Lens' env Config
  instance HasConfig Config where
    configL = id

  data Env = Env { envLogFunc :: !LogFunc, envConfig :: !Config }
  class (HasLogFunc env, HasConfig env) => HasEnv env where
    envL :: Lens' env Env
  instance HasLogFunc Env where
    logFuncL = lens envLogFunc (\x y -> x { envLogFunc = y })
  instance HasConfig Env where
    configL = lens envConfig (\x y -> x { envConfig = y })
  instance HasEnv Env where
    envL = id

  -- And then, at some other part of the code
  data SuperEnv = SuperEnv { seEnv :: !Env, seOtherStuff :: !OtherStuff }
  instance HasLogFunc SuperEnv where
    logFuncL = envL.logFuncL
  instance HasConfig SuperEnv where
    configL = envL.configL
  instance HasEnv SuperEnv where
    envL = lens seEnv (\x y -> x { seEnv = y })
  ```

* If you're writing code that you want to be usable outside of `RIO`
  for some reason, you should stick to the good mtl-style typeclasses:
  `MonadReader`, `MonadIO`, `MonadUnliftIO`, `MonadThrow`, and
  `PrimMonad`. It's better to use `MonadReader`+`Has` than to create
  new typeclasses like `MonadLogger`, though usually just sticking
  with the simpler `RIO env` is fine (and can easily be converted to
  the more general form with `liftRIO`). You should avoid using the
  following typeclasses (intentionally not exposed from this library):
  `MonadBase`, `MonadBaseControl`, `MonadCatch`, and `MonadMask`.

### Exceptions

For in-depth discussion, see [safe exception
handling](https://haskell.fpcomplete.com/tutorial/exceptions). The
basic idea is:

* If something can fail, and you want people to deal with that failure
  every time (e.g., `lookup`), then return a `Maybe` or `Either`
  value.
* If the user will usually not want to deal with it, then use
  exceptions. In the case of pure code, use a `MonadThrow`
  constraint. In the case of `IO` code: use runtime exceptions via
  `throwIO` (works in the `RIO` monad too).
* You'll be upset and frustrated that you don't know exactly how some
  `IO` action can fail. Accept that pain, live with it, internalize
  it, use `tryAny`, and move on. It's the price we pay for async
  exceptions.
* Do all resource allocations with functions like `bracket` and
  `finally`.

It’s a good idea to define an app-wide exception type:

```haskell
data AppExceptions
  = NetworkChangeError Text
  | FilePathError FilePath
  | ImpossibleError
  deriving (Typeable)

instance Exception AppExceptions

instance Show AppExceptions where
  show =
    \case
      NetworkChangeError err -> "network error: " <> (unpack err)
      FilePathError fp -> "error accessing filepath at: " <> fp
      ImpossibleError -> "this codepath should never have been executed. Please report a bug."
```

### Strict data fields

Make data fields strict by default, unless you have a good reason to
do otherwise.

### Project template

We provide a project template which sets up lots of things for you out
of the box. You can use it by running:

```
$ stack new projectname rio
```

### Safety first

This library intentionally puts safety first, and therefore avoids
promoting partial functions and lazy I/O. If you think you need lazy
I/O: you need a streaming data library like conduit instead.

### When to generalize

A common question in Haskell code is when should you generalize. Here
are some simple guidelines. For parametric polymorphism: _almost
always_ generalize, it makes your type signatures more informative and
functions more useful. In other words, `reverse :: [a] -> [a]` is far
better than `reverse :: [Int] -> [Int]`.

When it comes to typeclasses: the story is more nuanced. For
typeclasses provided by `RIO`, like `Foldable` or `Traversable`, it's
generally a good thing to generalize to them when possible. The real
question is defining your own typeclasses. As a general rule: avoid
doing so as long as possible. And _if_ you define a typeclass: make
sure its usage can't lead to accidental bugs by allowing you to swap
in types you didn't expect.

### Module hierarchy

The `RIO.Prelude.` module hierarchy contains identifiers which are reexported
by the `RIO` module. The reason for this is to make it easier to view the
generated Haddocks. The `RIO` module itself is intended to be imported
unqualified, with `NoImplicitPrelude` enabled. All other modules are _not_
reexported by the `RIO` module,
and will document inside of them whether they should be imported qualified or
unqualified.
