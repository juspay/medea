# Introduction

This document describes a set of standards for all code for the Haskell
implementation of Medea. It also explains our reasons for these choices, and 
acts as a living document to document our practices for current and future 
contributors to the project. We intend for this document to evolve as our 
needs change, as well as act as a single point of truth for standards.

# Motivation

The desired outcomes from the prescriptions in this document are as follows.

## Increase consistency

Inconsistency is worse than _any_ standard, as it requires us to track a large
amount of case-specific information. Software development is already a difficult
task due to the inherent complexities of the problems we seek to solve, as well
as the inherent complexities foisted upon us by _decades_ of bad historical
choices we have no control over. For newcomers to a project and old hands alike,
increased inconsistency translates to developmental friction, resulting in
wasted time, frustration and ultimately, worse outcomes for the code in
question.

To avoid putting ourselves into this boat, both currently and in the future, we
must strive to be _automatically consistent_. Similar things should look
similar; different things should look different; as much as possible, we must
pick some rules _and stick to them_; and this has to be clear, explicit and
well-motivated. This will ultimately benefit us, in both the short and the long
term. The standards described here, as well as this document itself, is written
with this foremost in mind.

## Limit non-local information

There is a limited amount of space in a developer's skull; we all have bad days,
and we forget things or make decisions that, perhaps, may not be ideal at the
time. Therefore, limiting cognitive load is good for us, as it reduces the
amount of trouble we can inflict due to said skull limitations. One of the worst
contributors to cognitive load (after inconsistency) is _non-local information_
- the requirement to have some understanding beyond the scope of the current
unit of work. That unit of work can be a data type, a module, or even a whole
project; in all cases, the more non-local information we require ourselves to
hold in our minds, the less space that leaves for actually doing the task at
hand, and the more errors we will introduce as a consequence.

Thus, we must limit the need for non-local information at all possible levels.
'Magic' of any sort must be avoided; as much locality as possible must be
present everywhere; needless duplication of effort or result must be avoided.
Thus, our work must be broken down into discrete, minimal, logical units, which
can be analyzed, worked on, reviewed and tested in as much isolation as
possible. This also applies to our external dependencies.

Thus, many of the decisions described here are oriented around limiting the
amount of non-local knowledge required at all levels of the codebase.
Additionally, we aim to avoid doing things 'just because we can' in a way that
would be difficult for other Haskellers to follow, regardless of skill level.

## Minimize impact of legacy

Haskell is a language that is older than some of the people currently writing
it; parts of its ecosystem are not exempt from it. With age comes legacy, and
much of it is based on historical decisions which we now know to be problematic
or wrong. We can't avoid our history, but we can minimize its impact on our
current work. 

Thus, we aim to codify good practices in this document _as seen today_. We also
try to avoid obvious 'sharp edges' by proscribing them away in a principled,
consistent and justifiable manner. 

## Automate away drudgery

As developers, we should use our tools to make ourselves as productive as
possible. There is no reason for us to do a task if a machine could do it for
us, especially when this task is something boring or repetitive. We love Haskell
as a language not least of all for its capability to abstract, to describe, and
to make fun what other languages make dull or impossible; likewise, our work
must do the same.

Many of the tool-related proscriptions and requirements in this document are
driven by a desire to remove boring, repetitive tasks that don't need a human to
perform. By removing the need for us to think about such things, we can focus on
those things which _do_ need a human; thus, we get more done, quicker.

# Conventions

The words MUST, SHOULD, MUST NOT, SHOULD NOT and MAY are defined as per [RFC
2119][rfc-2119].

# Tools

## Compiler warning settings

The following warnings MUST be enabled for all builds of any project, or any
project component:

* ``-Wall``
* ``-Wcompat``
* ``-Wincomplete-record-updates``
* ``-Wincomplete-uni-patterns``
* ``-Wredundant-constraints``
* ``-Werror``

### Justification

These options are suggested by [Alexis King][alexis-king-options] - the
justifications for them can be found at the link. These fit well with our
motivations, and thus, should be used everywhere. The ``-Werror`` ensures that
warnings _cannot_ be ignored: this means that problems get fixed sooner.

## Linting

Every Haskell source file MUST be free of warnings as produced by [HLint][hlint], 
with default settings.

### Justification

HLint automates away the detection of many common sources of boilerplate and
inefficiency. It also describes many useful refactors, which in many cases make
the code easier to read and understand. As this is fully automatic, it saves
effort on our part, and ensures consistency across the codebase without us
having to think about it.

## Code formatting

Every Haskell source file MUST be formatted according to [Ormolu][ormolu], with 
default settings. Each Haskell source code line MUST be at most 100 characters 
wide, and SHOULD be at most 80 characters wide.

Cabal files MUST be formatted according to [cabal-fmt][cabal-fmt], with default
settings. 

### Justification

Consistency is the most important goal of readable codebases. Having a single
standard, automatically enforced, means that we can be sure that everything will
look similar, and not have to spend time or mind-space ensuring that our code
complies. Additionally, as Ormolu is opinionated, anyone familiar with its
layout will find our code familiar, which eases the learning curve. Similar, and
often analogous, arguments apply to cabal-fmt.

Lines wider than 80 characters become difficult to read, especially when viewed
on a split screen. Sometimes, we can't avoid longer lines (especially with more
descriptive identifiers), but a line length of over 100 characters becomes
difficult to read even without a split screen. We don't _enforce_ a maximum of
80 characters for this exact reason; some judgment is allowed.

# Code practices

## Naming

camelCase MUST be used for all non-type, non-data-constructor names; otherwise,
TitleCase MUST be used. Acronyms used as part of a naming identifier (such as 
'JSON', 'API', etc) SHOULD be downcased; thus ``repairJson`` and
``fromHttpService`` are correct. Exceptions are allowed for external libraries
(Aeson's ``parseJSON`` for example).

### Justification

camelCase for non-type, non-data-constructor names is a long-standing convention
in Haskell (in fact, HLint checks for it); TitleCase for type names or data
constructors is _mandatory_. Obeying such conventions reduces cognitive load, as
it is common practice among the entire Haskell ecosystem. There is no particular
standard regarding acronym casing: examples of always upcasing exist (Aeson) as
well as examples of downcasing (``http-api-data``). One choice for consistency
(or as much as is possible) should be made however.

## Modules

All publically facing modules (namely, those which are not listed in
``other-modules`` in the Cabal file) MUST have explicit export lists.

All modules MUST use one of the following conventions for imports:

* ``import Foo (Baz, Bar, quux)``
* ``import qualified Foo as F``

Data types from qualified-imported modules SHOULD be imported unqualified by
themselves:

```haskell
import Data.Vector (Vector)
import qualified Data.Vector as V
```

The main exception is if such an import would cause a name clash:

```haskell
-- no way to import both of these without clashing the Vector type name
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
```

The _sole_ exception is a 'hiding import' to replace part of the functionality
of ``Prelude``:

```haskell
-- replace the String-based readFile with a Text-based one
import Prelude hiding (readFile)
import Data.Text.IO (readFile)
```

### Justification

Explicit export lists are an immediate, clear and obvious indication of what
publically visible interface a module provides. It gives us stability guarantees
(namely, we know we can change things that aren't exported and not break
downstream code at compile time), and tells us where to go looking first when
inspecting or learning the module. Additionally, it means there is less chance
that implementation details 'leak' out of the module due to errors on the part
of developers, especially new developers.

One of the biggest challenges for modules which depend on other modules
(especially ones that come from the project, rather than an external library) is
knowing where a given identifier's definition can be found. Having explicit
imports of the form described helps make this search as straightforward as
possible. This also limits cognitive load when examining the sources (if we
don't import something, we don't need to care about it in general). Lastly,
being explicit avoids stealing too many useful names.

In general, type names occur far more often in code than function calls: we have
to use a type name every time we write a type signature, but it's unlikely we
use only one function that operates on said type. Thus, we want to reduce the
amount of extra noise needed to write a type name if possible. Additionally,
name clashes from function names are far more likely than name clashes from type
names: consider the number of types on which a ``size`` function makes sense.
Thus, importing type names unqualified, even if the rest of the module is
qualified, is good practice, and saves on a lot of prefixing.

## LANGUAGE pragmata

Any pragmata governing GHC extensions MUST be enabled at the source file level. 
All language pragmata MUST be at the top of the source file, written as 
``{-# LANGUAGE PragmaName #-}``.

### Justification

Some language extensions provided by GHC have considerable impact, often
allowing us to write Haskell in ways that wouldn't otherwise be possible.
This often changes syntax, sometimes in non-trivial ways, creating 'spooky
action at a distance' if done in a global fashion (such as in the Cabal file).
By having any extensions declared per-file, and immediately at that, gives a
good reminder to the reader what to expect.

## Prelude

The default ``Prelude`` from ``base`` MUST be used. A 'hiding import' to remove
functionality we want to replace SHOULD be used when necessary.

### Justification

Haskell is a 30-year-old language, and the ``Prelude`` is one of its biggest
sources of legacy. A lot of its defaults are questionable at best, and often
need replacing. As a consequence of this, a range of 'better ``Prelude``s' have
been written, with a range of opinions: while there is a common core, a large
number of decisions are opinionated in ways more appropriate to the authors of
said alternatives and their needs than those of other users of said
alternatives. This means that, when a non-``base`` ``Prelude`` is in scope, it
often requires familiarity with its specific decisions, in addition to whatever
cognitive load the current module and its other imports impose.

While there are _many_ reasoned arguments for any given choice of alternative
``Prelude``, and even arguments for a custom ``Prelude`` for our own use,
ultimately _every_ Haskell developer is familiar with the ``Prelude`` from
``base``. Additionally, local replacements to the defaults from the ``Prelude``
from ``base`` require only local knowledge; a global replacement, whether with
an alternative or with something of our own design, require more non-local
information, which increases cognitive load. Lastly, it is rare that _every_
legacy decision from the ``Prelude`` in ``base`` is relevant in any given
module; specifying _which_ legacy decision is being replaced by targeted hides
and imports actually _helps_ understanding, at relatively low cost. Lastly, an
alternative ``Prelude`` serves as a _massive_ number of unqualified imports,
which we avoid for reasons described previously: use of one would smuggle all
such problems in via the back door. Lastly, the dependency footprint of many
alternative ``Prelude``s is _highly_ non-trivial; it isn't clear if we need all
of this in our dependency tree.

For all the above reasons, the best choice is 'default with local replacements'.

## Other

Lists SHOULD NOT be field values of types; this extends to ``String``s. Instead,
``Vector``s (``Text``s) SHOULD be used, unless a more appropriate structure exists.

[Boolean blindness][boolean-blindness] SHOULD NOT be used in the design of any
function or API. Returning more meaningful data SHOULD be the preferred choice.
The general principle of ['parse, don't validate'][parse-dont-validate] SHOULD
guide design and implementation.

Partial functions MUST NOT be defined. Partial functions SHOULD NOT be used
except to ensure that another function is total (and the type system cannot be
used to prove it). 

Derivations MUST use an explicit [strategy][deriving-strategies]. Thus, the 
following is wrong:

```haskell
newtype Foo = Foo (Bar Int)
    deriving (Eq, Show, Generic, FromJSON, ToJSON, Data, Typeable)
```

Instead, write it like this:

```haskell
newtype Foo = Foo (Bar Int)
    deriving stock (Generic, Data, Typeable)
    deriving newtype (Eq, Show)
    deriving anyclass (FromJSON, ToJSON)
```

### Justification

Haskell lists are a large example of the legacy of the language: they (in the
form of singly linked lists) have played an important role in the development of
functional programming (and for some 'functional' languages, continue to do so).
However, from the perspective of data structures, they are suboptimal except for
_extremely_ specific use cases. In almost any situation involving data (rather
than control flow), an alternative, better structure exists. Although it is both
acceptable and efficient to use lists within functions (due to GHC's extensive
fusion optimizations), from the point of view of field values, they are a poor
choice from both an efficiency perspective, both in theory _and_ in practice.
For almost all cases where you would want a list field value, a ``Vector`` field
value is more appropriate, and in almost all others, some other structure (such
as a ``Map``) is even better.

The [description of boolean blindness][boolean-blindness] gives specific reasons why it is a poor
design choice; additionally, it runs counter to the principle of ['parse, don't
validate][parse-dont-validate]. While sometimes unavoidable, in many cases, it's
possible to give back a more meaningful response than 'yes' or 'no, and we
should endeavour to do this. Designs that avoid boolean blindness are more
flexible, less bug-prone, and allow the type checker to assist us when writing.
This, in turn, reduces cognitive load, improves our ability to refactor, and
means fewer bugs from things the compiler _could_ have checked if a function
_wasn't_ boolean-blind.

Partial functions are runtime bombs waiting to explode. The number of times the
'impossible' happened, especially in production code, is significant in our
experience, and most partiality is easily solvable. Allowing the compiler to
support our efforts, rather than being blind to them, will help us write more
clear, more robust, and more informative code. Partiality is also an example of
legacy, and it is legacy of _considerable_ weight. Sometimes, we do need an
'escape hatch' due to the impossibility of explaining what we want to the
compiler; this should be the _exception_, not the rule.

Derivations are one of the most useful features of GHC, and extend the
capabilities of Haskell 2010 considerably. However, with great power comes great
ambiguity, especially when ``GeneralizedNewtypeDeriving`` is in use. While there
_is_ an unambiguous choice if no strategy is given, it becomes hard to remember.
This is especially dire when ``GeneralizedNewtypeDeriving`` combines with
``DeriveAnyClass`` on a newtype. Explicit strategies give more precise control
over this, and document the resulting behaviour locally. This reduces the number
of things we need to remember, and allows more precise control when we need it.
Lastly, in combination with ``DerivingVia``, considerable boilerplate can be
saved; in this case, explicit strategies are _mandatory_.

# Project practices

## Testing

The project MUST have tests. These MUST use the [Hspec][hspec] framework. Tests
MUST be placed in a project-level directory named ``test``. A test's entry point
SHOULD be named ``Main.hs``.

Each publically visible data type with any invariants MUST have [property-based
tests][property-based-testing]; these SHOULD be written using [Hedgehog][hedgehog]
by way of [``hspec-hedgehog``][hspec-hedgehog]. Additionally, any
non-automatically-derived instances of any of the following type classes MUST
have property-based tests of their instance methods, using
[``hedgehog-classes``][hedgehog-classes]:

* ``Alternative``
* ``Applicative``
* ``Arrow``
* ``Bifoldable``
* ``Bifunctor``
* ``Bitraversable``
* ``Binary``
* ``Bits``
* ``Category``
* ``Comonad``
* ``Contravariant``
* ``Eq``
* ``Foldable``
* ``Functor``
* ``Integral``
* ``Monad``
* ``MonadIO``
* ``MonadPlus``
* ``MonadZip``
* ``Monoid``
* ``Ord``
* ``Enum``
* ``Semigroup``
* ``Traversable``
* ``Generic``
* ``Prim``
* ``Semiring``
* ``Ring``
* ``Star``
* ``Show``
* ``Storable``

The following _combinations_ of type classes, if not both automatically derived,
MUST also have property-based tests, using appropriate 'combination' testing
from ``hedgehog-classes``:

* ``Bifoldable`` in combination with ``Bifunctor``
* ``Read`` in combination with ``Show``
* ``FromJSON`` in combination with ``ToJSON``
* ``Bounded`` in combination with ``Enum``

A data type _has no invariants_ if _any_ possible value of the type is correct
vis a vis its problem domain; otherwise, it _has invariants_.

A type class is _automatically derived_ only if:

* It is derived with the ``stock`` or ``newtype`` deriving strategy; or
* It is derived with the ``via`` strategy against a type whose corresponding
  instances are themselves automatically derived; or
* It is derived with the ``anyclass`` strategy, with a default specified by 
  way of ``Generic``.

Each test SHOULD test one part of the project. A test SHOULD be runnable in
isolation from any other without affecting its results.

### Justification

Testing gives us a multitude of important protections and benefits:

* We have a runnable description of the specification of our problem;
* Regressions are automatically detected, with (some) indication of where the
  regression stems from;
* Ensures that we write code that is coherent and minimal;
* Detects potential problems as early as possible.

Property-based testing is a particularly good form of testing, as it inherently
generates failing counter-examples, leaving us free to focus on the logic of the
tests, instead of describing the testing data. It is _especially_ useful to
guarantee invariants of data; preserving these improves our ability to reason
about code in any place these types are used, as well as providing an
automatically verifiable specification of these invariants.

Type classes should (and often do) have laws; most of these cannot be verified
by a compiler. These are guaranteed (or should be) by the methods described as
'automatic', but these can't always be used. In those cases, there is a high
chance of bugs creeping into our code, partly because the cases where we can't
automate derivations are usually complex, and partly because we can expect no
support from the compiler. Having tests for law-abiding type classes, or
combinations of these, once again gives us strong assurances at point of use.

Much as with projects, tests should have minimal clutter, and be focused on
testing _one_ aspect of the codebase. This allows for easier debugging, and
reduces the time required in the test-edit-rebuild cycle. Additionally, tests
that take too long to run, or produce too much clutter, are likely to be run
less, defeating the whole purpose of having them in the first place.

[hedgehog-classes]: http://hackage.haskell.org/package/hedgehog-classes
[hspec-hedgehog]: http://hackage.haskell.org/package/hspec-hedgehog
[property-based-testing]: https://dl.acm.org/doi/abs/10.1145/1988042.1988046
[hedgehog]: http://hackage.haskell.org/package/hedgehog
[deriving-strategies]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/deriving-strategies
[functor-parametricity]: https://www.schoolofhaskell.com/user/edwardk/snippets/fmap
[alexis-king-options]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#warning-flags-for-a-safe-build
[hlint]: http://hackage.haskell.org/package/hlint
[ormolu]: http://hackage.haskell.org/package/ormolu
[rfc-2119]: https://tools.ietf.org/html/rfc2119
[boolean-blindness]: http://dev.stephendiehl.com/hask/#boolean-blindness
[parse-dont-validate]: https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
[hspec]: http://hackage.haskell.org/package/hspec
[cabal-fmt]: http://hackage.haskell.org/package/cabal-fmt
