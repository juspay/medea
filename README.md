# Medea

## What is this?

Medea is a schema language for JSON document structure. It is similar to [JSON
Schema][json-schema], but is designed to be simpler and more self-contained.

This repo contains both a specification (in ``SPEC.md``) and a reference
implementation (in Haskell). We also have a [PureScript implementation available][medea-ps].

Medea is named after [Jason's wife][medea]. Additionally, the name reflects the
tensions between Jason and Medea as told in the legends, as told by the
[implementer of ``medea-ps``][ben]:

> "I'm not sure if the name is because it loves JSON, or murdered all of JSON's kids
> and flew away in a chariot pulled by dragons."

## Why Medea?

Since JSON Schema exists, and has gone through a range of revisions, why does
Medea exist? There are several reasons for this, which we summarize below.

### The JSON Schema standard is complex

The current version of the JSON Schema standard (the 2019-09 draft at the time
of writing) is _highly_ complex. It, along with its adjacent standards, covers
considerably more than simply validating JSON documents. It also includes:

* A MIME type for schema files
* Meta-schema information, including URI-based stuff
* Lexical and dynamic scope of keywords
* Sub-schemata and rules they follow
* Output formatting
* How to identify schemata via media type parameters

Note that this is _before_ we get to anything to do with validating JSON! It is
perhaps unsurprising that no Haskell package exists which complies with the
current standard. Thus, we would have to develop something 'from scratch' in any
case.

For our purposes, none of this was needed - we simply wanted to have a way to
describe the valid structure of a JSON document in textual form, and have a way
to use it as a cross-language tool for validating the structure of JSON. Medea
focuses on this, and _only_ this.

### JSON Schema requires arbitrary URI resolution

Due to the design of JSON Schema, schemata themselves have (meta) schemata.
These are indicated by use of URIs. Additionally, schemata can include other
schemata, in several different ways, _also_ indicating this by URIs. These URIs
can aim at basically arbitrary locations; in fact, multiple examples in the
standard(s) specify online locations where such information can be found.

This essentially means that a compliant validator must be able to follow
_arbitrary_ URIs, or give users the ability to direct the validator. This isn't
even needed for _validation itself_ - we might need to do this just to know what
the schema is! Furthermore, as canonical examples require us to fetch
information from online (or have a means of users directing us there), any
validator we create would need to support fetching data from the Internet. 

While this can be useful, it puts considerable complexity on the part of both
the validator and the user. The use case that prompted the creation of Medea
didn't (and still doesn't) warrant this degree of complexity. This is not merely
a question of implementation time or dependency weight - it is also an issue of
correctness and usability. The design of Medea deliberately limits this - all
schemata are single, self-contained files. How these files are obtained, and
aimed at, is left to the user; if they need to fetch these from a remote
location or not should not be Medea's concern (and isn't).

## Getting started 

For an easy starting point, check out ``TUTORIAL.md``, which describes basic usage
of Medea, with examples. For a more detailed description of Medea's capabilities
and schema language, ``SPEC.md`` has you covered. 

We also provide a collection of Medea schema files, designed for conformance
testing, in the ``conformance`` directory.

## What does Medea work on?

We support every major GHC version from 8.6 onwards, for its most current minor
version. At the moment, this means:

* 8.6.5
* 8.8.3
* 8.10.1

We support both Stack and Cabal (v2) builds. We aim to be cross-platform where
possible - if the dependencies work on the platform, Medea should too.

## License

Medea's specification, as well as the Haskell (and PureScript) reference
implementations, are under the MIT license. Please see ``LICENSE.md`` for more
information.

[json-schema]: https://en.wikipedia.org/wiki/JSON#JSON_Schema 
[medea-ps]: https://github.com/juspay/medea-ps
[json-schema-validators]: https://json-schema.org/implementations.html#validators
[medea]: https://en.wikipedia.org/wiki/Medea
[ben]: https://github.com/Benjmhart
