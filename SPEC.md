# Specification

## Conventions

The keywords MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY and OPTIONAL are to be interpreted as described in [RFC
2119][rfc2119].

To _indicate a unique error condition_ means that the Medea validator MUST:

* Demonstrate that it failed; and
* Have said demonstration be programmatically distinguishable from any
  demonstration of another unique error condition.

A Medea validator SHOULD use language-native means to indicate unique error
conditions (such as a language-specific error or exception), as opposed to use
of string error messages or output to the standard error stream.

A _JSON value_ is taken to be defined: that is, ``undefined`` is not considered
to be a valid JSON value. A Medea validator MUST NOT indicate that an
``undefined`` value is valid against _any_ schema.

## Identifiers

A Medea _identifier_ is a non-empty sequence of UTF-8 scalar values (as defined by
[definition D76 (pdf)][d76] of the Unicode 5.2 standard), not exceeding 32 bytes
in length, containing no whitespace symbols. If limited to text using the 
ASCII code points only, this means a limit of 32 symbols. A Medea validator MUST
indicate a unique error condition if given an identifier that contains more than
this number of symbols. 

### Reserved identifiers

Any identifier starting with the "$" symbol is _reserved_. Users MUST NOT 
define identifiers starting with the "$" symbol, as they are used by Medea
validators internally. A Medea validator MAY fail if given a user-defined
identifier starting with the "$" symbol; if it does so, it MUST indicate a
unique error condition in such a case.

Unless stated otherwise, a Medea identifier is _non-reserved_.

## Primitive type

A Medea _primitive type_ corresponds to a set of basic types, as provided by
JSON. These are defined as follows:

* ``null``: The null value.
* ``boolean``: A non-null boolean (``true`` or ``false``).
* ``object``: A non-null JSON object.
* ``array``: A non-null JSON array.
* ``number``: A non-null JSON number.
* ``string``: A non-null JSON string.

A Medea validator MUST provide validation of JSON values of these types, and
MUST provide primitive identifiers under the following primitive identifiers:

* ``null``: "$null"
* ``boolean``: "$boolean"
* ``object``: "$object"
* ``array``: "$array"
* ``number``: "$number"
* ``string``: "$string"

A Medea validator MUST indicate a unique error condition _for each primitive
type_ when validation of a value fails against a schema corresponding to such a
type.

## Schema graph file format

A Medea _schema graph file_ MUST be encoded as UTF-8. A Medea validator MUST
indicate a unique error condition if asked to parse a schema graph file which is
not encoded as valid UTF-8.

A Medea schema graph file SHOULD have the extension ``.medea``.

### Schema graph file sections

A Medea schema graph file MUST have two _sections_, in this order: a _schemata
section_ and a _schema list section_. A Medea validator MUST indicate a unique
error condition if given a schema graph file missing either of these sections,
or not having them in the order given.

A schemata section MUST consist of the following, in this order: the reserved
identifier "$schemata", a newline, and zero or more _schema name lines_. Each
schema name line consists of four space symbols, a Medea identifier, and a
newline. A Medea validator MUST indicate a unique error condition if any of
these criteria are violated.

Let _n_ refer to the number of schema name lines in a schemata section in a
Medea schema graph file. A schema list section in the same file MUST consist of
_n_ _schemata_, plus a _$start schema_ (both defined fully in the next section).
Each entry in a schema list section must be separated by one newline from the
next. A Medea validator MUST indicate a unique error condition if provided with
too many, or too few, schemata. Schemata can be provided in any order.

### Schemata

A _schema_ (plural schemata) MUST consist of the following, in this order:

1) The reserved identifier "$schema";
2) A single space symbol;
3) A Medea identifier, which MUST exist in a schema name line in the same file;
4) A newline symbol; and
5) Zero or more _specifications_ (defined fully in the subsequent section).

A Medea validator MUST indicate a unique error condition if a schema is defined
using an identifier that does not correspond to a schema name line in the same
file, or using an identifier that has already been used to define an existing
schema. Additionally, a Medea validator MUST raise a unique error condition if
the order, or formation rules, described above (or subsequent in the case of
type specifications or additional specifications) are violated: each possible
violation is distinct from any other. 

### Specifications

Any schema can include any of the following specifications at most once. Some
specifications are conditional on others (noted in their descriptions). A Medea
validator MUST indicate a unique error condition if a specification which has
conditions is placed in a schema before any specifications that would satisfy
these conditions.

#### Type specification

A _type specification_ may included as part of any schema. A type specification 
MUST consist of the following, in this order:

1) Four space symbols;
2) The reserved identifier "$type";
3) A newline symbol;
4) One or more _type specifier lines_.

Each _type specifier line_ MUST consist of the following, in this order:

1) Eight space symbols;
2) Either a Medea identifier (which MUST exist in a schema name line in the same
   file) or one "$null", "$boolean", "$object", "$array", "$number", "$string";
   and
3) A newline symbol.

A Medea validator MUST indicate a unique error condition if any of these
requirements are not met. A JSON value is valid against a type specification if
it is valid against _any_ type specifier line.

A type specification MUST NOT be _self-recursive_: its identifier section cannot
refer to the schema identifier it is a part of. Specifically, the following is
not valid Medea schema graph file content:

```
$schemata
    foo
$schema foo
    $type
        foo
```

A Medea validator MUST indicate a unique error condition upon discovering such a
situation.

If a type specification is not present, the schema treats any JSON value as
valid irrespective of its type.

[d76]: http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35
[rfc2119]: https://tools.ietf.org/html/rfc2119
