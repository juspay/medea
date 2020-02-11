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

A _newline_ is a platform-specific, non-empty sequence of bytes indicating the
end of a line.

## Identifiers

A Medea _identifier_ is a non-empty sequence of UTF-8 scalar values (as defined by
[definition D76 (pdf)][d76] of the Unicode 5.2 standard), not exceeding 32 bytes
in length, containing no symbols from categories [Zs, Zl, Zp or Cc][categories]. If 
limited to text using the ASCII code points only, this means a limit of 32 
symbols. A Medea validator MUST indicate a unique error condition if given an 
identifier that contains more than this number of symbols. 

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
MUST provide the following _primitive type identifiers_:

* ``$null``
* ``$boolean
* ``$object``
* ``$array``
* ``$number``
* ``$string``

## Schema graph file format

A Medea _schema graph file_ MUST be encoded as UTF-8. A Medea validator MUST
indicate a unique error condition if asked to parse a schema graph file which is
not encoded as valid UTF-8.

A Medea schema graph file SHOULD have the extension ``.medea``.

A Medea file is made up of one or more _schemata_. A _schema_ (singular of 
'schemata') MUST consist of the following, in this order:

1) The reserved identifier "$schema";
2) A single space symbol;
3) A Medea identifier (called the _name_ or _naming identifier_);
4) A newline; and
5) Zero or more _specifications_ (defined fully in the subsequent section).

A Medea validator MUST indicate a unique error condition if a schema is defined
with a name that has already been used as the naming identifier of an existing
schema in the same file. Additionally, a Medea validator MUST raise a unique 
error condition if the order, or formation rules, described above (or 
subsequent in the case of type specifications or additional specifications) 
are violated: each possible violation is distinct from any other. 

Additionally, a Medea graph file MUST contain a schema named ``$start``. A Medea
validator MUST indicate a unique error condition if no such schema is defined.

Schemata MUST be separated by a single extra newline. Thus, a schema graph file
containing schemata ``foo`` and ``$start`` is formed like this:

```
$schema foo

$schema $start
```

### Specifications

Any schema can include any of the following specifications at most once. Some
specifications are conditional on others (noted in their descriptions). A Medea
validator MUST indicate a unique error condition if a specification is provided
for a schema where its conditions are not met.

Each of the subsequent entries has the following format:

* **Description:** An overview of the purpose and intended semantics of this
  specification.
* **Preconditions:**
* **Syntax:** Describes the rules of form for this specification type. A Medea
  validator MUST indicate a unique error condition if any of these are violated.
* **Semantics:** Describes how this specification affects the validation
  behaviour of its schema.
* **Default:** Describes the validation behaviour of a schema missing this
  specification.
 
#### Type specification

**Description:** A _type specification_ describes basic rules of form for JSON
values.

**Preconditions:** None

**Syntax:** A type specification MUST consist of the following, in this order:

1) Four space symbols;
2) The reserved identifier ``$type``;
3) A newline; and
4) One or more _type specifier lines_.

Each type specifier line MUST consist of the following, in this order: 

1) Eight space symbols;
2) _Either_ a Medea identifier, or one of ``$null``, ``$boolean``, ``$object``,
  ``$array``, ``$number``, ``$string``; and
3) A newline.

[TODO: Add note about the type relation graph being acyclic?]

**Semantics:** A JSON value is considered valid by this specifier if it is valid
by _any_ of the identifiers provided for all of its type specifiers. For each
individual identifier, the following validation rules apply:

* ``$null``: The JSON value is ``null`.
* ``$boolean``: The JSON value is a JSON boolean.
* ``$object``: The JSON value is a JSON object.
* ``$array``: The JSON value is a JSON array.
* ``$number``: The JSON value is a JSON number.
* ``string``: The JSON value is a JSON string.
* Any other identifier: The JSON value is valid according to the schema named by
  this identifier.

A Medea VALIDATOR must indicate a unique error condition if an identifier in a
type specifier line does not correspond to any schema defined in the current
schema file.

**Default:** Any JSON value is considered valid by this specifier.

[d76]: http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35
[rfc2119]: https://tools.ietf.org/html/rfc2119
[categories]: http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table
