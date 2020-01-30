# Specification

## Conventions

The keywords MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY and OPTIONAL are to be interpreted as described in [RFC
2119][rfc2119].

## Schema graph file format

A Medea _schema graph file_ MUST be encoded as UTF-8. A Medea validator MUST
indicate an error if asked to parse a schema graph file which is not encoded as
valid UTF-8.

A Medea schema graph file SHOULD have the extension ``.medea``.

## Identifiers

A Medea _identifier_ is a sequence of UTF-8 scalar values (as defined by
[definition D76 (pdf)][d76] of the Unicode 5.2 standard), not exceeding 32 bytes
in length. If limited to text using the ASCII code points only, this means a
limit of 32 symbols. 

A Medea validator MUST indicate an error if given an identifier that is too
large.

### Reserved identifiers

Any identifier starting with the "$" symbol is _reserved_. Users SHOULD NOT
define identifiers starting with the "$" symbol, as they are used by Medea
validators internally.

## Primitive type

A Medea _primitive type_ corresponds to a set of basic types, as provided by
JSON. These are defined as follows:

* ``null``: The null value.
* ``boolean``: A boolean (``true`` or ``false``).
* ``object``: A JSON object.
* ``array``: A JSON array.
* ``number``: A JSON number.
* ``string``: A JSON string.

A Medea validator MUST provide validation of JSON values of these types, and
MUST provide definitions of the schemata for such validation under the following
primitive identifiers:

* ``null``: "$null"
* ``boolean``: "$boolean"
* ``object``: "$object"
* ``array``: "$array"
* ``number``: "$number"
* ``string``: "$string"

[d76]: http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35
[rfc2119]: https://tools.ietf.org/html/rfc2119
