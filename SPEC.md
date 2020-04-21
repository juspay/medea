# Specification

## Introduction

This specification describes Medea, which is intended as a schema language for
describing and validating the structure of JSON documents. In particular, this
specification describes the following:

- The human-readable representation of a Medea description of a JSON document
  (that is, the _syntax_);
- The validation behaviour that is required from any given Medea construct (that
  is, the _semantics_); and
- Any requirements or limitations, as precisely as possible.

## Conventions

The keywords MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY and OPTIONAL are to be interpreted as described in [RFC
2119][rfc2119].

_Compile time_ refers to the step when the human-readable representation of a
Medea description is being converted to a machine-usable form. _Validation time_
refers to the step when, given a JSON document to validate and a machine-usable
form of a Medea description, validation of said document against said
description is requested.

To _indicate a unique error condition_ means that the Medea validator MUST:

* Demonstrate that it failed; and
* Have said demonstration be programmatically distinguishable from any
  demonstration of another unique error condition.

A Medea validator SHOULD use language-native means to indicate unique error
conditions (such as a language-specific error or exception), as opposed to use
of string error messages or output to the standard error stream. Unique error
conditions can be indicated both at compile time and validation time; if not
specified, they SHOULD be indicated at compile time.

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
unique error condition.

Unless stated otherwise, a Medea identifier is _non-reserved_.

## Strings

A Medea _string_ is a non-empty sequence of UTF-8 scalar values (as defined by
[definition D76 (pdf)][d76] of the Unicode 5.2 standard), containing no symbols
from categories [Zs, Zl, Zp or Cc][categories]. Furthermore, the first and last
symbol of a Medea string must both be QUOTATION MARK Unicode character (hex code
0x22). A Medea validator MUST indicate a unique error condition if given a
sequence of UTF-8 scalar values which has a QUOTATION MARK at the first and last
symbol, but contains symbols from any of Zs, Zl, Zp or Cc.

## Natural numbers

A Medea _natural number_ is a non-empty sequence of UTF-8 scalar values (as
defined by [definition D76 (pdf)][d76] of the Unicode 5.2 standard), containing
only symbols from DIGIT ZERO to DIGIT NINE inclusive (hex codes 0x30 to 0x39),
and not starting with DIGIT ZERO (hex code 0x30). A Medea validator SHOULD
indicate a unique error condition if it finds a sequence of DIGIT ZERO to DIGIT
NINE starting with DIGIT ZERO.

The _value_ of a Medea natural number is the natural number which it textually
represents.

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

A Medea _schema graph file_ is a human-readable representation of a Medea
specification. A Medea schema graph file MUST be encoded as UTF-8. A Medea validator MUST
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
schema in the same file. Additionally, a Medea validator MUST indicate a unique 
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

Any schema can include any of the following specifications at most once, in any
order. Some specifications are conditional on others (noted in their 
descriptions). A Medea validator MUST indicate a unique error condition if a 
specification is provided for a schema where its conditions are not met.

Each of the subsequent entries has the following format:

* **Description:** An overview of the purpose and intended semantics of this
  specification.
* **Preconditions:** Any requirements of the schema that MUST be met for this
  type specifier to be valid. A Medea validator MUST indicate a unique error
  condition when these are not met.
* **Syntax:** Describes the rules of form for this specification type. A Medea
  validator MUST indicate a unique error condition if any of these are violated.
* **Semantics:** Describes how this specification affects the validation
  behaviour of its schema.
* **Postconditions:** Any requirements or restrictions on the use of this type
  specifier of a non-syntactic nature. A Medea validator MUST indicate a unique
  error condition if any of these are violated.
* **Default:** Describes the validation behaviour of a schema missing this
  specification.

Certain combinations of specifications can produce contradictory requirements:
for example, a schema may have a type specification which requires a JSON object
with a property "foo" with a JSON string value, but then have an object property
specification saying that property "foo" should have a value which is a JSON array. 
A Medea validator MUST indicate a unique error condition at compile time in such
situations.

#### List specification

**Description:** A _list specification_ describes the specifics of a JSON array
meant to serve as a list; that is, a homogenously-typed collection of varying
length.

**Preconditions:** If the schema has a type specification, said type
specification must contain the type specifier line ``$array``. Additionally, the
schema must not contain a tuple specification. 

**Syntax:** A list specification MUST consist of one, or both, of the following, in any order: 

1) A _length specification_; and
2) An _element schema specification_.

A length specification MUST consist of one, or both, of the following, in any order:

1) A _minimum length specification_; and
2) A _maximum length specification_.

An element schema specification MUST consist of the following, in this order:

1) Four space symbols;
2) The reserved identifier ``$element_type``;
3) A space symbol;
4) _Either_ a Medea identifier, or one of ``$null``, ``$boolean``, ``$object``,
   ``$array``, ``$number``, ``$string``; and
5) A newline.

A minimum length specification MUST consist of the following, in this order:

1) Four space symbols;
2) The reserved identifier ``$min_length``;
3) A single space symbol;
4) A Medea natural number; and
5) A newline.

A maximum length specification MUST consist of the following, in this order:

1) Four space symbols;
2) The reserved identifier ``$max_length``;
3) A single space symbol;
4) A Medea natural number; and
5) A newline.

**Semantics:** A JSON value is considered valid by this specifier if it is a
JSON array. Additionally: 

* If an element schema specification is provided, every element of the array 
  must be valid, as defined by the following validation rules:
    * ``$null``: The value is ``null``.
    * ``$boolean``: The value is a JSON boolean.
    * ``$object``: The value is a JSON object.
    * ``$array``: The value is a JSON array.
    * ``$number``: The value is a JSON number.
    * ``$string``: The value is a JSON string.
    * Any other identifier: The value is valid according to the schema named by
      this identifier.
* If a minimum length specification is provided, the array must have at least as
  many elements as the value of the Medea natural number in said specification.
* If a maximum length specification is provided, the array must _not_ have more
  elements than the value of the Medea natural number in said specification.

**Postconditions:** A Medea validatory MUST indicate a unique error condition if
the identifier in an element schema specification does not correspond to any
schema defined in the current schema file. 

If both a minimum length specification and a maximum length
specification are provided, a Medea validator MUST indicate a unique error
condition if the value of the Medea natural number in the minimum length
specification is greater than the value of the Medea natural number in the
maximum length specification.

**Default:** An array may have any length (no minimum or maximum), and its
elements may be any JSON value.

#### Object property specification 
 
**Description:** An _object property specification_ describes permitted
properties for an object, what schemata they must validate against, whether the
property is optional or required, and whether additional properties are allowed. 

**Preconditions:** If the schema has a type specification, said type
specification must contain the type specifier line ``$object``.

**Syntax:** An object property specification MUST consist of the following, in
this order:

1) Four space symbols;
2) The reserved identifier ``$properties``;
3) A newline;
4) Zero or more _object property specifier sections_; and
5) An optional _additional property permission_.

Each object property specifier section MUST consist of the following, in this
order:

1) A _property name line_; and
2) An optional _property schema line_.
3) An optional _optional property declaration_.

A property name line MUST consist of the following, in this order:

1) Eight space symbols;
2) The reserved identifier ``$property-name``;
3) A single space symbol;
4) A Medea string; and
5) A newline.

A property schema line MUST consist of the following, in this order:

1) Eight space symbols;
2) The reserved identifier ``$property-schema``;
3) A single space symbol;
4) _Either_ a Medea identifier, or one of ``$null``, ``$boolean``, ``$object``,
  ``$array``, ``$number``, ``$string``; and
5) A newline.

An optional property declaration MUST consist of the following, in this order:

1) Eight space symbols;
2) The reserved identifier ``$optional-property``; and
3) A newline.

An additional property permission MUST consist of the following, in this order:

1) Eight space symbols;
2) The reserved identifier ``$additional-properties-allowed``; and
3) A newline.

**Semantics:** A JSON value is considered valid by this specifier if it a JSON
object, and for each of its object property specifier sections, the following
all hold:

* The object has a property whose name is the same as the Medea string given a
  the property name line;
* If a corresponding property schema line is provided, the value of said 
  property is valid by the schema named by the identifier given in the property 
  schema line.
* If a corresponding optional property declaration is _not_ provided, said 
  property is defined (that is, is not ``undefined``).

Furthermore, if the additional property permission is _absent_, no property is defined
for the object _other_ than those given by some object property specifier
section. 

A property value is always valid by no property schema line. Otherwise, these 
validation rules apply, based on the naming identifier:
 
* ``$null``: The property value is `null`.
* ``$boolean``: The property value is a JSON boolean.
* ``$object``: The property value is a JSON object.
* ``$array``: The property value is a JSON array.
* ``$number``: The property value is a JSON number.
* ``$string``: The property value is a JSON string.
* Any other identifier: The property value is valid according to the schema
  named by this identifier. 

**Postconditions:** A Medea validator MUST indicate a unique error condition if
an identifier in a property schema line does not correspond to any schema 
defined in the current schema file.

If multiple object property specifier sections have a property name line naming
the same schema, a Medea validator MUST indicate a unique error condition.

**Default:** If an object property specification is not present at all, a JSON
object is considered valid regardless of its properties and their values. 

If an object property specification is present, but provides no additional
information (that is, no object property specifier sections and no additional
property permission), a JSON object is only considered valid if it is empty
(that is, it defines no properties at all). 

If an object property specifier contains a property name line, but no property
schema line, then, provided that the named property is defined, any value for
said property is considered valid.  

#### String value specification

**Description:** A _string value specification_ describes which values a JSON string
is allowed to have.

**Preconditions:** If the schema has a type specification, said type
specification must contain the type specifier line ``$string``. 

**Syntax:** A string value specification MUST consist of the following, in this
order:

1) Four space symbols;
2) The reserved identifier ``$string_values``;
3) A newline;
4) One or more _string value lines_; and

Each string value line MUST consist of the following, in this order:

1) Eight space symbols;
2) A Medea string; and
3) A newline.

**Semantics:** A JSON value is considered valid by this specifier if it is a
JSON string. Additionally, the value must be equal to _any_ of the Medea strings
in a string value line.

**Postconditions:** A Medea validator MAY indicate a unique error condition at
compile time if two or more string value lines for the same string value
specifier are the same.

**Default:** The JSON string may have any value. 

#### Tuple specification

**Description:** A _tuple specification_ describes the specifics of a JSON array
meant to serve as a tuple; that is, a heterogenously-typed collection of fixed
length.

**Preconditions:** If the schema has a type specification, said type
specification must contain the type specifier line ``$array``. Additionally, the
schema must not contain a list specification.

**Syntax:** A tuple specification MUST consist of the following, in this order:

1) Four space symbols;
2) The reserved identifier ``$tuple``;
3) A newline; and
3) Zero or more _positional schema specifications_.

A positional schema specification MUST consist of the following, in this order:

1) Eight space symbols;
2) _Either_ a Medea identifier, or one of ``$null``, ``$boolean``, ``$object``,
   ``$array``, ``$number``, ``$string``; and
3) A newline.

**Semantics:** A JSON value is considered valid by this specifier if it is a
JSON array. Additionally, let _p(1)_, _p(2)_, ..., _p(N)_ be each of the
positional specifications, in the order declared, where _N_ is the total number
of positional schema specifications. For each _i_ in 1, 2, ... _N_, the _i -
1_th element of the array must be valid according to the following rules, based
on the Medea identifier used in _p(i)_:

* ``$null``: The value is ``null``.
* ``$boolean``: The value is a JSON boolean.
* ``$object``: The value is a JSON object.
* ``$array``: The value is a JSON array.
* ``$number``: The value is a JSON number.
* ``$string``: The value is a JSON string.
* Any other identifier: The value is valid according to the schema named by this 
  identifier.

Lastly, the array must have a length of _exactly_ _N_.

**Postconditions:** A Medea validator MUST indicate a unique error condition if
an identifier in a positional schema specification does not correspond to any
schema defined in the curent schema file.

**Default:** An array may have any length, and its elements may be any JSON
value.

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

**Semantics:** A JSON value is considered valid by this specifier if it is valid
by _any_ of the identifiers provided for all of its type specifiers. For each
individual identifier, the following validation rules apply:

* ``$null``: The JSON value is ``null`.
* ``$boolean``: The JSON value is a JSON boolean.
* ``$object``: The JSON value is a JSON object.
* ``$array``: The JSON value is a JSON array.
* ``$number``: The JSON value is a JSON number.
* ``$string``: The JSON value is a JSON string.
* Any other identifier: The JSON value is valid according to the schema named by
  this identifier.

**Postconditions:** Let _S, T_ be schemata in a single Medea schema graph i
file. We say that _S types as T_ if:

* _S_ has a type specifier; and
* The type specifier of _S_ contains a type specifier line with the naming
  identifier of _T_.

For any schema _S_, the _typing neighbourhood of S_ (denoted _T(S)_) is the
transitive closure of the 'types as' relation for _S_. We say that schema _S_ is
_circularly-typed_ if _S_ is a member of _T(S)_.

The type specifiers of a Medea schema graph file MUST NOT induce the circular
typing of any schema within it. A Medea validator MUST indicate a unique error
condition if a Medea graph file contains any schema _S_ such that _S_ is
circularly-typed. 

A Medea validator MUST indicate a unique error condition if an identifier in a
type specifier line does not correspond to any schema defined in the current
schema file.

**Default:** Any JSON value is considered valid by this specifier.

### Isolated schemata

We say that a schema _S_ in a Medea schema graph file is _isolated_ when it is
not referred to by any specification in its Medea schema graph file. A Medea
validator SHOULD indicate a unique error condition if it detects any isolated
schemata.

[d76]: http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35
[rfc2119]: https://tools.ietf.org/html/rfc2119
[categories]: http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table
