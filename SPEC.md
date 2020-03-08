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

## Strings

A Medea _string_ is a non-empty sequence of UTF-8 scalar values (as defined by
[definition D76 (pdf)][d76] of the Unicode 5.2 standard), containing no symbols
from categories [Zs, Zl, Zp or Cc][categories]. Furthermore, the first and last
symbol of a Medea string must both be QUOTATION MARK Unicode character (hex code
0x22). [note about string validation?]

## Natural numbers

A Medea _natural number_ is a non-empty sequence of UTF-8 scalar values (as
defined by [definition D76 (pdf)][d76] of the Unicode 5.2 standard), containing
only symbols from DIGIT ZERO to DIGIT NINE inclusive (hex codes 0x30 to 0x39),
and not starting with DIGIT ZERO (hex code 0x30). [note about whitespace ending
a number?]

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

#### Array specification

**Description:** An _array specification_ describes the minimum and
maximum length of an array. 

**Preconditions:** The schema must have a type specifier by which a JSON array
would be considered valid.

**Syntax:** An array specification MUST consist of the following, in
this order:

1) Four space symbols;
2) The reserved identifier ``$length``;
3) A newline; and
4) One, or both of: a _minimum length specification_, a _maximum length
specification_; in any order.

A minimum length specification MUST consist of the following, in this order:

1) Eight space symbols;
2) The reserved identifier ``$minimum``;
3) A single space symbol; 
4) A Medea natural number; and
5) A newline.

A maximum length specification MUST consist of the following, in this order:

1) Eight space symbols;
2) The reserved identifier ``$maximum``;
3) A single space symbol;
4) A Medea natural number; and
5) A newline.

**Semantics:** A JSON value is considered valid by this specificer if it is a
JSON array. Additionally, if a minimum length specification is provided, the
array must have at least as many elements as the value of the Medea natural
number in said specification. Furthermore, if a maximum length specification is
provided, the array must have no more elements than the value of the Medea
natural number in said specification.

**Postconditions:** A Medea validator MUST indicate a unique error condition if
the value of the Medea natural number in a minimum length specification is
greater than the value of the Medea natural number in a maximum length
specification in the same array dimensions specification.

**Default:** An array may have any length (no minimum or maximum).

#### Object property specification 
 
**Description:** An _object property specification_ describes permitted
properties for an object, what schemata they must validate against, whether the
property is optional or required, and whether additional properties are allowed. 

**Preconditions:** The schema must have a type specifier by which a JSON object
would be considered valid.

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
