# Syntax specification

## Introduction

We describe the syntax of _Medea_. Medea is a schema language for describing the
structure of JSON. We use Medea to define _Medea descriptions_, which define a
specific structure that JSON must conform to. We describe the human-readable
syntax of Medea descriptions, along with any requirements or limitations of said
syntax.

## Conventions

The keywords MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY and OPTIONAL are to be interpreted as described in [RFC
2119][rfc2119].

A _JSON document_ is defined as per [ECMA-404 (pdf)][ecma-404].

A _newline_ is a platform-specific, non-empty sequence of bytes indicating the
end of a line. A _space_ is the Unicode symbol SPACE (hex code 0x20).

## Syntax

### General notions

A _Medea file_ contains the human-readable representation of a single Medea
description. A Medea file MUST be a non-empty sequence of UTF-8 scalar values,
as defined by [definition D76 (pdf)][d76] of the Unicode 5.2 standard). A Medea
file MUST conform to the structure described in this document (to follow). If
stored as a file system object, a Medea file SHOULD have the extension
``.medea``. 

A _Medea token_ is a non-empty sequence of UTF-8 scalar values in a Medea file.
Tokens MUST be separated by any of the following:

* A non-empty sequence of spaces;
* A non-empty sequence of newlines.

Tokens MUST NOT contain any symbols from categories [Zs, Zl, Zp or
Cc][categories]. 

The _length_ of a token is the number of bytes required to represent its
sequence of scalar values.

A _Medea identifier_ is a Medea token whose length MUST NOT be more than 32.
Medea identifiers are separated into two varieties: _reserved_ and
_non-reserved_. A Medea identifier is reserved if its first symbol is DOLLAR
SIGN (hex code 0x24); otherwise, it is non-reserved. If not specified,
references to 'a Medea identifier' in this document refer to non-reserved
identifiers. Where clear from context, we will drop 'Medea', referring only to
'identifiers', 'reserved identifiers' and 'non-reserved identifiers' as
appropriate. 

A _primitive type identifier_ is one of the following reserved identifiers:

* ``$array``
* ``$boolean``
* ``$null``
* ``$number``
* ``$object``
* ``$string``

A _Medea natural number_ is a Medea token of length from 1 to 10 bytes
inclusive. A Medea natural number MUST only contain symbols from DIGIT ZERO to
DIGIT NINE inclusive (hex codes 0x30 to 0x39), and MUST NOT have DIGIT ZERO (hex
code 0x30) as its first symbol.

A _Medea string_ is a Medea token of length greater than 2. The first and last
symbol of a Medea string MUST both be QUOTATION MARK (hex code 0x22).

### Structure of a Medea file

A Medea file consists of one or more _Medea schemata_ (singular _Medea schema_).
Where clear from context, we will drop 'Medea', referring only to 'schemata' or
'schema' as appropriate.

A schema MUST consist of the following, in this order:

1) The reserved identifier ``$schema``;
2) One space;
3) One identifier (called the _schema name_ or the _schema naming identifier_);
4) One newline; and
5) Zero or more _specifications_.

Within a single Medea file, every schema name MUST be unique. Additionally, one
schema in each Medea file MUST have the schema name ``$start``; all other
schemata MUST have schema names which are non-reserved identifiers. Schemata in
a Medea file MUST be separated by one newline.

### Specifications

A specification describes particular structural requirements. A schema can
include any specification zero or one time; specifications within a single
schema can be in any order.

Each of the subsequent entries describes a specification, in the following
format:

* **Description:** An overview of the purpose of the specification.
* **Preconditions:** Any requirements that MUST be met by a schema if it
  is to contain this specification.
* **Syntax:** Describes the way this specification MUST be formed in a
  schema.

#### List specification

**Description:** A _list specification_ describes the form of a JSON array meant
to serve as a list; that is, a homogenously-typed collection of varying length.

**Preconditions:** If the schema has a type specification, said type
specification contains the type specifier line ``$array``. Additionally, the
schema does not contain a tuple specification.

**Syntax:** At least one of the following, in any order:

* A _minimum length specification_
* A _maximum length specification_
* An _element schema specification_

A minimum length specification conists of the following, in this order:

1) Four spaces;
2) The reserved identifier ``$min_length``;
3) One space;
4) A Medea natural number; and
5) One newline.

A maximum length specification consists of the following, in this order:

1) Four spaces;
2) The reserved identifier ``$max_length``;
3) One space;
4) A Medea natural number; and
5) One newline.

An element schema specification consists of the following, in this order:

1) Four spaces;
2) The reserved identifier ``$element_type``;
3) One space;
4) _Either_ one identifier, _or_ one primitive type identifer; and
5) One newline.

#### Object specification

**Description:** An _object specification_ describes the form of a JSON object. 

**Preconditions:** If the schema has a type specification, said type
specification contains the type specifier line ``$object``.  

**Syntax:** The following, in this order:

1) Four spaces;
2) The reserved identifier ``$properties``;
3) A newline;
4) Zero or more _object property specifiers_; and
5) Zero or one _additional property permission_;

Each object property specifier consists of the following, in this order:

1) A _property name line_;
2) Zero or one _property schema line_; and
3) Zero or one _optional property declaration_;

A property name line consists of the following, in this order:

1) Eight spaces;
2) The reserved identifier ``$property_name``;
3) One space;
4) One Medea string; and
5) One newline.

A property schema line consists of the following, in this order:

1) Eight spaces;
2) The reserved identifier ``$property_schema``;
3) One space;
4) _Either_ one identifier, _or_ one primitive type identifier; and 
5) One newline;

An optional property declaration consists of the following, in this order:

1) Eight spaces;
2) The reserved identifier ``$optional_property``; and
2) One newline.

An additional property permission consists of the following, in this order:

1) Eight spaces;
2) The reserved identifier ``$additional_properties_allowed``; and
3) One newline.

#### String specification

**Description:** A _string specification_ describes the form of a JSON string.

**Preconditions:** If the schema has a type specification, said type
specification contains the type specifier line ``$string``. 

**Syntax:**

#### Tuple specification

**Description:** A _tuple specification_ describes the form of a JSON array
meant to serve as a tuple; that is, a heterogenously-typed collection of
fixed length.

**Preconditions:** If the schema has a type specification, said type
specification contains the type specifier line ``$array``. Additionally, the
schema does not contain a list specification.

**Syntax:** Consists of the following, in this order:

1) Four spaces;
2) The reserved identifier ``$tuple``;
3) One newline; and
4) Zero or more _positional schema specifications_.

A positional schema specification consists of the following, in this order:

1) Eight spaces;
2) _Either_ one identifier _or_ one primitive type identifier; and
3) One newline. 

#### Type specification

**Description:** A _type specification_ describes basic rules of form for all
JSON values, by relating them to the base constructs of JSON (such as numbers,
strings, arrays, etc).

**Preconditions:** None.

**Syntax:** Consists of the following, in this order:

1) Four spaces;
2) The reserved identifier ``$type``;
3) One newline; and
4) One or more _type specifier lines_.

Each type specifier line consists of the following, in this order:

1) Eight spaces;
2) _Either_ one identifier _or_ one primitive type identifier; and
3) One newline.
