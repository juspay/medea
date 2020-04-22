# Semantic specification

## Introduction

We describe the semantics of _Medea_: a schema language for describing the
structure of JSON. We use Medea to define _Medea descriptions_, which define a
specific structure that JSON must conform to. We assume the existence of a
syntactically-valid human-readable representation of a Medea description is
available; an example would be a _Medea file_, described in SYNTAX.md.

Firstly, we describe the model of a Medea description and of JSON that
we used to define our semantics. We then describe how a valid Medea file
translates to this model, and the behaviour of validation, along with what it
deems valid and invalid.

## Conventions

The keywords MUST, MUST NOT, REQUIRED, SHALL, SHALL NOT, SHOULD, SHOULD NOT,
RECOMMENDED, MAY and OPTIONAL are to be interpreted as described in [RFC
2119][rfc2119].

A _JSON document_ is defined as per [ECMA-404 (pdf)][ecma-404]. We use
definitions from ECMA-404 for any JSON-related purpose in this document.

We will refer to terminology from SYNTAX.md throughout this document; unless
necessary for clarity, the definitions will not be repeated here.

The _value_ of a Medea natural number _n_ is the numeric quality it represents
as an unsigned decimal. The maximum value of a Medea natural number MUST be 
2,147,483,647 (that is, 2^31 - 1). 

## Model of JSON

We represent a JSON document as a labelled, rooted tree. More precisely, for any
JSON document _J_, the tree _T(J)_ has the following nodes:

* Every ``null``, JSON string, JSON boolean and JSON number in _J_ labels a leaf
  of the tree.
* Every JSON object in _J_ is represented by an internal node (which is
  labelled with the empty string). Said internal node has one child per property
  of said object; each edge to a child is labelled by the corresponding property
  name. 
* Every JSON array in _J_ is represented by an internal node (which is labelled
  with the empty string). Said internal node has one child per element of said
  array; each edge to a child is labelled by its corresponding index.

The root of _T(J)_ is the lexically first JSON value in _J_.

## Model of Medea description

We represent a Medea description as a directed, labelled flow network, with a
single source vertex. More precisely, for any Medea description _M_, the graph
_G(M)_ contains the following vertices:

* One vertex labelled with each of ``$array``, ``$boolean``, ``$null``,
  ``$number``, ``$object``, and ``$string``.
* One vertex per schema in _M_, labelled by its schema name.

Additionally, each vertex in _M_ labelled by the schema name for schema _S_ is
labelled with the following additional information:

* If _S_ contains a list specification, the values of the Medea natural numbers
  present in the corresponding maximum length specification and minimum length
  specification, if any;
* If _S_ contains an object specification, the value ``allowed`` if there is a
  corresponding additional property permission;
* If _S_ contains a string specification, each unique Medea string from all
  corresponding string value lines;

Additionally, for each schema _S_ in _M_, the following directed, labelled 
edges exist in _G(M)_. We will use _S_ and its naming identifier interchangeably
here, where clear from context:

* If _S_ contains a list specification, an edge from the vertex labelled with
  _S_ to the vertex labelled with the identifier in the corresponding element
  schema specification, if any. If the edge exists, it is labelled ``element``.
* If _S_ contains an object specification, for each object property specifier,
  an edge from the vertex labelled with _S_ to the vertex labelled with the
  identifier in the corresponding property schema line. If said property
  specifier contains an optional property declaration, said edge is labelled
  with ``(optional, N)``; otherwise, ``(mandatory, N)``, where ``N`` is the
  corresponding Medea string in the corresponding property name line.
* If _S_ contains a tuple specification, for each positional schema
  specification in _S_, an edge from the vertex labelled with _S_ to the vertex
  labelled with the identifier in the corresponding positional schema
  specification. Said edge is labelled with the natural number _n_, where _n_ is
  the document-order position of its corresponding positional schema
  specification (starting with 0).
* If _S_ contains a type specification, for each type specifier line, an edge
  from the vertex labelled with _S_ to the vertex labelled with the identifier
  in said type specifier line. Each such edge is termed a _typing edge_.

Lastly, the following two properties must hold for _G(M)_:

* The induced subgraph of _G(M)_ on only typing edges must be acyclic; and
* Aside from vertices labelled by a primitive type specifier, no vertices must
  have zero incoming _and_ zero outgoing edges; and
* For any vertex with labels corresponding to both a minimum length
  specification _min_ and maximum length specification _max_, _min <= max_.

## Model of validation


[ecma-404]: http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
[d76]: http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35
[rfc2119]: https://tools.ietf.org/html/rfc2119
[categories]: http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table
