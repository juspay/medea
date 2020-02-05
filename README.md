# Medea

## Why Medea?

### Medea is _efficient_

* Medea identifiers are limited in length, and each one can easily fit into a
  cache line.

### Medea is _principled_

* Medea has a formal model of its schema graph and schemata checks.
* Even though Medea allows recursive, or even self-recursive, schema
  definitions, the validator has an inherent limit of edge traversals, and will
  terminate if it is reached.
* Medea is highly opinionated - there is one right way to write something, and
  _only_ one.

### Medea is _user-friendly_

* Medea's schema graph file format is plain UTF-8 text, with minimal syntax.

