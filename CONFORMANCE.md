# Conformance suite

## Introduction

The conformance suite is a collection of source files (both schemata and data,
where appropriate). These are designed to test error paths in the parser and
schema builder, as well as the behaviour of the validator.

## Structure

The conformance suite is located entirely in the ``conformance`` directory. It
is divided into three sections:

* ``parser``: Erroneous ``.medea`` files which should be rejected by the 
  parser.
* ``schema-builder``: Erroneous ``.medea`` files which parse, but should still
  be rejected.
* ``validator``: ``.medea`` files which correspond to valid schemata, as well as
  JSON files to serve as test data.

The ``validator`` directory also has two sub-directories: ``valid`` and
``invalid``. Each of these sub-directories contains pairs of ``.medea`` and
``.json`` files - the base names correlate which schema graph file goes with
which data file. The pairs in the ``valid`` sub-directory should validate, while
the pairs in the ``invalid`` directory should not.
