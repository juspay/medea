# Medea Tutorial

Medea is a simple schema language for specifying JSON, optimized for easily 
describing Haskell and Purescript types.

The system is ideal for describing:
- Javascript primitives (null, boolean, string, number, object, array)
- Sum types (multiple types permitted for the same property)
- Enums (one of a list of valid values for strings)
- Homogenously-typed arrays of variable length (with optional bounds)
- Tuples (heterogenously typed, fixed size collections) 
- Maps (objects with 0 or more defined properties, and fixed types for 
  additional properties)

In this tutorial, we'll explore the following topics:

1. How to create a schema
2. Defining re-usable type schemas
3. Defining sum types
4. Null, boolean, and number types
5. String types
6. Array types
7. Object types
8. Library usage for validation

## 1. How to specify a schema

```
$schema $start
    $type
        $object
    $properties
        $property-name "foo"
        $property-schema $number
        $additional-properties-allowed
        $additional-property-schema $null
```

This is a simple JSON specification in Medea that:

1. Defines the root type as ``object``;
2. Defines that we must have a property with name ``foo`` and type ``number``;
3. That additional properties beyond that are allowed; and
4. That any additional properties must be typed as ``null``.

A JSON example that would be valid against the above schema is:

```JSON
{
  "foo": 1,
  "bar": null
}
```

However, the following would not be valid:

```JSON
{
  "bar": null
}
```

The key concepts of Medea's syntax are:

* Language keywords, including primitive types, begin with ``$``;
* Strict, four-space indentation is enforced; 
* Trailing spaces are not allowed;
* String literals are wrapped in double-quotes; and
* Medea schemata are described in a Medea schema graph file, which has the
  ``.medea`` extension.

## 2. Defining re-usable type schemas

The ``$schema`` keyword begins the definition of a single schema. There must be
a schema named ``$start``, which describes the validity conditions for the first
value in the JSON to be validated. A Medea schema graph file can contain
multiple schemata, separated by a single empty line:

```
$schema $start
    $type
        foo

$schema foo
    $type
        $string
```

Schemata can refer to each other, as long as they are all within the same file.
We use a schema's name for this purpose. The above schemata together validate
any JSON consisting of a single string value; for example, this would be valid:


```JSON
"example value"
```

We can see that the schema name ``foo`` can be used in place of any type
argument.

**NOTE:** Schemata can't declare themselves their own type.

## 3. Defining sum types

The ``$type`` keyword accepts a list of one or more primitive types, or defined
schemata:

```
$schema $start
    $type
        $array
        $object
```

Here, our ``$start`` schema's allowed type is either a JSON array or a JSON
object.

Listing a type in a schema allows us to also specify additional requirements of
values we validate, provided they have the corresponding type. For example, we
could modify the above schema to state that JSON objects which validate against
it must have certain properties of certain types, or JSON arrays must have a
minimum length.

## 4. Primitive types

Medea provides three primitive types which have no additional specification
options. These are ``$null``, ``$boolean`` and ``$number``, corresponding to the
JSON null value, any JSON boolean and any JSON number. These can be used
whenever a type is required.

There are also three additional primitive types which have additional
specification options: ``$string``, ``$array`` and ``$object``, corresponding to
JSON strings, JSON arrays and JSON objects. In particular, ``$array`` gives us
two mutually-exclusive sets of additional options, depending on whether we want
the array to be treated as a _list_ (that is, a homogenously-typed collection of
varying length) or as a _tuple_ (that is, a heterogenously-typed collection of
fixed length).

## 5. String types

String types permit an optional specification defining a list of valid values
that the string may take:

```
$schema $start
    $type
        $string
    $string-values
        "foo"
        "bar"
        "baz"
```

The above schema validates a JSON string which is one of exactly ``"foo"``,
``"bar"`` or ``"baz"``.

If you use the ``$string-values`` keyword, you must specify at least one value.
Only string literals are permitted.

## 6. Array types

Array types have two sets of mutually-exclusive additional options. One set,
called _list_ specifications, is designed to describe a JSON array which may
have a range of lengths, but all of whose elements are structurally similar. The
other set, called _tuple_ specifications, is designed to describe a JSON array
which has fixed length, but whose elements are structurally different (perhaps
dramatically).

For an example of list specifications, the following schema validates only JSON
arrays with at least 3 elements, all of which must be strings:

```
$schema $start
    $type
        $array
    $min-length 3
    $element-type $string
```

The ``$min-length`` and ``$max-length`` keywords are optional, and accept
positive integers. The ``$element-type`` keyword accepts one of either a
primitive type, or a defined schema. If you want an array whose elements may be
of multiple types (that is, effectively a list of sums), use a defined schema
with multiple types:

```
$schema $start
    $type
        $array
    $element-type foo

$schema foo
    $type
        $boolean
        $number
```

Each of the keyword lines described for list specifications are optional, and
can be placed in any order.

For an example of tuple specifications, the following schema validates JSON
arrays of length _exactly_ 3, such that the first element is a JSON string, the
second is a JSON boolean, and the third is the JSON ``null``:

```
$schema $start
    $type
        $array
    $tuple
        $string
        $boolean
        $null
```

The following JSON array would be valid against the above schema:

```JSON
[ "hello", false, null ]
```

The ``$tuple`` keyword accepts any number of primitive types or defined
schemata. That list represents the number of elements an array must have, and
their types (or what they must validate against), in order. If given an empty
list, the only valid value is the empty array; specifically, given the following
schema:

```
$schema $start
    $type
        $array
    $tuple
```

The _only_ JSON value valid against it is:

```JSON
[]
```

7. Object Types

The ``$object`` specification allows various ``$properties`` specifications.
Each such specification has the following options:

1. ``$property-name`` describes the property name as a string.
2. ``$property-schema`` defines the primitive type or defined schema that the
  named property's value must be valid by.
3. ``$optional-property`` describes whether the property is optional; if this is
  present, objects missing this property will still be considered valid.

These must be specified in sequential lines, in the above order. Additionally,
once all properties are specified, two additional keywords are available:

1. ``$additional-properties-allowed``, which states that the previously-described
  list of properties is not exhaustive; and
2. ``$additional-property-schema``, which describes what schema any additional
   properties (beyond those described as above) must be valid against.

The following is an exhaustive example of all these options:

```
$schema $start
    $type
        $object
    $properties
        $property-name "foo"
        $property-schema foo
        $property-name "bar"
        $optional-property
        $additional-properties-allowed
        $additional-property-schema $number

$schema foo
    $type
        $boolean
        $null
```

This schema graph file validates JSON objects, which contain at _least_ the
property named ``foo``. It is allowed to contain additional properties; if a
property ``bar`` is defined, it is allowed to have any value, but any other
property must be a JSON number. The property named "foo" must have a value which
is either a JSON boolean or the JSON ``null``.

## 8. Library usage for validation

To validate a JSON value using Medea from Haskell:

```Haskell
import Control.Monad.Trans.Except (runExcept, runExceptT)
import Data.Aeson (Value)
import Data.Medea.Loader (loadSchemaFromFile)
import Data.Medea (validate)

main :: IO ()
main = do
  -- Compile a Medea schema graph from its file representation
  result <- runExceptT . loadSchemaFromFile $ "./my-schema.medea"
  case result of
    Left e -> print e
    Right scm -> do
      -- Validate against the schema graph we just compiled 
      validation <- runExcept $ validate scm (myJson :: Value)
      case validation of 
        Left e -> print e
        Right _ -> putStrLn "JSON is valid against schema"
```

The result of a successful validation is an annotated JSON tree; we don't use it
here, but it can be useful as an intermediate processing input.
