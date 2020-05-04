# Medea Tutorial


Medea is a simple schema language for specifying JSON, optimized for easily describing Haskell and Purescript types.

The system is ideal for describing:
- Javascript Primitives (null, boolean, string, number, object, array)
- Sum types (multiple types permitted for the same property)
- enums (one of a list of valid values for strings)
- Tuples (heterogynously typed, fixed size collections) 
- Maps (objects with 0 or more defined properties, and fixed types for additional properties)

In this tutorial, we'll explore the following topics:

1. How to Create a Schema
2. Defining re-usable type schemas
3. Defining Sum Types
4. Null, Boolean, and Number Types
5. String types
6. Array types
7. Object Types
8. Library usage for validation


## 1. How to specify a Schema

example.medea
```
$schema $start
    $type
        $object
    $properties
        $property-name "foo"
        $property-schema $number
        $optional-property
        $additional-properties-allowed
        $additional-property-schema $null
```

This is a simple JSON specification in Medea that 1) defines the root type as `object`, and 2) defines properties one optional property of that object with the name `foo` and a type of `number`, finally it defines 3) any additional properties to hold the type of `null`.

A JSON example that would be valid against the above schema is:

```JSON
{
  "foo": 1,
  "bar": null
}
```

This should provide an intuition around how a Medea schema approaches describing a JSON value. 

Here are some important key concepts about the Medea Syntax
- language keywords including primitive types begin with `$`.
- the language enforces strict, four-space indentation
- the language does not permit trailing spaces
- string literals are wrapped in double-quotes
- medea schema are described in a *Medea Schema Graph file*, which has the `.medea` extension


## 2. Defining re-usable type schemas

The `$schema` keyword begins the definition of a single schema, with the $start schema referring to the primary value to be validated. However the file can contain multiple schemas, seperated by a line. 

```
$schema $start
    $type
        foo

$schema foo
    $type
        $string
```

Here we can see that we can define schemas with a name using the `$schema` keyword, and that we can refer to a defined schema from other schemas within the same Medea Schema Graph file.

The above schemata together define a single string value, this would be validated by the JSON value.

```
"example value"
```

By defining a Schema for `foo`, the `foo` schema can be used in place of any type argument.

NOTE: Schema cannot be defined in terms of themselves.

## 3. Defining Sum Types

The `$type` keyword accepts a list of one or more primitive types or defined schema.

```
$schema $start
    $type
        $array
        $object
```

Here we can see that our root schema defines a type which can be either any valid object, or any valid array.

Listing a type in a schema allows the optional additional specifications related to that type to be available. For example, we can specify properties which will be checked against our value if and only if the supplied value is an object, we can also specify a minimum length that will be checked if the supplied value is an array.

## 4. Primitive Types

Medea supplies 3 primitive types which have no additional specification options. These are `$null`, `$boolean`, and `$number`. These can be used by the `$type` keyword or in the place of any type argument.

3 additional primitives are available with additional options, these are `$string`, `$array`, and `$object`.

5. String types

String types permit an optional specification to define an enum of valid strings:

```
$schema $start
    $type
        $string
    $string-values
        "foo"
        "bar"
        "baz"
```

The above schema specifies a string that is strictly one of: "foo", "bar", or "baz". 

If you use the `$string-values` keyword, you must specify at least one value.

Only string literals are permitted, no schemas or types.

## 6. Array types

Array values allow several seperate options, some of which cannot be used together:
- Minimum Length
- Maximum Length
- Element Schema Specification
- Tuple Specification

The tuple specification cannot be used with minimum length, maximum length, or element schemas, as the tuple specification specifies types and length at once.

Otherwise, each of minimum length, maximum length, and element schemas can be used optionally in any order.

To specify a JSON Array of 3 or more strings :

```
$schema $start
    $type
        $array
    $min-length 3
    $element-type
        $string
```

The `$min-length` and `$max-length` keywords accept positive integers.

The `$element-type` keyword accepts any primitive type or defined schema.

`$element-type` accepts only a single type. To defined an Array which has elements of multiple types and does not have a fixed length, use a defined schema with multiple types:

```

$schema $start
    $type
        $array
    $element-type
        foo

$schema foo
    $type
        $boolean
        $number
```


The tuple specification describes the types, length and order of the values in an array. To specify a tuple over string, boolean, and null:

```
$schema $start
    $type
        $array
    $tuple
        $string
        $boolean
        $null
```

The following JSON array would be valid against the above value:

```JSON
[ "hello", false, null ]
```

The `$tuple` keyword accepts any number of primitive types or defined schema. the array supplied must have a length equal to the number of types specified in the tuple, with values of types in the same order as the order of types in the tuple specification.

7. Object Types

The `$object` specification allows various `$properties` specifications. Each property can be specified in the following ways

- `$property-name` - defines the property name as a string
- `$property-schema` - defines the primitive type or defined schema the property value must match
- `$optional-property` - defines whether the property is optional, if present, the property will be marked as valid if the property is not present, if the property is present it will be validated against the property schema, if one is present.

The specifiers for a single property must described on up to 3 sequential lines, in the order above

Additionally, after all named properties are specified, the two keywords for specifying additional properties are available in this order:

- `$additional-properties-allowed` - if present, properties in addition to named properties in the schema will be permitted, if omitted, only the specified property names are valid.
- `$additional-property-schema` - if present, additional properties will be validated against the schema

Here is an example that uses all of the optional specifiers

```
$schema $start
    $type
        $object
    $properties
        $property-name "foo"
        $property-schema foo
        $property-name "bar"
        $optional-property
        $additional-properties-allow
        $additional-property-Schema $number

$schema foo
    $type
        $boolean
        $null
```

This schema graph file describes a JSON value which is an object,  permitting a property name of `foo`, with a value that is either a boolean or null, there is an additional property named `bar` which can be any value, and any more properties must be numbers.

8. Library usage for validation

to validate a JSON value using Medea from Haskell:

```Haskell
import Control.Monad.Trans.Except (runExcept, runExceptT)
import Data.Aeson (Value)
import Data.Medea.Loader (loadSchemaFromFile)
import Data.Medea (validate)

main :: IO ()
main = do
  result <- runExceptT . loadSchemaFromFile $ "mySchema.medea"
  case result of
    Left e -> print e
    Right scm -> do
      validation <- runExcept $ validate scm (myjson :: value)
      case validation of 
        Left e -> print e
        Right _ -> putStrLn "JSON is valid against schema"
```


