# An imperative language specification

### Feature list

  - 01 Types (int, bool, char)
  - 02 Literals, arithmetic, comparison (text in "" resolves to `array<char>`)
  - 03 Variables, assignment
  - 04 read/write from/to stdin/stdout
  - 05 While, if/else, for
  - 06 Recursive procedures with overloading
  - 07 Argument passing like Java (primitive types by value, everything else by reference)
  - 08 Readonly variables and for loop
  - 09 Variable shadowing and static binding
  - 10 Runtime exceptions (unchecked exceptions in Java), printing stacktraces
  - 11 Return values for procedures
  - 12 Static typechecking
  - 13 Nested functions with static binding
  - 14 Records (structs without OOP (methods, inheritance,...)) and typed arrays
  - 15 Generating JSON like constructors and toString functions for records
  - 16 break, continue
  - 17 Anonymous functions, higher order functions, closures (capturing of variables similar to JS)
  - 18 Type aliases (like typedef in C)

Expected points: 30

### Description

The language is mostly based on the Late language specified in compiler construction course. There are a few additions like exceptions with stacktraces, higher order functions and closures, structs and arrays. Here is a description of features that are not present in C.

- Argument passing for functions will be done similar to the semantics of Java. 
- Exceptions are similar to unchecked exception in Java, however any type of value can be thrown be it struct, primitive type or an array. Later the value can be matched with a corresponding try block with simple instanceof check (like a `switch` block). The interpreter will not point out errors in catched types (like having two blocks catching exceptions of type `int`)  
- For non primitive types, the `const` modifier will prevent only the reassignment of the reference, not values themselves.
- For all structs and arrays default print method is generated

### Examples

Attached in archive

### Grammar

Attached in archive


