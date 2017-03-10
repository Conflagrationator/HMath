HMATH
=====

A General Math Library that aims to provide support for any applications needing robust mathematical equation representation, evaluation and manipulation support

Mathematical Structure
--------

The Structure provided for encoding expressions is designed to let the user of the library extend it with any value structures or operators as they see fit and extend upon the default value structures and operators.

### Expressions

Expressions are set up in a tree fashion:
```
(4+5)/3.2*(x-3)

     /
  ┌──┴───┐
  +     *
 ┌┴┐   ┌┴──┐
 4 5  3.2  -
          ┌┴┐
          x 3
```

All Types in the tree (all types that are involved in calculations) are members of the class `Expression`. The Function of `Expression`s is simply to evaluate everything below them (recursively). This is done via the `evaluate` function.

### Values

The types that encode values like numbers or vectors are called `Structures`.

`Structures` are members of the class `Expression` and are a specific subset that `evaluate` to themselves (making them leaf nodes of the tree).

Some defalut `Structure` Types/Constructors include:
- `Number`: `Absolute`, `Measure`

### Operators

The types that evaluate `Expressions` into new `Expressions` are `Operators`.

An `Operator` is defined with respect to a `class` which acts as a constraint on `Expressions` in that they must have the typeclass' functions defined to be used in that operator. This ensures type safety when calling the operator and allows for the `Operator` to predict the return type of the operation. That is, if any `Expression` wants to be added (via `Addition`), they must first declare how they are `Addable` by defining the function `add`.

Some default constraint classes with their respective `Operator`s include:
- `Addable` : `Addition`
- `Multipliable` : `Multiplication`
- `Powerable` : `Power`
- `VectorSpace` : `Negation` (& `Addition` & Scalar `Multiplication`)

Constraint classes can also be subclasses of others. For instance, `VectorSpace` is a subclass of `Addable` since vectors are addable and it is also a subclass of `Multipliable`, but only for a definition of scalar multiplication.
