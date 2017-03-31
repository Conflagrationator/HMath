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
  +      *
 ┌┴┐    ┌┴──┐
 4 5   3.2  -
           ┌┴┐
           x 3
```

All Types in the tree (all types that are involved in calculations) are members of the class `Expression`. The Function of `Expression`s is simply to evaluate everything below them (recursively). This is done via the `evaluate` function.

### Values

When a mathematical object is the thing you're manipulating, combining and evaluating to, it is a `Value` Type. `Number`s are the quintessential example of a `Value` type.

Value types are the leaf nodes of the expression tree. In particular, what makes them the leaf nodes of the expression tree is that they must return themselves unchanged when they evaluate. (In fact, all expressions by definition evaluate to some `Value` type.)

Some `Value` types (like `Vectors` and `Matrices`) might also be 'container' types of sorts, where they potentially contain entire expression trees within themselves.

### Operators (& Constraints)

There are two components to creating an operator. The first is the Operator Type itself. This is the `Expression` type that is a node in the expression tree. An example would be `Addition` or `Multiplication`.

However, in order to evaluate these sensibly, we must put some restrictions on just what the rules for evaluating them are. This is where the constraints come in. Some example constraints are `Addable` and `Multipliable`. `Value` types implement these constraint classes.

As an example let's consider the case of `Matrix` operations:

`Matrix` being the `Value` type, we know that we can add matrices, given that their dimensions are the same. Thus we want an operator `Addition` that takes two of the same size matrices and outputs a matrix of the same size. Similarly for Scalar Multiplication and Negation.

```haskell
-- Addition
Addition (Matrix m n r) (Matrix m n r) (Matrix m n r)
-- Scalar Multiplication
Multiplication Number (Matrix m n r) (Matrix m n r)
Multiplication (Matrix m n r) (Matrix m n r)
-- Negation
Negation (Matrix m n r) (Matrix m n r)
```

Here we see `Addition`, `Multiplication`, and `Negation` are Operators. Nodes in the tree which contain sub-matrices and evaluate to their final type argument.

Now, this (along with an appropriate Zero vector construction) is enough to define matrices as being a `VectorSpace` as we can see from the definition of `VectorSpace`:

```haskell
class (Addable a a a, Multipliable Number a a, Multipliable a Number a) => VectorSpace a where
    zeroVector :: a -- The 0 vector (definition)
    negateVector :: Guard a -> Guard a -- Inverse Vector
```

An interesting case where the operator system shows its extensibility is in defining more specific operators such as matrix multiplication. For this we would still use the `Multiplication` operator but the type would be more specific.

```haskell
Multiplication (Matrix m n r) (Matrix n k r) (Matrix m k r)
```

as you can see here, the type restricts the operator to only evaluating the correct types. Here is an example for Vector Cross products, which as you know only work on vectors of dimension 3.

```haskell
Multiplication (Vector 3 r) (Vector 3 r) (Vector 3 r)
```


Reference
---------

### Values

```haskell
Number
Variable
Vector
Matrix
```

### Operators (and associated Constraint)

```haskell
Addition :: (Expression a n, Expression b m, Addable n m c) => a -> b -> Addition a b c
Multiplication :: (Expression a n, Expression b m, Multipliable n m c) => a -> b -> Multiplication a b c
Power :: (Expression a n, Expression b m, Radicalizable n m c) => a -> b -> Power a b c
Root :: (Expression b n, Expression c m, Radicalizable a n m) => b -> c -> Root b c a
Logarithm :: (Expression c n, Expression a m, Radicalizable m b n) => c -> a -> Logarithm c a b
Negation :: (VectorSpace a) => a -> Negation a
Magnitude :: (MeasureSpace a) => a -> Magnitude a
InnerProduct :: (InnerProductSpace a) => a -> a -> InnerProduct a
-- Type-Specific Operators
CrossProduct :: (Multipliable r r r) => Vector 3 r -> Vector 3 r -> CrossProduct r
Transpose :: Matrix m n r -> Transpose m n r
```

### Constraints

```haskell
class (Value a, Value b, Value c) => Addable a b c | a b -> c
class (Value a, Value b, Value c) => Multipliable a b c | a b -> c
class (Value a, Value b, Value c) => Radicalizable a b c | a b -> c, b c -> a, c a -> b
class (Addable a a a, Multipliable Number a a, Multipliable a Number a) => VectorSpace a
class (VectorSpace a) => MeasureSpace a
class (MeasureSpace a) => InnerProductSpace a
```
