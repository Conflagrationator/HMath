HMATH
=====

A General Math Library that aims to provide support for any applications needing robust mathematical equation representation, evaluation and manipulation support

Structure
--------

### Values

Values fundementally have a number-y part and a unit part
```
5.3 Hz
```

#### Numbers

Numbers are known in a few diffrent forms, each for their specific use case

* Real Finite       -- "normal" inputted numbers and everything measured
* Absolute Integer  -- numbers as used in math for finding simplest forms
* Unknown String    -- the representation of variables
* Infinity Bool     -- Infinity, whrere true is +∞ and false is -∞
* Undefined         -- the value when a computation cannot give a useful solution


#### Units

Units are stored as a combination of SI Units:

* Meter     -- Length
* KiloGram  -- Mass
* Second    -- Time
* Ampere    -- Electric Current
* Kelvin    -- Temperature
* Mole      -- Amount of Substance
* Candela   -- Luminous Intensity

they are stored as a list of (Dimension, Power) pairs
```
Newtons === [(KiloGram, 1), (Meter, 1), (Second, -2)]
```

These all have integer powers
(I haven't come across a unit that doesn't!)

### Equations

Equations are set up in a tree fashion:
```
4+5=3.2*(x-3)

   =
 ┌─┴───┐
 +     *
┌┴┐   ┌┴──┐
4 5  3.2  -
         ┌┴┐
         x 3
```

#### Evaluators



#### Identifiers
