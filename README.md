# opaque-constraints [![Build Status](https://travis-ci.com/basile-henry/opaque-constraints.svg?branch=master)](https://travis-ci.com/basile-henry/opaque-constraints)

Have you ever wished GHC would complain about your constraint synonym not being
fulfilled rather than what it depends on?

There is a way! It is a bit convoluted, but if you're interested it is very well
explained in https://kcsongor.github.io/opaque-constraint-synonyms/

This package automates this setup using *TemplateHaskell*. It should be fairly
safe to use as long as the module `mkOpaque` is used in has an explicit export
list!
`mkOpaque` creates a data type that **should not** be exported from your module!

Here is a small example module using `mkOpaque`:

```haskell
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Serialise (roundTrip) where

import Data.Constraint.Opaque

mkOpaque [d|
  -- This type alias for the constraints Show and Read is changed into a more
  -- "opaque" constraint synonym
  type Serialise a = (Show a, Read a)
  |]

roundTrip :: Serialise a => a -> a
roundTrip = read . show
```

Now in another module if we have the following:

```haskell
foo :: a -> a
foo = roundTrip
```

GHC will complain about the missing `Serialise a` constraint (mentioning it can
be fulfilled with `Show a` and `Read a` fulfilled) instead of skipping straight
to constraints `Serialise` depends on.
This means we can use `Serialise` as a proper abstraction over what it is a
synonym for instead of merely being a helper to write many constraints more
concisely.

Thanks to [*Csongor Kiss*](https://github.com/kcsongor) for the ideas making this possible!

## License

This project is licensed under the MIT License.

```
Copyright (c) 2019 Basile Henry
```
