This document has been shamelessly plagiarized from Johan Tibell
[https://github.com/tibbe/haskell-style-guide].

Haskell Style Guide
===================

This is a short document describing the preferred coding style for
this project.  I've tried to cover the major areas of formatting and
naming.  When something isn't covered by this guide you should stay
consistent with the code in the other modules.

Formatting
----------

### Line Length

Maximum line length is *80 characters*.

### Indentation

Tabs are illegal. Use spaces for indenting.  Indent your code blocks
with *2* or *4 spaces*.  Indent the `where` keyword two spaces to set it
apart from the rest of the code and indent the definitions in a
`where` clause 2 spaces. Some examples:

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
```

Observe the _Pfenning principle_.  Indentation should be stable
under renaming of identifiers (alpha-equality).
This means a line break before extra indentation.

The following indentation scheme is not stable under renaming of `filter`.
```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter _ []                 = []
filter p (x:xs) | p x       = x : filter p xs
                | otherwise = filter p xs
```

### Blank Lines (Not enforced)

One blank line between top-level definitions.  No blank lines between
type signatures and function definitions.  Add one blank line between
functions in a type class instance declaration if the functions bodies
are large.  Use your judgement.

### Whitespace

Surround binary operators with a single space on either side.  Use
your better judgement for the insertion of spaces around arithmetic
operators but always be consistent about whitespace on either side of
a binary operator.  Insert a space after a lambda.

### Data Declarations

Align the constructors in a data type definition.
Align the separating vertical bar `|` with the `=`.
Add haddockumentation to the type (prefix) and each constructor (postfix).
The constructor documentation should be on the following line.
Very short documentation can be directly after the constructor (cf. records).
Example:

```haskell
-- | Strict binary trees.
data Tree a
  = Branch !a !(Tree a) !(Tree a)
      -- ^ Labelled binary node.
  | Leaf
      -- ^ Empty tree.
  deriving (Eq, Ord)
```

Format records as follows:

```haskell
-- | Personal data used by the simple match-maker.
data Person = Person
  { firstName :: !String  -- ^ First name.
  , lastName  :: !String  -- ^ Last name.
  , age       :: !Int     -- ^ Age.
  } deriving (Eq, Show)
```

Document each field.  End documentation with a dot.

### List Declarations

Align the elements in the list.  Example:

```haskell
exceptions =
  [ InvalidStatusCode
  , MissingContentHeader
  , InternalServerError
  ]
```

### Pragmas (Not enforced)

Put pragmas immediately following the function they apply to.
Example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In the case of data type definitions you must put the pragma before
the type it applies to.  Example:

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### Hanging Lambdas

You may or may not indent the code following a "hanging" lambda.  Use
your judgement. Some examples:

```haskell
bar :: IO ()
bar = do
  forM_ [1, 2, 3] $ \ n -> do
    putStrLn "Here comes a number!"
    print n

foo :: IO ()
foo = do
  alloca 10 $ \ a ->
  alloca 20 $ \ b ->
  cFunction a b
```

### Export Lists

Format export lists as follows:

```haskell
module Data.Set
  (
    -- * The @Set@ type
    Set
  , empty
  , singleton

    -- * Querying
  , member
  ) where
```

### If-then-else clauses (Not enforced)

Generally, guards and pattern matches should be preferred over if-then-else
clauses, where possible.  Short cases should usually be put on a single line
(when line length allows it).

When writing non-monadic code (i.e. when not using `do`) and guards
and pattern matches can't be used, you can align if-then-else clauses
you like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, you should be consistent with the 4-spaces indent rule, and the
`then` and the `else` keyword should be aligned.  Examples:

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

The same rule applies to nested do blocks:

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

### Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or as

```haskell
foobar =
  case something of
    Just j  -> foo
    Nothing -> bar
```

Align the `->` arrows when it helps readability.

Imports
-------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically, by module name.

A typical module header looks like this.

```haskell
-- LANGUAGE pragmas

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- | Module haddockumentation.

 This module demonstrates some standard practice.
 -}

-- Control.* imports

import Control.Applicative hiding (empty)
import Control.Monad.Reader

-- Data.* imports

import Data.Foldable as Fold
import Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Seq (Seq)
import qualified Data.Seq as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Traversable as Trav

-- other external imports

import Test.QuickCheck

-- Agda imports in groups

import Agda.Interaction.Options

import qualified Agda.Syntax.Abstract as A
import Agda.Syntax.Common as Common
import qualified Agda.Syntax.Concrete as C
import qualified Agda.Syntax.Concrete.Name as C
import Agda.Syntax.Internal as I

import Agda.TypeChecking.Monad as TCM
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Substitute

-- last: Agda.Utils.*

import Agda.Utils.Lens
import Agda.Utils.Maybe
import Agda.Utils.Null
import Agda.Utils.Tuple

-- very last: Agda.Utils.Impossible

#include ../../undefined.h
import Agda.Utils.Impossible
```

All the short module names given in the above example are mandatory:
`Map`, `Set`, `Seq` (these should also import the type separately
unqualified, to avoid things like `Map.Map`), `Fold`, `Trav`, and the
Agda-specific `Common`, `C`, `A`, `I` for syntax and `TCM` for the
type-checking monad.

Structuring module content
--------------------------

Put the main function of the module first.

Structure the auxiliary functions into sensible groups.  Start a topic
with a heading like this:

```haskell
------------------------------------------------------------------------
-- * Monoid structure on positions.
------------------------------------------------------------------------
```

This means 72 dashes, a haddock heading, and again 72 dashes.  Haddock
subheadings can also be used.

Put boring instances very last in the file.  (Like `KillRange` instances.)

Comments
--------

### Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### Top-Level Definitions

Comment every top level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments format them like so:

```haskell
data Record = Record
  { field1 :: !Text
      -- ^ This is a very very very long comment that is split over
      --   multiple lines.
  , field2 :: !Int
      -- ^ This is a second very very very long comment that is split
      --   over multiple lines.
  }
```

### End-of-Line Comments

Separate end-of-line comments from the code using 2 spaces.  Align
comments for data type definitions.  Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### Links (Not enforced)

Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  We therefore recommend adding a link to an API name
if:

* The user might actually want to click on it for more information (in
  your judgment), and

* Only for the first occurrence of each API name in the comment (don't
  bother repeating a link)

Naming
------

Use camel case (e.g. `functionName`) when naming functions and upper
camel case (e.g. `DataType`) when naming data types.

For readability reasons, don't capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: Two letter abbreviations, e.g. `IO`.

### Modules

Use singular when naming modules e.g. use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

Dealing with laziness (Not enforced)
---------------------

By default, use strict data types and lazy functions.

### Data types

Constructor fields should be strict, unless there's an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the number of brain cycles the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you can put

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file. Including this flag in the file inself instead
of e.g. in the Cabal file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e. the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g. `Double` or `Int`). If you're using GHC 7.4 or
later you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### Functions

Have function arguments be lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

## Point-free style ##

Avoid over-using point-free style. For example, this is hard to read:

```haskell
-- Bad:
f = (g .) . h
```

Use the application operator `$` to reduce parentheses, especially
long ranging-ones.

Monadic programming
-------------------

### Loops ###

Use `mapM` and `mapM_` when the loop-body is short.  Use `forM` and
`forM_` for large loop bodies.

```haskell
-- Bad:
constructorApplications :: [(I.Arg Term, I.Dom Type)] -> TCM (Maybe [Nat])
constructorApplications args = do
  xs <- mapM (\(e, t) -> do
                 t <- reduce (unDom t)
                 constructorApplication (unArg e) (ignoreSharingType t))
             args
  return (concat <$> sequence xs)

-- Good:
constructorApplications :: [(I.Arg Term, I.Dom Type)] -> TCM (Maybe [Nat])
constructorApplications args = do
  xs <- forM args $ \ (e, t) -> do
    t <- reduce $ unDom t
    constructorApplication (unArg e) $ ignoreSharingType t
  return $ concat <$> sequence xs
```

### Case on monadic computations ###

Use the extra monadic combinators in `Agda.Utils.Monad`, `Agda.Utils.Maybe` etc.

```haskell
callCompiler
  :: FilePath  -- ^ The path to the compiler.
  -> [String]  -- ^ Command-line arguments.
  -> TCM ()
callCompiler cmd args = do
  merrors <- callCompiler' cmd args
  case merrors of
    Nothing     -> return ()
    Just errors -> typeError (CompilationError errors)
```

Rather than a binding followed by a trivial case,
use a version of `whenM` for the `Maybe` type:

```haskell
callCompiler cmd args = do
  whenJustM (callCompiler' cmd args) $ \ errors -> do
    typeError $ CompilationError errors
```

Saves a volatile binding `merrors` and a boring `return ()`.
Note that a further eta-contraction would remove the binding `errors`
which actually helps reading the code.

```haskell
callCompiler cmd args = do
  whenJustM (callCompiler' cmd args) $ typeError . CompilationError
```


Misc
----

### Warnings ###

Agda uses a subset of the GHC warnings as errors.  See Agda.cabal.

