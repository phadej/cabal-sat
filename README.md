# Using cabal-install's solver as a SAT solver!?

Dependency resolution in Haskell ecosystem is a hard computational problem.
While I'm unsure how hard problem is to picking individual package versions
without any additional features, selecting assignment of *automatic package flags*
seems to be very hard: it seems we encode arbitrary *boolean satisfiablity problems*, *SAT*,
into automatic package flag selection problem.

Real world flag selection problems are easy.
Yet, I wanted to try how good is `cabal-install`'s solver at problems
it definitely haven't tuned for.

## Boolean satisfiability problems

[From Wikipedia](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem):

> In logic and computer science, the Boolean satisfiability problem is the problem of determining if there exists an interpretation that satisfies a given Boolean formula.

The problems are given to solvers in conjunctive normal form:

```
(x₁ ∨ x₂) ∧ (x₃ ∨ x₄)
```

and the solvers job is to find an assignment making the formula true.
In the example above there are many solutions, e.g. setting all variables to true.

## Sudoku

One of go to examples of what you can do with a SAT solvers is solving sudoku puzzles.

Our running example will be a very simple 2×2 sudoku puzzle.

```
┌─────┬─────┐
│   1 │ 4   │
│   3 │   2 │
├─────┼─────┤
│     │ 3   │
│     │     │
└─────┴─────┘
```

Problem encoding is somewhat of art form, but for sudoku its quite simple.

The problem variables are numbers in cells `i, j`.
We can encode each number using four variables, `x(i,j,k)`, and requiring that exactly one is true.
We could also use only two "bits" to encode four options (so called binary encoding), but using one-bit per option makes easier to encode the sudoku rules.

Recall the sudoku rules: each number have to occur exactly once in each row, column and subsquare.

With our number encoding the puzzle rules are easy to encode.
For example for each row `i` and number `k` we require that exactly one literal in `x(i,j,k), j <- [1..4]` is true
And similarly for columns and subsquares.

For what it's worth, sudoku can be very neatly encoded using `Applicative`s and `Traversable`s. See [the StackOverflow answer](https://stackoverflow.com/a/10242673) by Conor McBride.

SAT solvers consume a DIMACS format which looks like:

```
p cnf 64 453
-60 -64 0
-48 -64 0
-48 -60 0
-44 -64 0
-44 -60 0
-44 -48 0
44 48 60 64 0
-59 -63 0
-47 -63 0
-47 -59 0
-43 -63 0
-43 -59 0
-43 -47 0
43 47 59 63 0
-58 -62 0
...
```
  
A DIMACS file begins with a header line of the form `p cnf <variables> <clauses>`. Where `<variables>` and `<clauses>` are replaced with decimal numbers indicating the number of variables and clauses in the formula.
Following the header line are the clauses of the formula. The clauses are encoded as a sequence of decimal numbers separated by spaces and newlines. For each clause the contained literals are listed followed by a `0`.

The above is beginning of encoding of our sudoku problem.
There are 4 × 4 cells and each number uses 4 variables, so in total there is 64 variables.

The exactly once encoding I used is done using naive (binomial) at most one encoding.
You can see a pattern:

```
-60 -64 0
-48 -64 0
-48 -60 0
-44 -64 0
-44 -60 0
-44 -48 0
44 48 60 64 0
```

The last line is requiring that *at least one* of four variables (`44, 48, 60, 64`) is true.
The first 6 lines are pairwise requirements that *at most one* of the variables is true: n over 2 i.e. 6 pairs.
All of sudoku rules are such exactly one of four constraints, which we have 64 in total: 16 for digits, 4 rows, columns and subsquares with four numbers, 
That is `7 × 64 = 448` clauses.

The final 5 clauses are *initial value constraints*.
As we know 5 numbers, we tell that the following bits must be true.

The `sudoku.cnf` file indeed ends with five *unit* clauses:

```
...
43 0
30 0
23 0
12 0
5 0
```

When we run the SAT solver, e.g. `z3 -dimacs sudoku.cnf` it will
immediately give a solution which looks something like

```
sat
-1 2 -3 -4 5 -6 -7 -8 -9 -10 -11 12 -13 -14 15 -16 -17 -18 -19 20 -21 -22 23 -24 25 -26 -27 -28 -29 30 -31 -32 33 -34 -35 -36 -37 38 -39 -40 -41 -42 43 -44 -45 -46 -47 48 -49 -50 51 -52 -53 -54 -55 56 -57 58 -59 -60 61 -62 -63 -64 
```

For each of 64 variables it prints whether the satisfying assignment for that variable is true (positive) or false (negative).

When we decode the solution we'll get a solved sudoku puzzle:

```
┌─────┬─────┐
│ 2 1 │ 4 3 │
│ 4 3 │ 1 2 │
├─────┼─────┤
│ 1 2 │ 3 4 │
│ 3 4 │ 2 1 │
└─────┴─────┘
```

## Encoding as cabal automatic flags

So how can we encode a SAT problem as flag selection one?

It is hopefully obvious that each variable will be represented by an *automatic* flag:

```cabal
flag 1
  manual: False
  default: False
```

(Yes, flag names can be "numbers", they are still treated as strings).

The default value shouldn't matter, but it's probably better to pick `False`
as most variables in sudoku problem are indeed false.

Let next think how to encode clauses. When the CNF is satisfiable
each clause should evaluate to true. When CNF is *unsatisfiable*
it's enough that *any* clause evaluate to false. Recall clauses are disjunctions
of literals:

```
x₁ ∨ ¬ x₂ ∨ ¬ x₃ ∨ x₄
```

Then we can encode such clause as a conditional in a component stanza of `.cabal` file.
There shouldn't be an install plan if a clause value if false:

```cabal
if !(flag(1) || !flag(2) || !flag(3) || flag(4))
  build-depends: unsatisfiable <0
```

or equivalently:

```cabal
if !flag(1) && flag(2) && flag(3) && !flag(4))
  build-depends: unsatisfiable <0
```


The resulting `.cabal` file library stanza looks like

```cabal
library
  if flag(60) && flag(64)
    build-depends: unsatisfiable <0

  if flag(48) && flag(64)
    build-depends: unsatisfiable <0

  if flag(48) && flag(60)
    build-depends: unsatisfiable <0

  if flag(44) && flag(64)
    build-depends: unsatisfiable <0

  if flag(44) && flag(60)
    build-depends: unsatisfiable <0

  if flag(44) && flag(48)
    build-depends: unsatisfiable <0

  if !flag(44) && !flag(48) && !flag(60) && !flag(64)
    build-depends: unsatisfiable <0

...

  if !flag(43)
    build-depends: unsatisfiable <0

  if !flag(30)
    build-depends: unsatisfiable <0

  if !flag(23)
    build-depends: unsatisfiable <0

  if !flag(12)
    build-depends: unsatisfiable <0

  if !flag(5)
    build-depends: unsatisfiable <0
...

And we can ask `cabal-install` to construct an install plan with

```
cabal build --dry-run
```

On my machine it took 17 seconds to complete.
(I actually don't know what to expect).

`cabal-install` writes out `plan.json` file which contains the install plan.
It's a JSON file which can be read direcly, or queries with [`cabal-plan`](https://hackage.haskell.org/package/cabal-plan) utility.

```
cabal-plan topo --show-flags
```

shows

```
sudoku-0 -1 -10 -11 +12 -13 -14 +15 -16 -17 -18 -19 +2 +20 -21 -22 +23 -24 +25 -26 -27 -28 -29 -3 +30 -31 -32 +33 -34 -35 -36 -37 +38 -39 -4 -40 -41 -42 +43 -44 -45 -46 -47 +48 -49 +5 -50 +51 -52 -53 -54 -55 +56 -57 +58 -59 -6 -60 +61 -62 -63 -64 -7 -8 -9
```

there is only one package in the install plan, and we asked `cabal-plan` to also show the flag assignment.
The output is almost the same as from `z3`!

If we decode this solution, we get the same answer:

```
┌─────┬─────┐
│ 2 1 │ 4 3 │
│ 4 3 │ 1 2 │
├─────┼─────┤
│ 1 2 │ 3 4 │
│ 3 4 │ 2 1 │
└─────┴─────┘
```

## Conclusion

We successfully used `cabal-install` dependency solver as a SAT solver.
It is terribly slow, but it's probably still faster at solving 2×2 sudoku puzzle than myself.
The code is available on [GitHub](https://github.com/phadej/cabal-sat) if you want to play with it.

However, it is not unheard that we need to encode some logical constraints in cabal file.

For example [`transformers-compat`](https://hackage.haskell.org/package/transformers-compat) encodes which `transformers` version it depends on using kind of unary encoding: each bucket is encoded using single flag:
Removing some unrelated bits:

```cabal
...

  if flag(four)
    build-depends: transformers >= 0.4.1 && < 0.5
  else
    build-depends: transformers < 0.4 || >= 0.5

  if flag(three)
    build-depends: transformers >= 0.3 && < 0.4
  else
    build-depends: transformers < 0.3 || >= 0.4

...
```

The choice of `transformers` versions forces assignments to the automatic flags (`four`, `three`, ...)
and then we can alter build info of a package based on that.

That is an indirect way of writing (encoding!) something like

```cabal
  if build-depends(transformers >= 0.4.1 && <0.5)
    ...

  if build-depends(transformers >= 0.3 && <0.4)
    ...
```

A common example in the past was adding `old-locale` dependency when the old version of `time` library was picked:

```cabal
  if flag(old-locale)
    build-depends:
        old-locale  >=1.0.0.2 && <1.1
      , time        >=1.4     && <1.5

  else
    build-depends: time >=1.5 && <1.7
```

which could be written as

```cabal
  build-depends: time >=1.4 && <1.7
  if build-depends(time < 1.5)
    build-depends: old-locale >=1.0.0.2 && <1.1
```

Another example is [`functor-classes-compat`](https://hackage.haskell.org/package/functor-classes-compat`) which also encodes `transformers` and `base` version subsets, but it is using binary encoding of four options.
There the implied constraints are also (hopefully) disjoint making flag assignment deterministic.

I think that automatic flags are good feature to have. It is a basic building block, but is a "low-level" feature.
On the other hand `if build-depends(...)` construct is more difficult to use wrong, and probably covers 99% of the use cases for automatic flags.
If you are mindful that you encoding `if build-depends (...)` constraint, then you'll probably use cabal's automatic flags correctly.
Conversely, if you are using automatic flags to encode something else, most likely you are doing something wrong.
