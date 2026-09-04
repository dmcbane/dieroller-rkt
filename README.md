# Die Roller

[![CI](https://github.com/dmcbane/dieroller-rkt/actions/workflows/ci.yml/badge.svg)](https://github.com/dmcbane/dieroller-rkt/actions/workflows/ci.yml)

Die Roller is a simple command line die roller for desktop RPG players. It provides precise control over die rolls via
command line parameters.

```
dieroller [ <option> ... ] <roll>

 where <option> is one of
  -v, --verbose : Show the notation and the dice that were kept.
  -V, --version : Show the version
  -h, --help : Show this help
```

A roll may be repeated with `6x4d6k3` and reduced to a single number with `sum(6x4d6k3)`; see
[Aggregating repeated rolls](#aggregating-repeated-rolls).

A roll is written entirely in dice notation, as a single argument. The flags
that used to describe the dice (`--dice`, `--sides`, `--keep`, `--modifier`,
`--iterations`) and the `<dice> <sides> <modifier> <keep>` positional form are
gone; the notation says all of it.

## Dice notation

```
roll       := aggregate '(' repeated ')'
            | aggregate ':' repeated
            | repeated
repeated   := (integer 'x')? expression
aggregate  := 'sum' | 'avg' | 'high' | 'low' | 'median'   -- or an alias
expression := term (('+' | '-') term)* ('*' integer)?
term       := dice | integer
dice       := integer 'd' integer selector?
selector   := 'k' ('h' | 'l')? integer     -- keep, defaulting to highest
            | 'd' ('h' | 'l')  integer     -- drop
```

| notation | meaning |
|---|---|
| `1d20` | one twenty-sided die |
| `5d20` | five of them |
| `3d6+3` | three six-sided dice, plus three |
| `4d6k3`, `4d6kh3` | keep the highest three of four |
| `2d20kh1` | advantage |
| `2d20kl1` | disadvantage |
| `4d6dl1` | drop the lowest (the same roll as `4d6k3`) |
| `4d6dh1` | drop the highest |
| `2d6+1d8-1` | several groups and constants |
| `3d6*2` | double the total of the kept dice |
| `6x4d6k3` | roll the same expression six times |
| `sum(6x4d6k3)` | add those six rolls together |
| `sum:6x4d6k3` | the same, with nothing for a shell to eat |
| `avg:100x1d20` | the average of a hundred rolls |
| `max:2x1d20` | the better of two rolls |

A modifier applies to the sum of the kept dice, not to each die, so `3d6*2` doubles the total rather than rolling
`3d12`. Drop always needs its direction letter, since a bare `d` already separates dice from sides; dropping is stored
as keeping from the other end, so `4d6dl1` and `4d6k3` are the same roll and both display as `4D6K3`.

The repeat count is not part of the expression and does not appear in the rendered notation, which describes a single
roll. Each dice group gets its own parentheses in verbose output:

```console
$ dieroller 6x4d6k3 --verbose
4D6K3 (5 4 4) => 13
4D6K3 (5 2 1) => 8
4D6K3 (5 3 1) => 9
4D6K3 (3 2 2) => 7
4D6K3 (6 5 4) => 15
4D6K3 (4 3 2) => 9

$ dieroller -v 2d6+1d8-1
2D6+1D8-1 (5 2) (6) => 12

$ dieroller "2d6 + 1d8"          # quote it if you write spaces
14
```

### Aggregating repeated rolls

`6x4d6k3` reports six rolls. An aggregate wraps the whole thing and reports one number instead:

| aggregate | aliases | what it reports |
|---|---|---|
| `sum` | `total` | every roll added together |
| `avg` | `average`, `mean` | their average, rounded to two places |
| `high` | `highest`, `max` | the best of them |
| `low` | `lowest`, `min` | the worst of them |
| `median` | `med` | the middle one |

Most shells treat unquoted parentheses as syntax of their own -- fish reads `(...)` as command substitution, bash as a
subshell -- so either quote the whole roll or use the colon form, which parses identically:

```console
$ dieroller "sum(6x4d6k3)"
71

$ dieroller sum:6x4d6k3
68

$ dieroller -v "sum(6x4d6k3)"
4D6K3 (6 4 4) => 14
4D6K3 (6 4 2) => 12
4D6K3 (1 1 1) => 3
4D6K3 (6 6 4) => 16
4D6K3 (5 3 2) => 10
4D6K3 (5 4 3) => 12
SUM(6x4D6K3) => 67

$ dieroller avg:100x1d20
11.14
```

Unlike the repeat count, an aggregate *does* appear in the rendered notation, and it takes the repeat with it: a sum of
six rolls is a property of all six, not of any one of them, so it renders as `SUM(6x4D6K3)`. Aliases canonicalise the
way `kh` does, so `max:2x1d20` renders as `HIGH(2x1D20)`.

An average is rounded to two places, in exact rational arithmetic rather than by scaling a flonum. Forty rolls
totalling three average exactly 0.075, which rounds to `0.08`; the nearest double to 0.075 is a hair below it, so
rounding a flonum would report `0.07` instead. The Elixir port rounds in whole hundredths for the same reason, so the
two agree on every value.

An aggregate needs every roll before it can report anything, so unlike a plain repeat it does not stream. Under
`--verbose` the rolls still appear as they are made and the summary follows them.

### Migrating from the old arguments

The removed forms report their notation equivalent rather than failing blankly:

```console
$ dieroller 3 6 +6 2
the <dice> <sides> <modifier> <keep> arguments have been replaced by dice notation; try: dieroller 3d6k2+6

$ dieroller 2d6 + 1d8
a roll is one argument; quote the whole expression, for example: dieroller "2d6 + 1d8"
```

## Pathfinder character generator

The repository also contains a Pathfinder character generator that will generate random characters using the classic,
standard, heroic, pool, and purchase methods.  The command line options below provide control over how characters are
generated.

```
pathfinder-character [ <option> ... ] [<arguments>] ...

  Examples:

    pathfinder-character --classic -v --number 10
    pathfinder-character -s -n 3

 where <option> is one of
/ -c, --classic : The classic method: 3D6 per ability.
| -s, --standard : The standard method: 4D6 keep high 3 per ability.
|   (this is the default)
| -r, --heroic : The heroic method: 2D6 plus 6 per ability.
| -l <diceperability>, --pool <diceperability> : The pool method: 24D6 for all 6 abilities. The parameter
|   specifies how many dice are assigned to each ability as
|   follows: 3/3/3/3/3/9 with a minimum of 3 dice per ability.
| -p <purchasetype>, --purchase <purchasetype> : The purchase method: parameters are set according to cost.
|   The parameter specifies the purchase type as follows: low,
|   standard, high, and epic fantasy which provides 10, 15, 20,
\   and 25 purchase points respectively.
  -v, --verbose : Display additional information (default to false).
  -n <n>, --number <n> : Number of characters to roll. Must be greater than 0.
    (default to 1)
  --help, -h : Show this help
```

Characters are listed weakest first, so the best roll is the last line on screen.

## Ability score tables

```console
$ racket all_ability_scores.rkt --out ./csv [--legal-only]
```

Writes `legal_scores.csv` (12,376 spreads of scores 7-18) and `uniq_scores.csv` (15,890,700 spreads of scores 1-45),
each row carrying total purchase cost, total ability bonus, and the six scores.

Earlier versions also wrote `all_scores.csv`, one row per *ordering* of six scores. That is 45^6, roughly 8.3 billion
rows, which does not finish in any practical time. Neither cost nor bonus depends on ability order, so every one of
those rows duplicated a spread already present in `uniq_scores.csv`; it is no longer generated.

## Building

Both dieroller and pathfinder-character are written in Racket for cross platform availability and ease of development.
(To be more accurate, it's because I enjoy functional programming.)

[Tagged releases](https://github.com/dmcbane/dieroller-rkt/releases) carry prebuilt Linux x86-64 executables and a
`SHA256SUMS` file, attached by CI from the same build it smoke tested.

No external packages are required. To build your own, from the command line:

```console
$ ./build.sh
```

or individually:

```console
$ raco exe dieroller.rkt
$ raco exe pathfinder-character.rkt
$ raco exe all_ability_scores.rkt
```

## Testing

```console
$ raco test tests.rkt
```

Covers the notation parser, the roll mechanics, the ability tables, the combinatorics, and the purchase table, plus end
to end runs of both command line programs.

## Layout

The programs share three library modules:

| file | contents |
|---|---|
| `notation.rkt` | dice notation parsing, rendering, and rolling |
| `abilities.rkt` | ability cost and bonus tables, combinations, the purchase table |
| `cli-args.rkt` | argument reordering and usage failure reporting |
| `version.rkt` | the version both programs report |

`abilities.rkt` exists because the two ability score tables were previously duplicated verbatim in
`pathfinder-character.rkt` and `all_ability_scores.rkt`.

## Recent fixes

- Options may now appear after positional arguments. `dieroller 5 -v` previously read `-v` as the `<sides>` argument
  and died with a contract violation.
- Validation errors now go to stderr and exit non-zero, so a shell pipeline can tell that a run failed. They previously
  went to stdout and exited 0.
- `pathfinder-character -v` no longer prints a stray `'((7 9 9 4 10 15) ...)` line after the report; that was the
  return value of a `map` leaking out as the module's result.
- Purchase characters no longer always have their abilities in descending order, which made STR the best stat every
  time. Spreads are stored sorted so they deduplicate as multisets, and are now shuffled when one becomes a character.
- `--keep 0` is rejected rather than silently becoming `--keep <dice>`, as the help text always said it would be.
- An unrecognised `--purchase` type is rejected rather than silently becoming `low`.
- `pathfinder-character` without `--verbose` prints one character per line rather than a single raw list-of-lists.
- The purchase table is built from combinations with repetition, C(17,6) = 12,376 spreads, rather than a 12^6 =
  2,985,984 nested-loop brute force deduplicated through a set. This also removed the `memoize` dependency.
- `-i` was mistaken for a value rather than a flag, because Racket reads `-i` as the imaginary unit and the check for
  "is this a negative number" used `string->number`.
- `dieroller`'s dice-describing flags and positional arguments were removed in favour of the notation, which expresses
  all of them. `--version` was added to both programs.
- A repeated roll can be reduced to one number: `sum(6x4d6k3)`, `avg:100x1d20`, `high:2x1d20`.

## See also

[dieroller-elixir](https://github.com/dmcbane/dieroller-elixir) is an Elixir port of these programs. The two accept the
same arguments and produce the same output; the Elixir version additionally offers `--json` and `--seed`. The purchase
tables of the two implementations have been diffed and are byte-identical.
