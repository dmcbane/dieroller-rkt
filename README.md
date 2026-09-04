# Die Roller

[![CI](https://github.com/dmcbane/dieroller-rkt/actions/workflows/ci.yml/badge.svg)](https://github.com/dmcbane/dieroller-rkt/actions/workflows/ci.yml)

Die Roller is a simple command line die roller for desktop RPG players. It provides precise control over die rolls via
command line parameters.

```
dieroller [ <option> ... ] [<arguments>] ...

  where the <arguments> are

    <notation>
  or
    <dice>
  or
    <dice> <sides>
  or
    <dice> <sides> <modifier>
  or
    <dice> <sides> <modifier> <keep>

  See the --dice, --sides, and --modifier parameters for details.

  Examples:

    dieroller 5
    dieroller 1 10
    dieroller 3 6 +3
    dieroller 3 6 +6 2
    dieroller 4d6k3
    dieroller 2d20kl1
    dieroller 4d6dl1
    dieroller 2d6+1d8-1
    dieroller --dice 5 --sides 100 --modifier +4 --keep 3
    dieroller --dice 4 --sides 6 --keep 3

 where <option> is one of
  -v, --verbose : Display additional information (default to false).
  -d <dice>, --dice <dice> : Number of dice to roll.  Must be greater than 0.
    (default to 1)
  -k <keep>, --keep <keep> : Number of rolls to keep. Must be greater than 0 and less than or equal to <dice>.
    (default to number of dice)
  -m <modifier>, --modifier <modifier> : Modifier to the rolls. The first character can optionally
    be one of +, -, or * followed by a number.  If the +, -, or
    * are missing, + is assumed. (default to no modifier)
  -s <sides>, --sides <sides> : Number of sides per die. Must be greater than 0.
    (default to 20)
  -i <iterations>, --iterations <iterations> : Number of times to repeat the same rolls.  Must be greater than 0.
    (default to 1)
  --help, -h : Show this help
```

## Dice notation

A single argument in standard dice notation replaces the positional form.

```
expression := term (('+' | '-') term)* ('*' integer)?
term       := dice | integer
dice       := integer 'd' integer selector?
selector   := 'k' ('h' | 'l')? integer     -- keep, defaulting to highest
            | 'd' ('h' | 'l')  integer     -- drop
```

| notation | meaning |
|---|---|
| `3d6` | roll three six-sided dice |
| `4d6k3`, `4d6kh3` | keep the highest three |
| `2d20kl1` | keep the lowest -- rolling with disadvantage |
| `2d20kh1` | keep the highest -- rolling with advantage |
| `4d6dl1` | drop the lowest (the same roll as `4d6k3`) |
| `4d6dh1` | drop the highest |
| `2d6+1d8-1` | several groups and constants |
| `3d6*2` | double the total of the kept dice |

Drop always needs its direction letter, since a bare `d` already separates dice from sides. Dropping is stored as
keeping from the other end, so `4d6dl1` and `4d6k3` are the same roll and both display as `4D6K3`.

The modifier applies to the sum of the kept dice, not to each die, so `3d6*2` doubles the total rather than rolling
`3d12`. Each dice group gets its own parentheses in verbose output:

```console
$ dieroller -v 2d6+1d8-1
2D6+1D8-1 (5 2) (6) => 12
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

No external packages are required. From the command line:

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

## See also

[dieroller-elixir](https://github.com/dmcbane/dieroller-elixir) is an Elixir port of these programs. The two accept the
same arguments and produce the same output; the Elixir version additionally offers `--json` and `--seed`. The purchase
tables of the two implementations have been diffed and are byte-identical.
