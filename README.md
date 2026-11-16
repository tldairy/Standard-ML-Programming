# COSC 341 Project 2: Simple ML Operations

The driver runs the specification examples, runs additional edge-case tests, then prompts for a data file and prints its summary.

## Files

- `driver.sml` — function implementations and test driver.
- `data.txt` — sample input file containing one positive integer per line.

## Requirements

- SML/NJ installed and available in the terminal as `sml`.
- Run the program from the project directory so `data.txt` can be found by name.

## Run

Open a terminal in this directory and start SML/NJ:

```text
sml
```

At the SML prompt, load the driver:

```sml
use "driver.sml";
```

The driver runs both test groups, then displays `Enter a file name:`. Enter a file path such as `data.txt` at that prompt. This is a filename prompt, not another SML command. Enter a full path if the file is in a different directory.

## Implemented functions

1. `replaceAll` — replaces each matching list element with two copies of a replacement value.
2. `removeDuplicates` — removes repeated elements while preserving the first occurrence.
3. `circulate` and `circulateDir` — rotate a list one position; `circulateDir` supports left and right, while an unrecognized direction leaves the list unchanged.
4. `largestCount` — returns the maximum and length of a non-empty integer list.
5. `int2String` — converts an integer to its string representation, including negative values.
6. `string2Int` — converts a valid integer string to an integer; negative values use Standard ML's `~` sign (for example, `"~123"`).
7. `removeLastChar` — removes the final character of a string; the empty string remains empty.
8. File-processing helpers — read integer lines into a list and report the list size, largest value, and average.

## Test groups

- **Specification test cases** exercise the examples from the assignment.
- **Additional test cases** cover empty lists, single-element lists, zero, invalid rotation directions, and reading `data.txt`.

The driver prints `PASS` or `FAIL` for each assertion. The file summary also handles an empty file by reporting that no largest value exists and an average of `0.00`. Blank lines in a file are ignored.

## Sample input

`data.txt`:

```text
12
2
100
51
```

For this file, the summary reports a list size of `4`, largest number `100`, and average `41.25`.
