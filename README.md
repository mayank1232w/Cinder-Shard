# Cinder

A small language with C-style syntax, written in OCaml. I'm building Cinder from scratch to learn how programming languages work, starting with an interpreter and adding features as I go.

Right now it evaluates integer expressions. It has a handwritten lexer, a recursive descent parser, and a tree-walking evaluator.

## Running

You'll need OCaml and Dune 3.0 or newer installed. From the project directory:

```sh
dune build
dune exec bin/main.exe
```

Enter one expression per line, ending with a semicolon. Type `quit` to exit.

```text
Enter an expression: 2 + 3 * 4;
14
Enter an expression: (2 + 3) * 4;
20
Enter an expression: -(12 / 3);
-4
Enter an expression: quit
```

## What works

- Integer literals
- Addition, subtraction, multiplication, and division
- Operator precedence and left-to-right grouping for operators at the same precedence
- Parenthesized expressions
- Unary minus, including expressions like `2 * -3` and `-(2 + 3)`
- An interactive prompt

Division uses integer arithmetic, so `7 / 2;` evaluates to `3`. Error handling is still basic: invalid input or division by zero currently ends the program.

## Implementation

The implementation is in [`bin/main.ml`](bin/main.ml). Source text is split into tokens, parsed into an expression tree, then evaluated.

The parser has a separate function for each level of precedence:

```text
expression  → addition and subtraction
term        → multiplication and division
unary       → negation
primary     → numbers and parenthesized expressions
```

## Next steps

I'm planning to add better error messages, variables, and C-style statements, followed by comparisons, conditionals, loops, and functions. The syntax is still taking shape.
