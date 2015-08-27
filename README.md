# markdown-racket
A markdown parser in racket. (for practice use)

## Markdown Syntax
Only the simpliest syntax are going to be supported [Markdown
Basics](https://help.github.com/articles/markdown-basics/). And NO guarantee
of error prone.

## Design and Thinking

The syntax of markdown can be divided into `block element` and `inline
element`.

Currently the parse of `block element` are intended to be implemented in
descent recursive parser. Wile `inline elements` are to be parsed via regular
expression.
