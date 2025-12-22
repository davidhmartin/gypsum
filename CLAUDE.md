# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Gypsum is a generator for Emacs themes based on the principles of Nikita Tonsky's Alabaster color scheme. The Alabaster philosophy emphasizes:
- Minimal syntax highlighting (only highlight what matters)
- Background highlighting for strings and comments rather than foreground colors
- Distinguishing definitions from uses
- Reserved bright colors for errors and warnings

The theory behind Tonsky's approach is described in detail at https://tonsky.me/blog/syntax-highlighting/

## Development Commands

This is an Emacs Lisp project. Common commands:

```bash
# Byte-compile .el files
emacs -Q --batch -f batch-byte-compile *.el

# Run ERT tests
emacs -Q --batch -l ert -l gypsum.el -l gypsum-test.el -f ert-run-tests-batch-and-exit

# Load and test interactively
emacs -Q -l gypsum.el
```

## Architecture Notes

The project should generate Emacs theme files (deftheme) that can be loaded with `load-theme`. Generated themes should support both light and dark variants following Alabaster principles.

## License

GPL-3.0
