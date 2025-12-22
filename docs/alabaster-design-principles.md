# Alabaster Theme Design Principles

A comprehensive guide to creating Emacs themes following Nikita Tonsky's minimal syntax highlighting philosophy.

## Core Philosophy

> "If everything is highlighted, nothing is highlighted."

Traditional syntax highlighting colors almost every token in code—keywords, variables, operators, punctuation, function calls. This approach defeats the purpose of highlighting because nothing stands out. Alabaster takes a radically different approach: **highlight sparingly so that colors become meaningful**.

## The Four Categories

Alabaster highlights exactly **four semantic categories**. This minimal palette is small enough to memorize, making color-based lookup practical ("I'm looking for a string, so I scan for green").

### 1. Strings (Green)
- String literals
- Regular expressions
- Character literals
- Symbols (in Lisp-family languages)

**Rationale**: Strings are used sparingly and serve as logical reference points in code. They're often what you're searching for.

### 2. Constants (Magenta/Lavender)
- Numbers
- Booleans (`true`, `false`, `nil`)
- Language-defined constants
- Escape sequences

**Rationale**: Like strings, constants appear infrequently and mark specific values worth noticing.

### 3. Comments (Red/Yellow - PROMINENT)
- Single-line comments
- Block comments
- Documentation strings

**Rationale**: This breaks tradition—most themes dim comments to gray. Tonsky argues: "If someone took the time to write it, you should read it." Good comments deserve visibility. The gray-comment convention stems from an era when comments were often outdated noise.

*Note*: Distinguish between disabled code (which could be dimmed) and explanatory comments (which should be vibrant).

### 4. Definitions (Blue)
- Function definitions
- Class/type definitions
- Variable declarations at definition site
- Top-level bindings

**Rationale**: Definitions reveal code structure. When scanning a file, you want to quickly identify what's being defined. This is different from *uses* of those definitions.

## What NOT to Highlight

Everything else uses the default foreground color. **This is intentional.**

### Keywords - NO highlighting
- `if`, `else`, `while`, `for`, `return`, `def`, `class`, etc.
- Tonsky: "You rarely look for keywords. The meaningful part is the condition or expression, not the keyword itself."

### Variables and Function Calls - NO highlighting
- These comprise ~75% of typical code
- Highlighting them creates visual noise without adding information
- The variable/function *name* is what matters, not that it *is* a variable

### Operators and Punctuation - NO or minimal highlighting
- Use base foreground color or slightly dim (gray) for punctuation
- Let the actual names stand out

### Properties and Parameters - NO highlighting
- These are uses, not definitions

## Color Selection Guidelines

### Minimal Palette
Use **4 colors maximum** for semantic highlighting. This ensures:
- Colors remain memorable
- Each color carries clear meaning
- Visual scanning becomes efficient

### Avoid
- Excessive colors (destroys memorability)
- Bold and italic variations (redundant highlighting methods)
- Mathematically "perfect" color schemes (aesthetics trump uniformity)

### Light Theme Colors (from reference)
```
Background:    #F7F7F7 (off-white)
Foreground:    #000000 (black)
Dimmed:        #777777 (gray for punctuation, line numbers)

String:        #448C27 (green)
Constant:      #7A3E9D (magenta)
Comment:       #AA3731 (red - prominent!)
Definition:    #325CC0 (blue)
```

### Dark Theme Colors (from reference)
```
Background:    #0E1415 (very dark)
Foreground:    #CECECE (light gray)
Dimmed:        #6E7B7C (gray)

String:        #99C592 (muted green)
Constant:      #9999FF (lavender)
Comment:       #DFDF8E (yellow - prominent!)
Definition:    #71ADE7 (light blue)
```

### Color Psychology
Dark themes have an advantage: the light spectrum offers more vibrant, distinguishable colors against a dark background.

Light themes can succeed by using **background colors** for highlighting—lighter colors fill more area and remain readable on white.

## UI Color Categories

Beyond the four semantic categories, themes need colors for:

### Status Colors
- **Error**: Bright red (`#E51400` light / `#F14C4C` dark)
- **Warning**: Orange (`#F09000` light / `#CCA700` dark)
- **Success**: Green (can reuse string color)

### Selection/Highlight
- Region selection: Muted blue (`#BFDBFE` light / `#264F78` dark)
- Current line: Slightly darker/lighter than background
- Search match: Yellow/orange background (`#FFBC5D`)

### Diff Colors
- Added: Green-tinted background
- Removed: Red-tinted background
- Changed: Yellow-tinted background

## Emacs Implementation Notes

### Emacs 29+ Font Lock Faces
Modern Emacs distinguishes between:
- `font-lock-function-name-face` (definitions) vs `font-lock-function-call-face` (uses)
- `font-lock-variable-name-face` (definitions) vs `font-lock-variable-use-face` (uses)
- `font-lock-property-name-face` vs `font-lock-property-use-face`

This aligns perfectly with Alabaster philosophy:
- **Definition faces** → use the `definition` color (blue)
- **Use faces** → use default foreground (no highlighting)

### Tree-sitter Faces
For tree-sitter based highlighting, apply the same logic:
- `tree-sitter-hl-face:function` (definition) → blue
- `tree-sitter-hl-face:function.call` (use) → default
- `tree-sitter-hl-face:keyword` → default (NOT highlighted)

### Face Categories for Theme Implementation

```elisp
;; Highlighted (the four categories)
font-lock-string-face           → string color
font-lock-constant-face         → constant color
font-lock-number-face           → constant color
font-lock-builtin-face          → constant color
font-lock-comment-face          → comment color
font-lock-doc-face              → comment color
font-lock-function-name-face    → definition color
font-lock-variable-name-face    → definition color
font-lock-type-face             → definition color

;; NOT highlighted (use default foreground)
font-lock-keyword-face          → fg
font-lock-operator-face         → fg
font-lock-function-call-face    → fg
font-lock-variable-use-face     → fg
font-lock-property-name-face    → fg
font-lock-property-use-face     → fg
font-lock-preprocessor-face     → fg

;; Dimmed
font-lock-punctuation-face      → fg-dim
font-lock-delimiter-face        → fg-dim
font-lock-bracket-face          → fg-dim
```

## Generating Themes from a Base Color

To create an Alabaster-style theme from a starting color:

1. **Choose background first**: Light theme (`#F0-F8`) or dark theme (`#0E-1A`)
2. **Derive foreground**: High contrast (black for light, light gray for dark)
3. **Select four semantic colors** that:
   - Are visually distinct from each other
   - Work well on your background
   - Have adequate contrast for readability
4. **Generate derived colors**:
   - `fg-dim`: Midpoint between fg and bg
   - `bg-alt`: Slightly darker (light theme) or lighter (dark theme) than bg
   - Status colors: Conventional meanings (red=error, yellow=warning, green=success)
   - Selection: Muted blue that doesn't clash with semantic colors

## References

- [Tonsky's Blog Post on Syntax Highlighting](https://tonsky.me/blog/syntax-highlighting/)
- [Alabaster Theme for VS Code](https://github.com/tonsky/vscode-theme-alabaster)
- [Alabaster Theme for Emacs (Reference Implementation)](https://github.com/davidhmartin/alabaster-theme-emacs)
