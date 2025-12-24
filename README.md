# Gypsum

An Emacs theme generator based on Nikita Tonsky's [Alabaster](https://github.com/tonsky/vscode-theme-alabaster) minimal syntax highlighting philosophy.

## Philosophy

> "If everything is highlighted, nothing is highlighted."

Traditional syntax highlighting colors almost every token—keywords, variables, operators, punctuation. This defeats the purpose because nothing stands out. Gypsum takes a different approach: **highlight only what matters**.

Gypsum themes highlight exactly **four semantic categories**:

1. **Strings** (green) - literals, regexps, symbols
2. **Constants** (purple) - numbers, booleans, language constants
3. **Comments** (red/peach) - prominently, NOT dimmed
4. **Definitions** (blue) - function/class/variable declarations

Everything else—keywords, operators, variable uses, function calls—uses the default foreground color. This is intentional.

## Installation

### Using straight.el with use-package

```elisp
(use-package gypsum
  :straight (:type git :host github :repo "davidhmartin/gypsum"))
```

### Manual Installation

Clone the repository and add to your load-path:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/gypsum")
(require 'gypsum)
```

## Quick Start

Run `M-x gypsum` and follow the prompts. The interactive wizard guides you through:

1. **Choose a source** - Start from a preset or generate from a seed color
2. **Transform** - Apply transformations with live preview and undo support
3. **Generate** - Name and save your theme

## Interactive Usage

### Main Command: `M-x gypsum`

The primary way to create themes. Features:

- **Live preview** - See changes immediately as you work
- **Transform chaining** - Apply multiple transformations in sequence
- **Undo support** - Back out of any transformation
- **Auto-cleanup** - Original theme restored if you cancel

#### Workflow

```
Source Selection
    ↓
[Auto-preview]
    ↓
Transform Loop:
    → "Add transform" - apply a transformation
    → "Undo last transform" - revert to previous state
    → "Done - proceed to generate" - finish
    ↓
Name & Save → Load
```

### Available Transformations

| Transform | Description |
|-----------|-------------|
| `hue-shift` | Rotate all semantic colors by N degrees |
| `set-color` | Change a specific color (string, constant, comment, or definition) |
| `blend` | Blend all colors toward an accent color |
| `derive-dark` | Create dark variant from light palette |
| `derive-light` | Create light variant from dark palette |
| `change-background` | Change background, auto-adjusting other colors |

### Other Interactive Commands

| Command | Description |
|---------|-------------|
| `M-x gypsum-from-source` | Quick generation from a preset or discovered theme |
| `M-x gypsum-show-palette` | Display palette colors for any source |
| `M-x gypsum-preview-dismiss` | Dismiss active preview |
| `M-x gypsum-discover-refresh` | Rescan for gypsum-generated themes |

### Built-in Presets

Gypsum includes curated presets ready to use or customize:

- `alabaster-light` - Classic Alabaster light theme
- `alabaster-dark` - Classic Alabaster dark theme

Previously generated gypsum themes are automatically discovered and available as sources.

## Programmatic Usage

### Generate from a Preset

```elisp
(gypsum-generate-from-palette
  "my-alabaster"
  (gypsum-preset-get 'alabaster-light))
```

### Transform a Preset

```elisp
;; Hue-shift by 30 degrees
(gypsum-generate-from-palette
  "shifted"
  (gypsum-palette-tint
    (gypsum-preset-get 'alabaster-light)
    :mode 'hue-shift :degrees 30))

;; Change the definition color
(gypsum-generate-from-palette
  "custom-def"
  (gypsum-palette-tint
    (gypsum-preset-get 'alabaster-dark)
    :mode 'set-color :key :definition :color "#7C3AED"))

;; Derive dark variant from light
(gypsum-generate-from-palette
  "my-dark"
  (gypsum-palette-derive-dark
    (gypsum-preset-get 'alabaster-light)))
```

### Generate from Seed Color

```elisp
(gypsum-generate-from-palette
  "seeded"
  (gypsum-palette-generate "#5E81AC" 'dark))
```

The seed color becomes the definition color. Other semantic colors are derived via color wheel relationships.

## Configuration

### Output Directory

By default, themes are saved to `custom-theme-directory` (typically `~/.emacs.d/`), which is already in `custom-theme-load-path` so generated themes work immediately with `M-x load-theme`.

To use a different directory:

```elisp
(setq gypsum-output-directory "~/my-themes/")
(add-to-list 'custom-theme-load-path "~/my-themes/")
```

### Auto-Preview

Live preview is enabled by default. To disable:

```elisp
(setq gypsum-auto-preview nil)
```

### Author Name

Set the author name in generated theme headers:

```elisp
(setq gypsum-author-name "Your Name")
```

## API Reference

### Interactive Commands

- `gypsum` - Main interactive wizard with preview and undo
- `gypsum-from-source` - Quick generation from preset/discovered theme
- `gypsum-show-palette` - Display palette colors
- `gypsum-preview-dismiss` - Dismiss active preview
- `gypsum-discover-refresh` - Refresh discovered themes cache

### Theme Generation

- `gypsum-generate-from-palette` - Generate theme file from a palette

### Palette Operations

- `gypsum-palette-tint` - Tint a palette (hue-shift, set-color, blend)
- `gypsum-palette-derive-dark` - Derive dark variant from light
- `gypsum-palette-derive-light` - Derive light variant from dark
- `gypsum-palette-generate` - Generate palette from seed color
- `gypsum-palette-change-background` - Change background with auto-adjustment

### Sources

- `gypsum-preset-get` - Get a preset palette by name
- `gypsum-preset-list` - List available presets
- `gypsum-sources-list` - List all sources (presets + discovered)
- `gypsum-discover-themes` - Discover gypsum themes in load path

### Color Utilities

- `gypsum-color-rotate` - Rotate hue
- `gypsum-color-lighten` / `gypsum-color-darken`
- `gypsum-color-saturate` / `gypsum-color-desaturate`
- `gypsum-color-blend` - Blend two colors
- `gypsum-color-contrast` - Calculate contrast ratio

## Supported Faces

Gypsum generates specifications for 170+ faces including:

- Core Emacs (font-lock, mode-line, minibuffer)
- Org mode and Markdown
- Magit and diff
- Company, Corfu, Vertico, Marginalia
- Flycheck and Flymake
- Tree-sitter
- Dired, Eshell
- And many more...

## Philosophy Reference

For the full rationale behind this approach, see Nikita Tonsky's blog post: [Syntax Highlighting Is a Waste of an Opportunity](https://tonsky.me/blog/syntax-highlighting/)

## License

GPL-3.0
