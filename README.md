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

### Using straight.el

```elisp
(straight-use-package
 '(gypsum :type git :host github :repo "davidhmartin/gypsum"))
```

### Using straight.el with use-package

```elisp
(use-package gypsum
  :straight (:type git :host github :repo "davidhmartin/gypsum"))
```

### Manual Installation

Clone the repository:

```bash
git clone https://github.com/davidhmartin/gypsum.git ~/.emacs.d/site-lisp/gypsum
```

Add to your init.el:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/gypsum")
(require 'gypsum)
```

## Usage

### Interactive Theme Generation

The easiest way to create a theme:

```
M-x gypsum-generate-theme
```

This guides you through:
1. Choosing a theme name
2. Selecting light or dark variant
3. Picking a seed color (with visual color picker)
4. Optional: customizing individual colors
5. Preview before saving

### Programmatic Generation

#### Minimal - Seed Only

```elisp
(gypsum-generate "my-theme" :seed "#5E81AC" :variant 'dark)
```

The seed color becomes the **definition** color. String, constant, and comment colors are derived via color wheel relationships.

#### With Background

```elisp
(gypsum-generate "my-theme"
  :background "#0E1415"
  :seed "#71ADE7")
```

When background is provided, variant (light/dark) is inferred from its luminance.

#### With Overrides

```elisp
(gypsum-generate "my-theme"
  :seed "#5E81AC"
  :variant 'dark
  :comment "#DFDF8E"      ; Override just comment color
  :contrast 'low)          ; Low contrast variant
```

#### Fully Explicit

```elisp
(gypsum-generate "my-theme"
  :background "#0E1415"
  :string "#99C592"
  :constant "#9999FF"
  :comment "#DFDF8E"
  :definition "#71ADE7")
```

#### Generate All Variants

Generate all 4 combinations of light/dark and normal/low contrast at once:

```elisp
(gypsum-generate-all "my-theme" :seed "#5E81AC")
```

This creates:
- `my-theme-dark-theme.el` (dark, normal contrast)
- `my-theme-dark-lc-theme.el` (dark, low contrast)
- `my-theme-light-theme.el` (light, normal contrast)
- `my-theme-light-lc-theme.el` (light, low contrast)

Or interactively:

```
M-x gypsum-generate-all
```

### Preview Without Saving

```elisp
(gypsum-preview :seed "#5E81AC" :variant 'dark)
```

Use `M-x gypsum-preview-dismiss` to remove the preview.

### View Palette

See what colors would be generated from a seed:

```
M-x gypsum-show-palette
```

## Options

| Option | Values | Description |
|--------|--------|-------------|
| `:seed` | hex color | Base color (becomes definition color) |
| `:variant` | `'light` or `'dark` | Theme variant |
| `:contrast` | `'normal` or `'low` | Contrast level |
| `:background` | hex color | Override background |
| `:string` | hex color | Override string color |
| `:constant` | hex color | Override constant color |
| `:comment` | hex color | Override comment color |
| `:definition` | hex color | Override definition color |
| `:output` | path | Output file path |
| `:load` | `t` or `nil` | Load and enable theme after generation (default `t`) |

## Color Derivation

From the seed (definition) color, other colors are derived:

- **String**: Hue rotated -120° (toward green)
- **Constant**: Hue rotated +60° (toward purple)
- **Comment**: Hue rotated +150° (light) or +165° (dark)

Saturation and lightness are adjusted based on variant:
- **Dark themes**: Muted pastels (lower saturation, higher lightness)
- **Light themes**: Richer colors (higher saturation, lower lightness)

Status colors (error, warning, success) are harmonized with the seed rather than using conventional red/yellow/green.

## Supported Faces

Gypsum generates specifications for 170+ faces including:

- Core Emacs faces (font-lock, mode-line, minibuffer)
- Org mode and Markdown
- Magit and diff
- Company, Corfu, Vertico, Marginalia
- Flycheck and Flymake
- Tree-sitter
- Dired, Eshell
- And many more...

## Example Themes

Generate sample themes:

```elisp
;; Dark theme with Nord-inspired blue
(gypsum-generate "nord-minimal"
  :seed "#5E81AC"
  :variant 'dark)

;; Light theme with forest green
(gypsum-generate "forest-light"
  :seed "#2E8B57"
  :variant 'light)

;; Low contrast dark theme
(gypsum-generate "easy-eyes"
  :seed "#7C3AED"
  :variant 'dark
  :contrast 'low)
```

## API Reference

### Theme Generation

- `gypsum` - Interactive theme generation (guided wizard)
- `gypsum-generate` - Generate and save a theme file (programmatic)
- `gypsum-generate-all` - Generate all 4 variants (programmatic)

### Preview

- `gypsum-preview-dismiss` - Dismiss current preview
- `gypsum-show-palette` - Display palette for a seed

### Color Tools

- `gypsum-palette-create` - Create palette programmatically

### Color Manipulation

- `gypsum-color-rotate` - Rotate hue
- `gypsum-color-lighten` / `gypsum-color-darken`
- `gypsum-color-saturate` / `gypsum-color-desaturate`
- `gypsum-color-blend` - Blend two colors
- `gypsum-color-contrast` - Calculate contrast ratio
- `gypsum-color-luminance` - Calculate luminance

## Customization

### Output Directory

By default, Gypsum saves generated themes to `~/.emacs.d/themes/`. To change this:

```elisp
(setq gypsum-output-directory "~/my-themes/")
```

You can also specify an output path per-theme:

```elisp
(gypsum-generate "my-theme" :seed "#5E81AC" :variant 'dark
                 :output "~/my-themes/my-theme-theme.el")
```

### Loading Generated Themes

For Emacs to find your generated themes, the output directory must be in `custom-theme-load-path`. Gypsum automatically adds it when generating a theme in the current session, but for future sessions you need to configure this.

**Standalone configuration:**

```elisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
```

**With straight.el:**

```elisp
(straight-use-package
 '(gypsum :type git :host github :repo "davidhmartin/gypsum"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
```

**With use-package and straight.el:**

```elisp
(use-package gypsum
  :straight (:type git :host github :repo "davidhmartin/gypsum")
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/"))
```

After this configuration, you can load your generated themes with:

```elisp
(load-theme 'my-theme t)
```

### Author Name

Set the author name included in generated theme headers:

```elisp
(setq gypsum-author-name "Your Name")
```

## Philosophy Reference

For the full rationale behind this approach, see Nikita Tonsky's blog post: [Syntax Highlighting Is a Waste of an Opportunity](https://tonsky.me/blog/syntax-highlighting/)

## License

GPL-3.0
