# gemini-emacs

Gemini CLI integration for Emacs, providing an interactive AI assistant via terminal.

## Features

- Interactive Gemini AI assistant in Emacs via vterm
- Context-aware file and project navigation
- Multiple Gemini instances support
- Send current file, region, or custom prompts to Gemini
- Inspired by claude-code.el

## Installation

### From GitHub

```elisp
;; Using straight.el
(straight-use-package
  '(gemini-emacs :type git :host github :repo "dawsh2/gemini-emacs"))

;; Or using use-package with straight
(use-package gemini-cli
  :straight (gemini-emacs :type git :host github :repo "dawsh2/gemini-emacs")
  :bind (("C-c g s" . gemini-send-prompt)
         ("C-c g r" . gemini-send-region)
         ("C-c g b" . gemini-send-buffer)
         ("C-c g x" . gemini-context)
         ("C-c g t" . gemini-toggle)
         ("C-c g k" . gemini-kill)))

;; Or manually
(add-to-list 'load-path "/path/to/gemini-emacs")
(require 'gemini-cli)
```

### Dependencies

- Emacs 27.1+
- vterm package
- Gemini CLI installed (`/opt/homebrew/bin/gemini` or customize `gemini-program`)

## Configuration

```elisp
;; Customize the Gemini CLI path if needed
(setq gemini-program "/path/to/gemini")

;; Load the package
(require 'gemini-cli)
```

## Usage

All commands are available under the `C-c g` prefix:

- `C-c g s`: Send a prompt to Gemini
- `C-c g r`: Send the selected region to Gemini
- `C-c g b`: Send the entire buffer to Gemini
- `C-c g x`: Send prompt with current file context
- `C-c g t`: Toggle Gemini terminal visibility
- `C-c g k`: Kill the Gemini session

## Files

- `gemini-cli.el` - Main package file with vterm integration
- `gemini-simple.el` - Alternative simple API-based implementation
- `gemini-rock-solid.el` - Robust implementation with error handling
- Various loader and test files

## License

MIT