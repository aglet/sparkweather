# Contributing to `sparkweather`

Thank you for your interest; fork and raise a pull request!

## Setup

[Mise-en-place](https://mise.jdx.dev) orchestrates tools:

- Install the tools: `mise install`
- List tasks: `mise task`

[Prek](https://prek.j178.dev) manages pre-commit hooks; to install them run `prek install`. [`.editorconfig`](.editorconfig) provides hints to editors.

## Validating Emacs Lisp changes

MELPA requires clean byte-compilation and checkdoc validation. Run these checks before submitting:

```elisp
(byte-compile-file "sparkweather.el")
(checkdoc-file "sparkweather.el")
```

Both must complete without warnings.
