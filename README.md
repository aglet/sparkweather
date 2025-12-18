# `sparkweather`

![GitHub License](https://img.shields.io/github/license/aglet/sparkweather) [![MELPA](https://melpa.org/packages/sparkweather-badge.svg)](https://melpa.org/#/sparkweather)

Weather forecasts with sparklines for Emacs, using data from [Open-Meteo]( https://open-meteo.com).

![screenshot](screenshot.png)

## Installation

Sparkweather is available in [MELPA](https://melpa.org/#/getting-started); `M-x package-install RET sparkweather` or in your configuration file:

```elisp
(use-package sparkweather
  :ensure t
  :after calendar
  :config
  (setq calendar-latitude -41.3         ; Wellington, New Zealand
        calendar-longitude 174.8))
```

## Usage

Run `M-x sparkweather` to display today's forecast with sparklines.

A small space marks the current time, and coloured blocks highlight your configured windows. Press `g` to refresh, `q` to close.

## Configuration

Sparkweather uses the standard Emacs calendar location variables `calendar-latitude` and `calendar-longitude`.

Customize all other options via `M-x customize-group` / `sparkweather`.

### Location

```elisp
(setq calendar-latitude 47.6            ; Seattle, Washington
      calendar-longitude -122.3)
```

### Time windows

Highlight specific time periods in the forecast by customizing `sparkweather-time-windows`. Each window colours a portion of the sparkline and displays the worst weather for that period:

```elisp
(setq sparkweather-time-windows
      '(("Lunch" 12 14)
        ("Commute" 17 19)))
```

Windows default to the `success` face. Specify a different face as the fourth argument:

```elisp
(setq sparkweather-time-windows
      '(("Morning run" 6 7)
        ("School drop-off" 8 9 warning)
        ("Afternoon pickup" 15 16 warning)
        ("Evening walk" 18 20 error)))
```

### Custom faces

Define custom faces for time windows with specific colours:

```elisp
(defface my-cycling-face
  '((t :foreground "#ff69b4"))
  "Face for cycling commute window.")

(setq sparkweather-time-windows
      '(("Cycling" 7 9 my-cycling-face)
        ("Lunch" 12 14)))
```

Theme-aware faces adapt to light and dark backgrounds:

```elisp
(defface my-cycling-face
  '((((background light)) :foreground "#d73a49")
    (((background dark)) :foreground "#ff6b6b"))
  "Face for cycling commute, adapts to theme.")
```

### Footer display

Hide the timestamp and location footer to save two lines:

```elisp
(setq sparkweather-show-footer nil)
```

## License

[MIT](LICENSE)
