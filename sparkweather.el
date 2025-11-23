;;; sparkweather.el --- Weather forecasts with sparklines -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Robin Stephenson. All rights reserved.

;; Author: Robin Stephenson <robin@aglet.net>
;; Keywords: convenience, weather
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/aglet/sparkweather

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this  software and  associated documentation  files (the  "Software"), to
;; deal in the  Software without restriction, including  without limitation the
;; rights to use, copy, modify,  merge, publish, distribute, sublicense, and/or
;; sell copies of the  Software, and to permit persons to  whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright  notice and this permission notice shall  be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS  PROVIDED "AS IS", WITHOUT WARRANTY OF  ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING  BUT NOT  LIMITED TO  THE WARRANTIES  OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND  NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS  OR COPYRIGHT  HOLDERS BE  LIABLE FOR  ANY CLAIM,  DAMAGES OR  OTHER
;; LIABILITY,  WHETHER IN  AN ACTION  OF CONTRACT,  TORT OR  OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;; IN THE SOFTWARE.
;; 

;;; Commentary:

;; This package provides a simple weather forecast display using
;; sparklines.
;;
;; Commands:
;; - `sparkweather-day': Show full day forecast with sparklines
;; - `sparkweather': Alias for `sparkweather-day'
;;
;; Configuration:
;; The package uses `calendar-latitude' and `calendar-longitude' for
;; the forecast location.
;;
;; The sparkline display highlights lunch and commute time windows,
;; which can be configured via `sparkweather-lunch-start-hour',
;; `sparkweather-lunch-end-hour', `sparkweather-commute-start-hour',
;; and `sparkweather-commute-end-hour'.
;;
;; Weather icons use Unicode weather glyphs from the Miscellaneous
;; Symbols block for broad compatibility.
;;
;; Weather data is provided by Open-Meteo https://open-meteo.com,
;; which is free for non-commercial use with up to 10000 requests per day.

;;; Code:

(require 'url)
(require 'json)
(require 'cl-lib)
(require 'iso8601)
(require 'solar)

(defgroup sparkweather nil
  "Weather forecasts with sparklines."
  :group 'calendar
  :prefix "sparkweather-")

(defun sparkweather--validate-hour (symbol value)
  "Validate that VALUE is a valid hour (0-23) for SYMBOL."
  (unless (and (integerp value) (>= value 0) (<= value 23))
    (user-error "Hour must be 0-23, got %s" value))
  (set-default symbol value))

(defcustom sparkweather-lunch-start-hour 12
  "Start hour for lunch time window (24-hour format, 0-23)."
  :type 'integer
  :set #'sparkweather--validate-hour
  :group 'sparkweather)

(defcustom sparkweather-lunch-end-hour 14
  "End hour for lunch time window (24-hour format, 0-23)."
  :type 'integer
  :set #'sparkweather--validate-hour
  :group 'sparkweather)

(defcustom sparkweather-commute-start-hour 17
  "Start hour for commute time window (24-hour format, 0-23)."
  :type 'integer
  :set #'sparkweather--validate-hour
  :group 'sparkweather)

(defcustom sparkweather-commute-end-hour 19
  "End hour for commute time window (24-hour format, 0-23)."
  :type 'integer
  :set #'sparkweather--validate-hour
  :group 'sparkweather)

(defconst sparkweather--buffer-name "*Sparkweather*"
  "Name of buffer used to display weather forecasts.")

(defconst sparkweather--wmo-codes-unicode
  '((0 . ("☀" "Clear sky"))
    (1 . ("☀" "Mainly clear"))
    (2 . ("⛅" "Partly cloudy"))
    (3 . ("☁" "Overcast"))
    (45 . ("☁" "Fog"))
    (48 . ("☁" "Rime fog"))
    (51 . ("⛆" "Light drizzle"))
    (53 . ("⛆" "Moderate drizzle"))
    (55 . ("⛆" "Dense drizzle"))
    (56 . ("⛆" "Freezing drizzle"))
    (57 . ("⛆" "Freezing drizzle"))
    (61 . ("⛆" "Slight rain"))
    (63 . ("⛆" "Rain"))
    (65 . ("⛆" "Heavy rain"))
    (66 . ("⛆" "Freezing rain"))
    (67 . ("⛆" "Heavy freezing rain"))
    (71 . ("⛇" "Slight snow"))
    (73 . ("⛇" "Snow"))
    (75 . ("⛇" "Heavy snow"))
    (77 . ("⛇" "Snow grains"))
    (80 . ("⛆" "Rain showers"))
    (81 . ("⛆" "Rain showers"))
    (82 . ("⛆" "Violent rain showers"))
    (85 . ("⛇" "Snow showers"))
    (86 . ("⛇" "Heavy snow showers"))
    (95 . ("⛈" "Thunderstorm"))
    (96 . ("⛈" "Thunderstorm with hail"))
    (99 . ("⛈" "Thunderstorm with heavy hail")))
  "WMO weather code to (glyph description) mapping from Miscellaneous Symbols.")

(defun sparkweather--wmo-code-info (code)
  "Get (icon description) for WMO CODE using Unicode weather glyphs."
  (let ((info (or (alist-get code sparkweather--wmo-codes-unicode)
                  '("?" "Unknown"))))
    (list (car info) (downcase (cadr info)))))

(defun sparkweather--process-day-response (status callback)
  "Process weather API response for full day and call CALLBACK with results.
STATUS is the `url-retrieve` status parameter."
  (condition-case err
      (progn
        (when (plist-get status :error)
          (error "Network error: %s" (plist-get status :error)))

        (goto-char (point-min))
        (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
          (error "Invalid HTTP response"))

        (let ((status-code (string-to-number (match-string 1))))
          (unless (= status-code 200)
            (error "HTTP error %d" status-code)))

        (unless (re-search-forward "\r?\n\r?\n" nil t)
          (error "Missing HTTP headers"))

        (let* ((json (condition-case parse-err
                         (json-parse-buffer :object-type 'alist)
                       (error (error "Failed to parse JSON: %s" parse-err))))
               (hourly (or (alist-get 'hourly json)
                          (error "Missing 'hourly' data in response")))
               (times (or (alist-get 'time hourly)
                         (error "Missing 'time' data in response")))
               (temps (or (alist-get 'temperature_2m hourly)
                         (error "Missing 'temperature_2m' data in response")))
               (precipitation-probability (or (alist-get 'precipitation_probability hourly)
                                              (error "Missing 'precipitation_probability' data in response")))
               (precipitation (or (alist-get 'precipitation hourly)
                                  (error "Missing 'precipitation' data in response")))
               (weather-codes (or (alist-get 'weather_code hourly)
                                 (error "Missing 'weather_code' data in response")))
               (results (cl-loop for i from 0 below (length times)
                               for time-string = (elt times i)
                               for decoded-time = (iso8601-parse time-string)
                               for hour = (decoded-time-hour decoded-time)
                               collect (list hour
                                            (elt temps i)
                                            (elt precipitation-probability i)
                                            (elt precipitation i)
                                            (elt weather-codes i)))))
          (kill-buffer)
          (funcall callback results)))
    (error
     (kill-buffer)
     (message "Weather fetch failed: %s" (error-message-string err)))))

(defun sparkweather--fetch-day (callback)
  "Fetch full day weather for calendar location and call CALLBACK with results."
  (unless (numberp calendar-latitude)
    (error "Calendar location not set.  Set `calendar-latitude' and `calendar-longitude'"))
  (unless (and (>= calendar-latitude -90) (<= calendar-latitude 90))
    (error "Invalid latitude %s.  Must be between -90 and 90" calendar-latitude))
  (unless (numberp calendar-longitude)
    (error "Calendar location not set.  Set `calendar-latitude' and `calendar-longitude'"))
  (unless (and (>= calendar-longitude -180) (<= calendar-longitude 180))
    (error "Invalid longitude %s.  Must be between -180 and 180" calendar-longitude))
  (let ((url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&hourly=temperature_2m,precipitation_probability,precipitation,weather_code&timezone=auto&forecast_days=1"
                    calendar-latitude calendar-longitude)))
    (url-retrieve url
                  #'sparkweather--process-day-response
                  (list callback)
                  t)))

(defun sparkweather--time-window-data (data start-hour end-hour)
  "Extract weather data from DATA for hours between START-HOUR and END-HOUR.
Returns (indices weather-codes) where indices is an alist for sparkline
highlighting and weather-codes is a list of WMO codes for averaging."
  (let ((indices nil)
        (weather-codes nil))
    (cl-loop for row in data
            for i from 0
            when (and (>= (car row) start-hour) (< (car row) end-hour))
            do (push (cons i nil) indices)
            and collect (nth 4 row) into codes
            finally (setq weather-codes codes))
    (list (nreverse indices) weather-codes)))

(defun sparkweather--sparkline (values &optional highlights current-hour)
  "Generate sparkline from VALUES.
HIGHLIGHTS is an alist of (index . face) to highlight specific positions.
CURRENT-HOUR, if provided, inserts a narrow no-break space before that hour."
  (let* ((min-val (apply #'min values))
         (max-val (apply #'max values))
         (range (- max-val min-val))
         (chars "▁▂▃▄▅▆▇█")
         (thin-space "\u202F"))
    (apply #'concat
           (cl-loop for val in values
                    for i from 0
                    for normalized = (if (zerop range)
                                         ;; Use floor for all-zero data (eg 0%
                                         ;; precipitation), mid-height otherwise
                                         ;; (eg constant temperature)
                                         (if (zerop max-val) 0 4)
                                       (min 7 (floor (* 8 (/ (- val min-val) range)))))
                    for char = (substring chars normalized (1+ normalized))
                    for face = (alist-get i highlights)
                    collect (concat
                             (when (and current-hour (= i current-hour)) thin-space)
                             (if face
                                 (propertize char 'face face)
                               char))))))

(defvar-keymap sparkweather-mode-map
  :doc "Keymap for `sparkweather-mode'."
  :parent tabulated-list-mode-map
  "q" #'quit-window
  "g" #'sparkweather-day)

(easy-menu-define sparkweather-mode-menu sparkweather-mode-map
  "Menu for Sparkweather mode."
  '("Sparkweather"
    ["Update" sparkweather-day]
    ["Quit" quit-window]))

(define-derived-mode sparkweather-mode tabulated-list-mode "Sparkweather"
  "Major mode for displaying weather forecasts with sparklines."
  (setq tabulated-list-format [("Range" 12 nil) ("Forecast" 0 nil)]
        tabulated-list-padding 1
        cursor-type nil)
  (setq-local show-help-function nil))

;; Configure display to use minimal window height
(add-to-list 'display-buffer-alist
             `(,(regexp-quote sparkweather--buffer-name)
               (display-buffer-reuse-window display-buffer-below-selected)
               (window-height . fit-window-to-buffer)
               (body-function . ,#'select-window)))

(defun sparkweather--display-day (data)
  "Display full day weather DATA with sparklines.
Highlights lunch and commute hours."
  (let* ((current-hour (decoded-time-hour (decode-time)))
         (temps (mapcar #'cadr data))
         (precipitation-probabilities (mapcar #'caddr data))
         (temp-min (apply #'min temps))
         (temp-max (apply #'max temps))
         (precipitation-max (apply #'max precipitation-probabilities))
         (lunch-data (sparkweather--time-window-data data
                                                     sparkweather-lunch-start-hour
                                                     sparkweather-lunch-end-hour))
         (lunch-indices (mapcar (lambda (pair) (cons (car pair) 'success)) (car lunch-data)))
         (lunch-weather (cadr lunch-data))
         (commute-data (sparkweather--time-window-data data
                                                       sparkweather-commute-start-hour
                                                       sparkweather-commute-end-hour))
         (commute-indices (mapcar (lambda (pair) (cons (car pair) 'warning)) (car commute-data)))
         (commute-weather (cadr commute-data))
         (highlights (append lunch-indices commute-indices))
         ;; Use worst weather (highest WMO code) for each time window
         (lunch-code (when lunch-weather (apply #'max lunch-weather)))
         (commute-code (when commute-weather (apply #'max commute-weather)))
         (lunch-info (when lunch-code (sparkweather--wmo-code-info lunch-code)))
         (commute-info (when commute-code (sparkweather--wmo-code-info commute-code)))
         (temp-sparkline (sparkweather--sparkline temps highlights current-hour))
         (precipitation-sparkline (sparkweather--sparkline precipitation-probabilities highlights current-hour))
         (rainy-weather-codes (cl-loop for row in data
                                       when (> (nth 2 row) 0)
                                       collect (nth 4 row)))
         (worst-weather-code (when rainy-weather-codes (apply #'max rainy-weather-codes)))
         (worst-weather-info (when worst-weather-code (sparkweather--wmo-code-info worst-weather-code)))
         (entries nil))
    (push (list 'temp (vector (format "%d—%d°C" (round temp-min) (round temp-max))
                              temp-sparkline))
          entries)
    (when worst-weather-info
      (push (list 'precip (vector (format "%d%% %s"
                                          (round precipitation-max)
                                          (car worst-weather-info))
                                  precipitation-sparkline))
            entries))
    (when lunch-info
      (push (list 'lunch (vector (concat (propertize "■" 'face 'success) " Lunch")
                                 (format "%s %s" (car lunch-info) (cadr lunch-info))))
            entries))
    (when commute-info
      (push (list 'commute (vector (concat (propertize "■" 'face 'warning) " Commute")
                                   (format "%s %s" (car commute-info) (cadr commute-info))))
            entries))
    (with-current-buffer (get-buffer-create sparkweather--buffer-name)
      (sparkweather-mode)
      (setq tabulated-list-entries (nreverse entries)
            tabulated-list-use-header-line nil)
      (tabulated-list-print t)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n" (propertize (format-time-string "%A %F %R") 'face 'bold)))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun sparkweather-day ()
  "Show full day weather with sparklines."
  (interactive)
  (sparkweather--fetch-day
   (lambda (data)
     (sparkweather--display-day data))))

;;;###autoload
(defalias 'sparkweather #'sparkweather-day)

(provide 'sparkweather)

;;; sparkweather.el ends here
