;;; sparkweather.el --- Weather forecasts with sparklines -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Robin Stephenson. All rights reserved.

;; Author: Robin Stephenson <robin@aglet.net>
;; Keywords: convenience, weather
;; Version: 0.3.1
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
;; The sparkline display highlights configurable time windows,
;; such as lunch and commute hours.  Customize `sparkweather-time-windows'
;; to add or modify highlighted periods.
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

(cl-defstruct (sparkweather-hour
               (:constructor sparkweather-hour-create
                (hour temperature precip-probability precipitation weather-code)))
  "Hourly weather data from API."
  hour
  temperature
  precip-probability
  precipitation
  weather-code)

(defgroup sparkweather nil
  "Weather forecasts with sparklines."
  :group 'calendar
  :prefix "sparkweather-")

(defcustom sparkweather-time-windows
  '(("Lunch" 12 14)
    ("Commute" 17 19))
  "Time windows to highlight in forecast display.
Each window colours a portion of the sparkline and displays the worst
weather for that period.

Define windows as (NAME START-HOUR END-HOUR) or
\(NAME START-HOUR END-HOUR FACE) where:
  NAME is a string describing the window (e.g., \"Lunch\", \"Commute\")
  START-HOUR is the first hour to highlight (0-23)
  END-HOUR is the hour to stop before (0-23, not included)
  FACE colours the window; defaults to \\='success if omitted

Examples:
  (\"Lunch\" 12 14) highlights hours 12 and 13 with \\='success face
  (\"Commute\" 17 19 warning) highlights 17 and 18 with \\='warning face"
  :type '(repeat (choice
                  (list (string :tag "Name")
                        (integer :tag "Start hour (0-23)")
                        (integer :tag "End hour (0-23)"))
                  (list (string :tag "Name")
                        (integer :tag "Start hour (0-23)")
                        (integer :tag "End hour (0-23)")
                        (symbol :tag "Highlight face"))))
  :group 'sparkweather)

(defcustom sparkweather-add-footer t
  "Whether to add timestamp and location footer to buffer."
  :type 'boolean
  :group 'sparkweather)

(defcustom sparkweather-hide-footer nil
  "Whether to size window to hide footer.
Has no effect if `sparkweather-add-footer' is nil."
  :type 'boolean
  :group 'sparkweather)

(defcustom sparkweather-temperature-unit 'celsius
  "Temperature unit for display and API requests.
Valid values are \\='celsius, \\='fahrenheit, \"C\", or \"F\"."
  :type '(choice (const :tag "Celsius" celsius)
                 (const :tag "Fahrenheit" fahrenheit)
                 (const :tag "C" "C")
                 (const :tag "F" "F"))
  :group 'sparkweather)

(defcustom sparkweather-lunch-start-hour 12
  "Start hour for lunch time window (24-hour format, 0-23).

DEPRECATED: Use `sparkweather-time-windows' instead."
  :type 'integer
  :group 'sparkweather)
(make-obsolete-variable 'sparkweather-lunch-start-hour
                        'sparkweather-time-windows
                        "0.2.0")

(defcustom sparkweather-lunch-end-hour 14
  "End hour for lunch time window (24-hour format, 0-23).

DEPRECATED: Use `sparkweather-time-windows' instead."
  :type 'integer
  :group 'sparkweather)
(make-obsolete-variable 'sparkweather-lunch-end-hour
                        'sparkweather-time-windows
                        "0.2.0")

(defcustom sparkweather-commute-start-hour 17
  "Start hour for commute time window (24-hour format, 0-23).

DEPRECATED: Use `sparkweather-time-windows' instead."
  :type 'integer
  :group 'sparkweather)
(make-obsolete-variable 'sparkweather-commute-start-hour
                        'sparkweather-time-windows
                        "0.2.0")

(defcustom sparkweather-commute-end-hour 19
  "End hour for commute time window (24-hour format, 0-23).

DEPRECATED: Use `sparkweather-time-windows' instead."
  :type 'integer
  :group 'sparkweather)
(make-obsolete-variable 'sparkweather-commute-end-hour
                        'sparkweather-time-windows
                        "0.2.0")

(defun sparkweather--temperature-unit-symbol ()
  "Return the display symbol (°C or °F) for the configured temperature unit."
  (pcase sparkweather-temperature-unit
    ((or 'fahrenheit "F") "°F")
    (_ "°C")))

(defun sparkweather--convert-temperature (celsius)
  "Convert CELSIUS temperature to the configured unit.
Returns temperature in Fahrenheit if unit is set to fahrenheit or \"F\",
otherwise returns CELSIUS unchanged."
  (pcase sparkweather-temperature-unit
    ((or 'fahrenheit "F") (+ (* celsius 1.8) 32))
    (_ celsius)))

(defun sparkweather--detect-invalid-windows (windows)
  "Detect windows with invalid hour ranges in WINDOWS list.
Returns list of invalid windows as (NAME START END REASON), or nil if all valid.
REASON is one of: out-of-range, invalid-range."
  (cl-loop for window in windows
           for (name start end _face) = window
           for reason = (cond
                         ((or (< start 0) (> start 23) (< end 0) (> end 23))
                          'out-of-range)
                         ((<= end start)
                          'invalid-range))
           when reason
           collect (list name start end reason)))

(defun sparkweather--format-invalid-warning (invalid-windows)
  "Format warning message for INVALID-WINDOWS."
  (concat "Invalid time windows detected in sparkweather-time-windows:\n\n"
          (mapconcat
           (pcase-lambda (`(,name ,start ,end ,reason))
             (pcase reason
               ('out-of-range
                (format "  - \"%s\" has hours outside valid range 0-23 (start: %d, end: %d)"
                       name start end))
               ('invalid-range
                (format "  - \"%s\" has end hour %d <= start hour %d"
                       name end start))))
           invalid-windows
           "\n")
          "\n\nHours must be in range 0-23, and end hour must be after start hour."))

(defun sparkweather--window-overlap (w1 w2)
  "Return overlap between windows W1 and W2, or nil if they don't overlap."
  (pcase-let ((`(,name1 ,start1 ,end1 ,_) w1)
              (`(,name2 ,start2 ,end2 ,_) w2))
    (let ((overlap-start (max start1 start2))
          (overlap-end (min end1 end2)))
      (when (< overlap-start overlap-end)
        (list name1 name2 overlap-start overlap-end)))))

(defun sparkweather--detect-window-overlaps (windows)
  "Detect overlapping time windows in WINDOWS list.
Returns list of overlaps as (NAME1 NAME2 OVERLAP-START OVERLAP-END),
or nil if no overlaps found."
  (cl-loop for (w1 . rest) on windows
           nconc (cl-loop for w2 in rest
                          for overlap = (sparkweather--window-overlap w1 w2)
                          when overlap collect overlap)))

(defun sparkweather--format-overlap-warning (overlaps)
  "Format warning message for OVERLAPS."
  (concat "Time window overlaps detected in sparkweather-time-windows:\n\n"
          (mapconcat
           (pcase-lambda (`(,name1 ,name2 ,start ,end))
             (format "  - \"%s\" and \"%s\" both cover hours %d-%d"
                    name1 name2 start end))
           overlaps
           "\n")
          "\n\nFirst window takes precedence for overlapping hours.\n"
          "Consider adjusting your window times to avoid overlaps."))

(defun sparkweather--validate-windows (windows)
  "Validate WINDOWS configuration and warn about problems.
Returns WINDOWS unchanged (validation doesn't prevent usage)."
  (let ((invalid (sparkweather--detect-invalid-windows windows)))
    (when invalid
      (display-warning 'sparkweather
                      (sparkweather--format-invalid-warning invalid)
                      :warning)))
  (let ((overlaps (sparkweather--detect-window-overlaps windows)))
    (when overlaps
      (display-warning 'sparkweather
                      (sparkweather--format-overlap-warning overlaps)
                      :warning)))
  windows)

(defun sparkweather--migrate-deprecated-config ()
  "Migrate old hour variables to new time-windows format.
Returns new time-windows list if old variables are customized,
nil if they have default values (no migration needed)."
  (let ((lunch-start sparkweather-lunch-start-hour)
        (lunch-end sparkweather-lunch-end-hour)
        (commute-start sparkweather-commute-start-hour)
        (commute-end sparkweather-commute-end-hour))
    (if (and (= lunch-start 12) (= lunch-end 14)
             (= commute-start 17) (= commute-end 19))
        nil
      (list (list "Lunch" lunch-start lunch-end 'success)
            (list "Commute" commute-start commute-end 'warning)))))

(defun sparkweather--format-migration-message (windows)
  "Format helpful migration message for WINDOWS config."
  (format "Sparkweather: Using deprecated hour variables.

Update your configuration to:

  (setq sparkweather-time-windows
        '%S)

The old sparkweather-{lunch,commute}-{start,end}-hour variables
are deprecated and will be removed in a future version."
          windows))

(defun sparkweather--maybe-migrate-config ()
  "Check for deprecated config and migrate with warning if found."
  (let ((migrated-windows (sparkweather--migrate-deprecated-config)))
    (when migrated-windows
      (display-warning 'sparkweather
                      (sparkweather--format-migration-message migrated-windows)
                      :warning)
      migrated-windows)))

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
  (pcase-let ((`(,icon ,description)
               (or (alist-get code sparkweather--wmo-codes-unicode)
                   '("?" "Unknown"))))
    (list icon (downcase description))))

(defun sparkweather--require-field (alist field)
  "Get FIELD from ALIST or signal error if missing."
  (or (alist-get field alist)
      (error "Missing '%s' data in response" field)))

(defun sparkweather--validate-http-response (status)
  "Validate HTTP response STATUS and position buffer after headers.
Signals error if response is invalid."
  (when (plist-get status :error)
    (error "Network error: %s" (plist-get status :error)))

  (goto-char (point-min))
  (unless (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
    (error "Invalid HTTP response"))

  (let ((status-code (string-to-number (match-string 1))))
    (unless (= status-code 200)
      (error "HTTP error %d" status-code)))

  (unless (re-search-forward "\r?\n\r?\n" nil t)
    (error "Missing HTTP headers")))

(defun sparkweather--parse-weather-json ()
  "Parse JSON weather response from current buffer.
Returns alist with hourly data arrays."
  (let ((json (json-parse-buffer :object-type 'alist)))
    (or (alist-get 'hourly json)
        (error "Missing 'hourly' data in response"))))

(defun sparkweather--transform-hourly-data (hourly)
  "Transform HOURLY alist into list of sparkweather-hour structs."
  (let ((times (sparkweather--require-field hourly 'time))
        (temps (sparkweather--require-field hourly 'temperature_2m))
        (precip-probs (sparkweather--require-field hourly 'precipitation_probability))
        (precips (sparkweather--require-field hourly 'precipitation))
        (codes (sparkweather--require-field hourly 'weather_code)))
    (cl-loop for i from 0 below (length times)
             for time-string = (aref times i)
             for decoded-time = (iso8601-parse time-string)
             for hour = (decoded-time-hour decoded-time)
             collect (sparkweather-hour-create
                     hour
                     (aref temps i)
                     (aref precip-probs i)
                     (aref precips i)
                     (aref codes i)))))

(defun sparkweather--process-day-response (status callback)
  "Process weather API response for full day and call CALLBACK with results.
STATUS is the `url-retrieve` status parameter."
  (condition-case err
      (progn
        (sparkweather--validate-http-response status)
        (let* ((hourly (sparkweather--parse-weather-json))
               (results (sparkweather--transform-hourly-data hourly)))
          (kill-buffer)
          (funcall callback results)))
    (error
     (kill-buffer)
     (message "Weather fetch failed: %s" (error-message-string err)))))

(defun sparkweather--validate-coordinates ()
  "Validate calendar coordinates are set and within valid ranges."
  (unless (and (boundp 'calendar-latitude) (boundp 'calendar-longitude)
               (numberp calendar-latitude) (numberp calendar-longitude))
    (error "Calendar location not set.  Set `calendar-latitude' and `calendar-longitude'"))
  (unless (and (>= calendar-latitude -90) (<= calendar-latitude 90))
    (error "Invalid latitude %s. Must be between -90 and 90" calendar-latitude))
  (unless (and (>= calendar-longitude -180) (<= calendar-longitude 180))
    (error "Invalid longitude %s. Must be between -180 and 180" calendar-longitude)))

(defun sparkweather--fetch-day (callback)
  "Fetch full day weather for calendar location and call CALLBACK with results."
  (sparkweather--validate-coordinates)
  (let ((url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&hourly=temperature_2m,precipitation_probability,precipitation,weather_code&timezone=auto&forecast_days=1"
                    calendar-latitude calendar-longitude)))
    (url-retrieve url
                  #'sparkweather--process-day-response
                  (list callback)
                  t)))

(defun sparkweather--time-window-data (data start-hour end-hour)
  "Extract weather data from DATA for hours between START-HOUR and END-HOUR.
Returns (indices weather-codes) where indices is a list of matching hour indices
and weather-codes is a list of WMO codes for that window."
  (cl-loop for hour-data in data
           for i from 0
           when (and (>= (sparkweather-hour-hour hour-data) start-hour)
                     (< (sparkweather-hour-hour hour-data) end-hour))
           collect i into indices
           and collect (sparkweather-hour-weather-code hour-data) into codes
           finally return (list indices codes)))

(defun sparkweather--prepare-window (data window)
  "Prepare sparkline data for WINDOW from weather DATA.
WINDOW is (name start-hour end-hour) or (name start-hour end-hour face).
If FACE is omitted, defaults to \\='success.
Returns plist with :name :face :indices :weather-info."
  (pcase-let* ((`(,name ,start ,end . ,rest) window)
               (face (or (car rest) 'success))
               (`(,indices ,codes)
                (sparkweather--time-window-data data start end)))
    (let* ((highlighted-indices (mapcar (lambda (idx) (cons idx face))
                                        indices))
           (worst-code (and codes (apply #'max codes)))
           (weather-info (when worst-code
                          (sparkweather--wmo-code-info worst-code))))
      (list :name name
            :face face
            :indices highlighted-indices
            :weather-info weather-info))))

(defun sparkweather--prepare-windows (data windows)
  "Prepare all WINDOWS from weather DATA.
Returns (window-data all-indices) where window-data is a list of window plists
and all-indices is combined highlight indices from all windows."
  (cl-loop for window in windows
           for window-plist = (sparkweather--prepare-window data window)
           collect window-plist into window-data
           nconc (plist-get window-plist :indices) into all-indices
           finally return (list window-data all-indices)))

(defun sparkweather--normalize-value (val min-val max-val range)
  "Normalise VAL to 0-7 for sparkline character selection.
MIN-VAL and MAX-VAL define the data range, RANGE is their difference.
Handles edge cases: all-zero data uses floor, constant data uses
mid-height."
  (cond
   ((and (zerop range) (zerop max-val)) 0)
   ((zerop range) 4)
   (t (min 7 (floor (* 8 (/ (- val min-val) (float range))))))))

(defun sparkweather--format-sparkline-char (char face current-hour-p)
  "Format sparkline CHAR with optional FACE and CURRENT-HOUR-P marker."
  (concat
   (when current-hour-p "\u202F")
   (if face
       (propertize char 'face face)
     char)))

(defun sparkweather--sparkline (values &optional highlights current-hour)
  "Generate sparkline from VALUES.
HIGHLIGHTS is an alist of (index . face) to highlight specific positions.
CURRENT-HOUR, if provided, inserts a narrow no-break space before that hour."
  (when values
    (let* ((min-val (apply #'min values))
           (max-val (apply #'max values))
           (range (- max-val min-val))
           (chars "▁▂▃▄▅▆▇█"))
      (cl-loop for val in values
               for i from 0
               concat (let* ((normalized (sparkweather--normalize-value val min-val max-val range))
                             (char (substring chars normalized (1+ normalized)))
                             (face (alist-get i highlights)))
                        (sparkweather--format-sparkline-char
                         char face (and current-hour (= i current-hour))))))))

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
  (setq tabulated-list-padding 1
        cursor-type nil)
  (setq-local show-help-function nil))

(defun sparkweather--format-footer ()
  "Generate footer text with timestamp and optional location.
Returns string suitable for insertion at buffer end.
Leading newline provides spacing, allowing hiding of only the
timestamp when footer is hidden."
  ;; Leading \n creates blank line for visual spacing.
  ;; sparkweather--window-max-height relies on this to hide only the timestamp line.
  (concat "\n" (format-time-string "%A %F %R")
          (when (and (boundp 'calendar-location-name) calendar-location-name)
            (concat " " calendar-location-name))))

(defun sparkweather--footer-line-count ()
  "Count number of lines in footer."
  (with-temp-buffer
    (insert (sparkweather--format-footer))
    (count-lines (point-min) (point-max))))

(defun sparkweather--window-max-height (window)
  "Calculate maximum height for sparkweather WINDOW.
Returns reduced height when `sparkweather-hide-footer' is enabled,
nil otherwise.  Keeps blank line visible as buffer, hiding only
timestamp/location line."
  (when (and sparkweather-add-footer sparkweather-hide-footer)
    (with-current-buffer (window-buffer window)
      ;; Calculate desired body lines (total - 1 to hide timestamp)
      ;; Relies on sparkweather--format-footer's leading newline to provide neat spacing
      ;; Then add overhead (mode line, etc.) to get total window height
      (let* ((desired-body-lines (1- (count-lines (point-min) (point-max))))
             (overhead (- (window-height window) (window-body-height window))))
        (+ desired-body-lines overhead)))))

(add-to-list 'display-buffer-alist
             `(,(regexp-quote sparkweather--buffer-name)
               (display-buffer-reuse-window display-buffer-below-selected)
               (window-height . ,(lambda (window)
                                   (fit-window-to-buffer window (sparkweather--window-max-height window))))
               (body-function . ,#'select-window)))

(defun sparkweather--display-window-entry (window-plist)
  "Create table entry from WINDOW-PLIST.
Returns (symbol vector) for `tabulated-list-entries', or nil if no weather info."
  (let ((name (plist-get window-plist :name))
        (face (plist-get window-plist :face))
        (info (plist-get window-plist :weather-info)))
    (when info
      (list (intern (downcase name))
            (vector (concat (propertize "■" 'face face) " " name)
                    (format "%s %s" (car info) (cadr info)))))))

(defun sparkweather--calculate-ranges (data)
  "Extract values and calculate ranges from DATA in single pass.
Returns (temps precip-probs temp-min temp-max precip-max rainy-codes)."
  (cl-loop for hour-data in data
           for temp = (sparkweather-hour-temperature hour-data)
           for precip-prob = (sparkweather-hour-precip-probability hour-data)
           for weather-code = (sparkweather-hour-weather-code hour-data)
           collect temp into temps
           collect precip-prob into precip-probs
           minimize temp into temp-min
           maximize temp into temp-max
           maximize precip-prob into precip-max
           when (> precip-prob 0)
           collect weather-code into rainy-codes
           finally return (list temps precip-probs temp-min temp-max precip-max rainy-codes)))

(defun sparkweather--create-entries (data current-hour windows)
  "Create table entries for DATA, highlighting CURRENT-HOUR and WINDOWS.
Returns list of entries for `tabulated-list-mode'."
  (pcase-let* ((`(,temps ,precip-probs ,temp-min ,temp-max ,precip-max ,rainy-codes)
                (sparkweather--calculate-ranges data))
               (converted-temps (mapcar #'sparkweather--convert-temperature temps))
               (`(,window-data ,highlights)
                (sparkweather--prepare-windows data windows))
               (temp-sparkline (sparkweather--sparkline converted-temps highlights current-hour))
               (precip-sparkline (sparkweather--sparkline precip-probs highlights current-hour))
               (worst-weather-code (and rainy-codes (apply #'max rainy-codes)))
               (worst-weather-info (when worst-weather-code
                                    (sparkweather--wmo-code-info worst-weather-code))))
    (append
     (list (list 'temp (vector (format "%d—%d%s"
                                      (round (sparkweather--convert-temperature temp-min))
                                      (round (sparkweather--convert-temperature temp-max))
                                      (sparkweather--temperature-unit-symbol))
                               temp-sparkline)))
     (when worst-weather-info
       (list (list 'precip (vector (format "%d%% %s"
                                           (round precip-max)
                                           (car worst-weather-info))
                                   precip-sparkline))))
     (cl-loop for window-plist in window-data
              for entry = (sparkweather--display-window-entry window-plist)
              when entry collect entry))))

(defun sparkweather--calculate-column-width (entries)
  "Calculate optimal first column width for ENTRIES."
  (or (cl-loop for (_id columns) in entries
               maximize (length (aref columns 0)))
      0))

(defun sparkweather--show-buffer (entries)
  "Populate weather buffer with ENTRIES and display it."
  (with-current-buffer (get-buffer-create sparkweather--buffer-name)
    (sparkweather-mode)
    (let ((first-col-width (sparkweather--calculate-column-width entries)))
      (setq tabulated-list-format `[("Range" ,first-col-width nil) ("Forecast" 0 nil)]))
    (setq tabulated-list-entries entries
          tabulated-list-use-header-line nil)
    (tabulated-list-print t)
    (when sparkweather-add-footer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (sparkweather--format-footer))))
    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun sparkweather--display-day (data)
  "Display full day weather DATA with sparklines.
Highlights configured time windows."
  (let* ((current-hour (decoded-time-hour (decode-time)))
         (windows (sparkweather--validate-windows
                  (or (sparkweather--maybe-migrate-config)
                      sparkweather-time-windows)))
         (entries (sparkweather--create-entries data current-hour windows)))
    (sparkweather--show-buffer entries)))

;;;###autoload
(defun sparkweather-day ()
  "Show full day weather with sparklines."
  (interactive)
  (sparkweather--fetch-day #'sparkweather--display-day))

;;;###autoload
(defalias 'sparkweather #'sparkweather-day)

(provide 'sparkweather)

;;; sparkweather.el ends here
