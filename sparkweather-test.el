;;; sparkweather-test.el --- Tests for sparkweather -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Robin Stephenson. All rights reserved.

;;; Commentary:
;; Tests for sparkweather.el

;;; Code:

(require 'ert)
(require 'sparkweather)

(defvar sparkweather-test--sample-data
  (list
   (sparkweather-hour-create 0 10.0 0 0.0 0)   ; hour 0: 10°C, 0% precipitation
   (sparkweather-hour-create 1 9.5 0 0.0 0)
   (sparkweather-hour-create 6 8.0 10 0.1 51)  ; hour 6: drizzle
   (sparkweather-hour-create 12 15.0 20 0.2 53) ; hour 12: moderate drizzle (lunch start)
   (sparkweather-hour-create 13 16.0 30 0.3 61) ; hour 13: slight rain
   (sparkweather-hour-create 14 15.5 25 0.2 53) ; hour 14: moderate drizzle (lunch end boundary)
   (sparkweather-hour-create 17 14.0 40 0.5 63) ; hour 17: rain (commute start)
   (sparkweather-hour-create 18 13.5 45 0.6 65) ; hour 18: heavy rain
   (sparkweather-hour-create 19 13.0 35 0.4 61) ; hour 19: slight rain (commute end boundary)
   (sparkweather-hour-create 23 11.0 5 0.05 1)) ; hour 23: mainly clear
  "Sample weather data for testing using sparkweather-hour structs.")

(ert-deftest sparkweather-test-wmo-code-info-clear-sky ()
  (should (equal (sparkweather--wmo-code-info 0) '("☀" "clear sky"))))

(ert-deftest sparkweather-test-wmo-code-info-rain ()
  (should (equal (sparkweather--wmo-code-info 63) '("⛆" "rain"))))

(ert-deftest sparkweather-test-wmo-code-info-thunderstorm ()
  (should (equal (sparkweather--wmo-code-info 95) '("⛈" "thunderstorm"))))

(ert-deftest sparkweather-test-wmo-code-info-unknown ()
  (should (equal (sparkweather--wmo-code-info 999) '("?" "unknown"))))

(ert-deftest sparkweather-test-normalize-value-all-zero ()
  "All-zero data should return 0."
  (should (= (sparkweather--normalize-value 0 0 0 0) 0)))

(ert-deftest sparkweather-test-normalize-value-constant ()
  "Constant data (zero range) should return mid-height 4."
  (should (= (sparkweather--normalize-value 10 10 10 0) 4)))

(ert-deftest sparkweather-test-normalize-value-min ()
  "Minimum value should normalize to 0."
  (should (= (sparkweather--normalize-value 0 0 100 100) 0)))

(ert-deftest sparkweather-test-normalize-value-max ()
  "Maximum value should normalize to 7."
  (should (= (sparkweather--normalize-value 100 0 100 100) 7)))

(ert-deftest sparkweather-test-normalize-value-mid ()
  "Middle value should normalize to middle range."
  (let ((result (sparkweather--normalize-value 50 0 100 100)))
    (should (and (>= result 3) (<= result 4)))))

(ert-deftest sparkweather-test-format-sparkline-char-no-face-no-marker ()
  (should (equal (sparkweather--format-sparkline-char "▃" nil nil) "▃")))

(ert-deftest sparkweather-test-format-sparkline-char-with-face ()
  (let ((result (sparkweather--format-sparkline-char "▃" 'success nil)))
    (should (equal (get-text-property 0 'face result) 'success))))

(ert-deftest sparkweather-test-format-sparkline-char-with-marker ()
  (let ((result (sparkweather--format-sparkline-char "▃" nil t)))
    (should (string-prefix-p "\u202F" result))))

(ert-deftest sparkweather-test-format-sparkline-char-with-both ()
  (let ((result (sparkweather--format-sparkline-char "▃" 'warning t)))
    (should (string-prefix-p "\u202F" result))
    (should (equal (get-text-property 1 'face result) 'warning))))

(ert-deftest sparkweather-test-sparkline-empty-values ()
  "Empty values list returns nil."
  (should (null (sparkweather--sparkline nil))))

(ert-deftest sparkweather-test-sparkline-single-value ()
  "Single value creates single character sparkline."
  (let ((result (sparkweather--sparkline '(5))))
    (should (stringp result))
    (should (= (length result) 1))))

(ert-deftest sparkweather-test-sparkline-all-identical ()
  "All identical values use mid-height character."
  (let ((result (sparkweather--sparkline '(10 10 10 10))))
    (should (stringp result))
    (should (= (length result) 4))
    (should (string-match-p "▅" result))))

(ert-deftest sparkweather-test-sparkline-range ()
  "Values with range create varied sparkline."
  (let ((result (sparkweather--sparkline '(0 25 50 75 100))))
    (should (stringp result))
    (should (= (length result) 5))
    (should (string-prefix-p "▁" result))
    (should (string-suffix-p "█" result))))

(ert-deftest sparkweather-test-sparkline-with-highlights ()
  "Highlights apply faces to specific positions."
  (let* ((highlights '((1 . success) (3 . warning)))
         (result (sparkweather--sparkline '(10 20 30 40 50) highlights)))
    (should (stringp result))
    (should (equal (get-text-property 1 'face result) 'success))
    (should (equal (get-text-property 3 'face result) 'warning))))

(ert-deftest sparkweather-test-sparkline-with-current-hour ()
  "Current hour marker inserts narrow no-break space."
  (let ((result (sparkweather--sparkline '(10 20 30 40 50) nil 2)))
    (should (stringp result))
    (should (> (length result) 5))
    (should (string-match-p "\u202F" result))))

(ert-deftest sparkweather-test-sparkline-all-zero ()
  "All zero values create all-minimum sparkline."
  (let ((result (sparkweather--sparkline '(0 0 0 0))))
    (should (stringp result))
    (should (string-match-p "^▁+$" result))))

(ert-deftest sparkweather-test-require-field-exists ()
  (let ((alist '((foo . "bar") (baz . 42))))
    (should (equal (sparkweather--require-field alist 'foo) "bar"))
    (should (equal (sparkweather--require-field alist 'baz) 42))))

(ert-deftest sparkweather-test-require-field-missing ()
  (let ((alist '((foo . "bar"))))
    (should-error (sparkweather--require-field alist 'missing)
                  :type 'error)))

(ert-deftest sparkweather-test-calculate-ranges ()
  (pcase-let ((`(,temps ,precip-probs ,temp-min ,temp-max ,precip-max ,rainy-codes)
               (sparkweather--calculate-ranges sparkweather-test--sample-data)))
    (should (= (length temps) 10))
    (should (= (length precip-probs) 10))
    (should (= temp-min 8.0))
    (should (= temp-max 16.0))
    (should (= precip-max 45))
    (should (= (length rainy-codes) 8))
    (should (equal (apply #'max rainy-codes) 65))))

(ert-deftest sparkweather-test-calculate-ranges-single-datapoint ()
  "Single data point returns correct structure."
  (let ((data (list (sparkweather-hour-create 12 20.0 50 0.5 61))))
    (pcase-let ((`(,temps ,precip-probs ,temp-min ,temp-max ,precip-max ,rainy-codes)
                 (sparkweather--calculate-ranges data)))
      (should (equal temps '(20.0)))
      (should (equal precip-probs '(50)))
      (should (= temp-min 20.0))
      (should (= temp-max 20.0))
      (should (= precip-max 50))
      (should (equal rainy-codes '(61))))))

(ert-deftest sparkweather-test-calculate-ranges-all-temps-identical ()
  "All identical temperatures have same min and max."
  (let ((data (list (sparkweather-hour-create 12 15.0 20 0.2 53)
                    (sparkweather-hour-create 13 15.0 30 0.3 61)
                    (sparkweather-hour-create 14 15.0 25 0.2 53))))
    (pcase-let ((`(,_temps ,_precip-probs ,temp-min ,temp-max ,_precip-max ,_rainy-codes)
                 (sparkweather--calculate-ranges data)))
      (should (= temp-min 15.0))
      (should (= temp-max 15.0)))))

(ert-deftest sparkweather-test-calculate-ranges-no-precipitation ()
  "Zero precipitation yields empty rainy-codes."
  (let ((data (list (sparkweather-hour-create 12 15.0 0 0.0 0)
                    (sparkweather-hour-create 13 16.0 0 0.0 0))))
    (pcase-let ((`(,_temps ,_precip-probs ,_temp-min ,_temp-max ,precip-max ,rainy-codes)
                 (sparkweather--calculate-ranges data)))
      (should (= precip-max 0))
      (should (null rainy-codes)))))

(ert-deftest sparkweather-test-calculate-column-width-basic ()
  "Calculate column width from entry list."
  (let ((entries '((temp ["8—16°C" "sparkline"])
                   (precip ["45% ⛆" "sparkline"])
                   (lunch ["■ Lunch" "☀ clear sky"]))))
    (should (= (sparkweather--calculate-column-width entries) 7))))

(ert-deftest sparkweather-test-calculate-column-width-long-names ()
  "Calculate column width handles long window names."
  (let ((entries '((temp ["8—16°C" "sparkline"])
                   (pickup ["■ Afternoon pickup" "⛆ rain"]))))
    (should (= (sparkweather--calculate-column-width entries) 18))))

(ert-deftest sparkweather-test-calculate-column-width-empty ()
  "Calculate column width with empty entries returns 0."
  (should (= (sparkweather--calculate-column-width '()) 0)))

(ert-deftest sparkweather-test-time-window-data-extracts-lunch-hours ()
  (let ((result (sparkweather--time-window-data sparkweather-test--sample-data 12 14)))
    (should (equal (car result) '(3 4)))
    (should (equal (cadr result) '(53 61)))))

(ert-deftest sparkweather-test-time-window-data-extracts-commute-hours ()
  (let ((result (sparkweather--time-window-data sparkweather-test--sample-data 17 19)))
    (should (equal (car result) '(6 7)))
    (should (equal (cadr result) '(63 65)))))

(ert-deftest sparkweather-test-time-window-data-window-outside-range ()
  "Window completely outside data range returns empty lists."
  (let ((result (sparkweather--time-window-data sparkweather-test--sample-data 3 5)))
    (should (null (car result)))
    (should (null (cadr result)))))

(ert-deftest sparkweather-test-time-window-data-at-boundaries ()
  "Window at hour boundaries extracts correctly."
  (let ((result (sparkweather--time-window-data sparkweather-test--sample-data 0 1)))
    (should (equal (car result) '(0)))
    (should (equal (cadr result) '(0)))))

(ert-deftest sparkweather-test-time-window-data-single-hour ()
  "Single hour window extracts one data point."
  (let ((result (sparkweather--time-window-data sparkweather-test--sample-data 12 13)))
    (should (equal (car result) '(3)))
    (should (equal (cadr result) '(53)))))

(ert-deftest sparkweather-test-prepare-window-creates-lunch-plist ()
  (let* ((window '("Lunch" 12 14 success))
         (result (sparkweather--prepare-window sparkweather-test--sample-data window)))
    (should (equal (plist-get result :name) "Lunch"))
    (should (equal (plist-get result :face) 'success))
    (should (equal (plist-get result :indices) '((3 . success) (4 . success))))
    (should (equal (plist-get result :weather-info) '("⛆" "slight rain")))))

(ert-deftest sparkweather-test-prepare-window-creates-commute-plist ()
  (let* ((window '("Commute" 17 19 warning))
         (result (sparkweather--prepare-window sparkweather-test--sample-data window)))
    (should (equal (plist-get result :name) "Commute"))
    (should (equal (plist-get result :face) 'warning))
    (should (equal (plist-get result :indices) '((6 . warning) (7 . warning))))
    (should (equal (plist-get result :weather-info) '("⛆" "heavy rain")))))

(ert-deftest sparkweather-test-prepare-window-handles-empty-window ()
  (let* ((window '("Evening" 20 22 error))
         (result (sparkweather--prepare-window sparkweather-test--sample-data window)))
    (should (equal (plist-get result :name) "Evening"))
    (should (equal (plist-get result :face) 'error))
    (should (equal (plist-get result :indices) nil))
    (should (equal (plist-get result :weather-info) nil))))

(ert-deftest sparkweather-test-prepare-window-defaults-to-success-face ()
  (let* ((window '("Lunch" 12 14))
         (result (sparkweather--prepare-window sparkweather-test--sample-data window)))
    (should (equal (plist-get result :name) "Lunch"))
    (should (equal (plist-get result :face) 'success))
    (should (equal (plist-get result :indices) '((3 . success) (4 . success))))
    (should (equal (plist-get result :weather-info) '("⛆" "slight rain")))))

(ert-deftest sparkweather-test-prepare-windows-processes-multiple-windows ()
  (let* ((windows '(("Lunch" 12 14 success)
                    ("Commute" 17 19 warning)))
         (result (sparkweather--prepare-windows sparkweather-test--sample-data windows))
         (window-data (car result))
         (all-indices (cadr result)))
    (should (= (length window-data) 2))
    (should (equal all-indices '((3 . success) (4 . success) (6 . warning) (7 . warning))))))

(ert-deftest sparkweather-test-display-window-entry-formats-lunch ()
  (let* ((window-plist '(:name "Lunch" :face success :weather-info ("⛆" "slight rain")))
         (entry (sparkweather--display-window-entry window-plist)))
    (should (equal (car entry) 'lunch))
    (should (equal (aref (cadr entry) 0) (concat (propertize "■" 'face 'success) " Lunch")))
    (should (equal (aref (cadr entry) 1) "⛆ slight rain"))))

(ert-deftest sparkweather-test-display-window-entry-formats-commute ()
  (let* ((window-plist '(:name "Commute" :face warning :weather-info ("⛆" "heavy rain")))
         (entry (sparkweather--display-window-entry window-plist)))
    (should (equal (car entry) 'commute))
    (should (equal (aref (cadr entry) 0) (concat (propertize "■" 'face 'warning) " Commute")))
    (should (equal (aref (cadr entry) 1) "⛆ heavy rain"))))

(ert-deftest sparkweather-test-display-window-entry-returns-nil-when-no-weather ()
  (let* ((window-plist '(:name "Evening" :face error :weather-info nil))
         (entry (sparkweather--display-window-entry window-plist)))
    (should (null entry))))

(ert-deftest sparkweather-test-migrate-from-old-config-with-customized-values ()
  (let ((sparkweather-lunch-start-hour 11)
        (sparkweather-lunch-end-hour 13)
        (sparkweather-commute-start-hour 16)
        (sparkweather-commute-end-hour 18))
    (let ((result (sparkweather--migrate-deprecated-config)))
      (should (equal result '(("Lunch" 11 13 success)
                             ("Commute" 16 18 warning)))))))

(ert-deftest sparkweather-test-migrate-from-old-config-with-default-values ()
  (let ((sparkweather-lunch-start-hour 12)
        (sparkweather-lunch-end-hour 14)
        (sparkweather-commute-start-hour 17)
        (sparkweather-commute-end-hour 19))
    (let ((result (sparkweather--migrate-deprecated-config)))
      (should (null result)))))

(ert-deftest sparkweather-test-format-migration-message ()
  (let ((windows '(("Lunch" 11 13 success)
                   ("Commute" 16 18 warning))))
    (let ((message (sparkweather--format-migration-message windows)))
      (should (string-match-p "sparkweather-time-windows" message))
      (should (string-match-p "'((\"Lunch\" 11 13 success)" message))
      (should (string-match-p "(\"Commute\" 16 18 warning))" message)))))

(ert-deftest sparkweather-test-detect-end-equals-start ()
  (let ((windows '(("Invalid" 12 12 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (= (length invalid) 1))
      (should (equal (car invalid) '("Invalid" 12 12 invalid-range))))))

(ert-deftest sparkweather-test-detect-end-before-start ()
  (let ((windows '(("Backwards" 14 12 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (= (length invalid) 1))
      (should (equal (car invalid) '("Backwards" 14 12 invalid-range))))))

(ert-deftest sparkweather-test-detect-start-hour-negative ()
  (let ((windows '(("Negative" -1 12 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (= (length invalid) 1))
      (should (equal (car invalid) '("Negative" -1 12 out-of-range))))))

(ert-deftest sparkweather-test-detect-start-hour-too-large ()
  (let ((windows '(("TooLarge" 24 25 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (>= (length invalid) 1))
      (should (member '("TooLarge" 24 25 out-of-range) invalid)))))

(ert-deftest sparkweather-test-detect-end-hour-negative ()
  (let ((windows '(("NegativeEnd" 12 -5 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (= (length invalid) 1))
      (should (equal (car invalid) '("NegativeEnd" 12 -5 out-of-range))))))

(ert-deftest sparkweather-test-detect-end-hour-too-large ()
  (let ((windows '(("EndTooLarge" 12 24 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (= (length invalid) 1))
      (should (equal (car invalid) '("EndTooLarge" 12 24 out-of-range))))))

(ert-deftest sparkweather-test-detect-both-hours-out-of-range ()
  "Verify that a window with both hours invalid is only reported once."
  (let ((windows '(("BothBad" 25 30 error))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should invalid)
      (should (= (length invalid) 1))
      (should (equal (car invalid) '("BothBad" 25 30 out-of-range))))))

(ert-deftest sparkweather-test-detect-multiple-invalid-windows ()
  (let ((windows '(("Invalid1" 12 12 error)
                   ("Valid" 12 14 success)
                   ("Invalid2" 18 15 warning))))
    (let ((invalid (sparkweather--detect-invalid-windows windows)))
      (should (= (length invalid) 2))
      (should (member '("Invalid1" 12 12 invalid-range) invalid))
      (should (member '("Invalid2" 18 15 invalid-range) invalid)))))

(ert-deftest sparkweather-test-detect-adjacent-windows-not-overlap ()
  (let ((windows '(("Lunch" 12 14 success)
                   ("Break" 14 15 warning))))
    (should (null (sparkweather--detect-window-overlaps windows)))))

(ert-deftest sparkweather-test-detect-simple-overlap ()
  (let ((windows '(("Lunch" 12 14 success)
                   ("Tea" 13 15 warning))))
    (let ((overlaps (sparkweather--detect-window-overlaps windows)))
      (should overlaps)
      (should (= (length overlaps) 1))
      (should (equal (car overlaps) '("Lunch" "Tea" 13 14))))))

(ert-deftest sparkweather-test-detect-multiple-overlaps ()
  (let ((windows '(("Morning" 9 12 success)
                   ("Lunch" 11 14 warning)
                   ("Afternoon" 13 16 error))))
    (let ((overlaps (sparkweather--detect-window-overlaps windows)))
      (should (= (length overlaps) 2))
      (should (member '("Morning" "Lunch" 11 12) overlaps))
      (should (member '("Lunch" "Afternoon" 13 14) overlaps)))))

(ert-deftest sparkweather-test-detect-complete-containment ()
  (let ((windows '(("All Day" 9 17 success)
                   ("Lunch" 12 14 warning))))
    (let ((overlaps (sparkweather--detect-window-overlaps windows)))
      (should overlaps)
      (should (= (length overlaps) 1))
      (should (equal (car overlaps) '("All Day" "Lunch" 12 14))))))

(provide 'sparkweather-test)

;;; sparkweather-test.el ends here
