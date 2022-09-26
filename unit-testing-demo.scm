;; Lilypond Harmony Rules tests harmony rules of Lilypond scores.
;; Copyright (C) 2022  Stéphane SOPPERA
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file tests the (unit-testing) module. It should either be ran
;; with the `guile` binary or via `unit-testing-demo.ly` and Lilypond.

(cond-expand
 (guile-2
  (add-to-load-path (dirname (current-filename))))
 (guile
  ;; There is no way in guile-1.8 to know the file name. We assume the
  ;; demo is ran with the correct current directory.
  (set! %load-path (cons "." %load-path))))

(use-modules (unit-testing))
(use-modules (logging))

(test-case "test1"
  (log-info "1.")
  (print-test-case)
  (test-case "sub-test1"
    (log-info "2.1.")
    (print-test-case))
  (log-info "3.")
  (print-test-case))
  