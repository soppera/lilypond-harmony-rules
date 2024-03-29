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

(tests
 (test-case "test1"
   ;; Should fail.
   (test-that = (+ 2 1) 4)
   (test-case "sub-test1"
     (test-that = 3 2)
     ;; Should be caught and converted to an error.
     (/ 1 0)
     ;; Should not be reached because of the previous exception.
     (test-that = 3 6))
   ;; Should pass.
   (test-that = (+ 2 1) 3)
   ;; Should pass.
   (test-that eq? 'a (car '(a b c)))
   ;; Should fail and be reached as the (/ 1 0) exception should be
   ;; caught by parent test case.
   (test-that eq? 'a (cadr '(a b c)))))
  
