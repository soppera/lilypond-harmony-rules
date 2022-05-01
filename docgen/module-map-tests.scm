;; Lilypond Harmony Rules tests harmony rules of Lilypond scores.
;; Copyright (C) 2021  Stéphane SOPPERA
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

(use-modules (docgen module-map))
(use-modules (docgen module-map-tests-module))

(define (test-start name)
  (newline)
  (display "========== Test: ")
  (display name)
  (display " ==========")
  (newline))

(define (test-module-ref symbol)
  "Return the object bound to SYMBOL in ‘(docgen module-map-tests-module)’."
  (module-ref (resolve-module
               '(docgen module-map-tests-module)
               ;; autoload
               #f)
              symbol))

(test-start "map-module")
(let* ((module-map (map-module '(docgen module-map-tests-module)))
       (something-id (or (unique-id module-map <something>)
                         (error "can't find unique id for <something>")))
       )
  (if (not (hash-ref (unique-ids module-map) something-id))
      (error (with-output-to-string
               (lambda ()
                 (display "(hash-ref (unique-ids module-map) ")
                 (write something-id)
                 (display ") → ")
                 (display from-something-id)
                 (display "; but wanted #t")))))
  ;; Test ‘object-is-public’.
  (if (not (hashq-ref (object-is-public module-map) <something>))
      (error "(hashq-ref (object-is-public module-map) <something>) → #f"))
  (let* ((generic-fn (test-module-ref 'generic-fn))
         (got (hashq-ref (object-is-public module-map) generic-fn 'found)))
    (if got
        (error (with-output-to-string
                 (lambda ()
                   (display "(hashq-ref (object-is-public module-map) ")
                   (write generic-fn)
                   (display " #t) → ")
                   (write got))))))
  ;; Test ‘classes’.
  (let ((got (classes module-map))
        (want (list (cons '<something> <something>))))
    (if (not (equal? got want))
        (error (with-output-to-string
                 (lambda ()
                   (display "(classes module-map) → ")
                   (display got)
                   (display "; wanted ")
                   (display want))))))
  ;; Test ‘generics’.
  (let ((got (generics module-map))
        (want (list (cons 'generic-fn (test-module-ref 'generic-fn))
                    (cons 'setter:width (setter width))
                    (cons 'width width))))
    (if (not (equal? got want))
        (error (with-output-to-string
                 (lambda ()
                   (display "(generics module-map) → ")
                   (display got)
                   (display "; wanted ")
                   (display want))))))
  ;; Test ‘module-documentation’.
  (let ((got (module-documentation module-map))
        (want "Module to test module-map."))
    (if (not (string=? got want))
        (error (with-output-to-string
                 (lambda ()
                   (display "(module-documentation module-map) → ")
                   (display got)
                   (display "; wanted ")
                   (display want))))))
  ;; Test ‘generics’.
  (let* ((generic-fn (test-module-ref 'generic-fn))
         (got (hashq-ref (generic-is-partial module-map) generic-fn #t)))
    (if got
        (error (with-output-to-string
                 (lambda ()
                   (display "(hashq-ref (generic-is-partial module-map) ")
                   (write generic-fn)
                   (display " → ")
                   (display got)
                   (display "; wanted #f"))))))
  ;; Test ‘procedures’.
  (let ((got (procedures module-map))
        (want (list (cons 'some-proc some-proc))))
    (if (not (equal? got want))
        (error (with-output-to-string
                 (lambda ()
                   (display "(procedures module-map) → ")
                   (display got)
                   (display "; wanted ")
                   (display want))))))
  ;; Test ‘macros’.
  (let ((got (macros module-map))
        (want (list (cons 'some-macro (test-module-ref 'some-macro)))))
    (if (not (equal? got want))
        (error (with-output-to-string
                 (lambda ()
                   (display "(macros module-map) → ")
                   (display got)
                   (display "; wanted ")
                   (display want))))))
  )
  
