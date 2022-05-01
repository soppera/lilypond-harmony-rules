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

(use-modules (docgen introspection))
(use-modules (oop goops))

(define (test-start name)
  (newline)
  (display "========== Test: ")
  (display name)
  (display " ==========")
  (newline))

(define-method (some-generic-function-1 (first <string>) second (third <integer>)))

(define-method (some-generic-function-2 (first <string>) (second <integer>))
  (next-method))
(define-method (some-generic-function-2 first (second <integer>)))

(define (find-method generic-fn specializers)
  (do ((methods (generic-function-methods generic-fn)
                (cdr methods)))
      ((or (null? methods)
           (equal? (method-specializers (car methods)) specializers))
       (if (null? methods)
           (error "can't find the method with the given specializers" generic-fn specializers)
           (car methods)))))

(test-start "display-arguments")
(for-each
 (lambda (test)
   (let* ((args (car test))
          (want (cdr test))
          (got (with-output-to-string
                 (lambda ()
                   (apply display-arguments args)))))
     (if (not (string= got want))
         (error (with-output-to-string
                  (lambda ()
                    (display "(display-arguments ...) returned unexpected result")
                    (newline)
                    (display "got:") (newline)
                    (display got) (newline)
                    (display "want:") (newline)
                    (display want)))))))
 ;; Tests ((proc specializers optionals) . want).
 (list
  (cons (list (lambda () #t) #f #f)
        "")
  (cons (list (lambda (first second third) #t) #f #f)
        " first second third")
  (cons (list (lambda (first second third) #t) #f '(#t #f #t))
        " [first] second [third]")
  (cons (list (lambda (first second third . rest) #t) #f #f)
        " first second third . rest")
  (cons (list (lambda arguments #t) #f #f)
        " <arguments>")
  (let ((method (find-method some-generic-function-1 (list <string> <top> <integer>))))
    (cons (list #f (method-specializers method) #f)
          " (? <string>) ? (? <integer>)"))
  (let ((method (find-method some-generic-function-1 (list <string> <top> <integer>))))
    (cons (list (method-procedure method) (method-specializers method) #f)
          " (first <string>) second (third <integer>)"))
  (let ((method (find-method some-generic-function-2 (list <top> <integer>))))
    (cons (list (method-procedure method) (method-specializers method) #f)
          " first (second <integer>)"))
  (let ((method (find-method some-generic-function-2 (list <string> <integer>))))
    (cons (list (method-procedure method) (method-specializers method) #f)
          " (? <string>) (? <integer>)"))
  )) 
