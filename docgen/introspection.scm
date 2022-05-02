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

(define-module (docgen introspection)
  #:export (display-arguments)
  #:use-module (oop goops)
  #:use-module ((tools)
                #:select (push!)))

;; In Guile 2.2, ‘procedure-source’ seems to always return #f, but we
;; now can use ‘program-lambda-list’ to get the data directly. This is
;; not available in Guile 1.8 though.
(cond-expand
 (guile-2
  (use-modules (system vm program))
  (define use-program-api #t))
 (else
  (define use-program-api #f)))

(define (proc-lambda-list proc)
  "Returns a representation of the PROC arguments as they would appear
in the corresponding ‘lambda’.

Returns #f if not available."
  (if use-program-api
      ;; For Guile 2.2 where procedure-source does not work.
      (program-lambda-list proc)
      (let ((src (procedure-source proc)))
        (cond
         ((not src) #f)
         ((not (pair? src)) (error "unexpected source" src))
         ((not (eq? (car src) 'lambda))
          (error "unexpected source first element" src))
         ((not (pair? (cdr src)))
          (error "unexpected source, not enough elements" src))
         (else (cadr src))))))

(define (display-arguments proc specializers optionals)
  "Print the arguments of the PROC with optional SPECIALIZERS.

If SPECIALIZERS is #f, print only argument names. If PROC is #f or its
arguments can't be extracted but SPECIALIZERS is provided, use those
with '?' as argument name.

This function is based on the procedure source (Guile 1.8) or
introspection (Guile 2.2); it may not alway work. It prints '?' if it
fails.

Returns the arguments names are found, else #f."
  (if (and (not proc) (not specializers))
      (error "parameters PROC and SPECIALIZERS can't be both #f")) 
  (let ((lambda-list (and proc (proc-lambda-list proc))))
    (cond
     ((not lambda-list)
      (cond
       ((not specializers)
        (error "can't get the parameter names of the procedure!" proc)
        (display " ?")
        #f)
       (else 
        ;; ‘specializers’ is provided but ‘lambda-list’ is #f.
        (do ((specializer specializers (cdr specializer))
             (optional optionals (and optional (cdr optional))))
            ((cond
              ((null? specializer) #t)
              ((not (pair? specializer))
               (display " . (? ")
               (display (class-name specializer))
               (display ")")
               #t)
              (else #f))
             ;; We don't return any name.
             #f)
          (let ((has-specializer (not (eq? (car specializer) <top>)))
                (is-optional (and optional (car optional))))
            (display " ")
            (if has-specializer (display "("))
            (if is-optional (display "["))
            (display "?")
            (if is-optional (display "]"))
            (if has-specializer
                (begin
                  (display " ")
                  (display (class-name (car specializer)))
                  (display ")"))))))))
     ((not (or (null? lambda-list) (pair? lambda-list)))
      (let ((stderr (current-error-port)))
        (display "lambda-list: " stderr)
        (display lambda-list stderr)
        (newline stderr))
      ;; We can have (lambda args ...).
      (display " <")
      (display lambda-list)
      (display ">")
      #f)
     (else
      ;; ‘lambda-list’ is not #f
      (do ((head lambda-list (cdr head))
           (specializer specializers (and specializer
                                          (cdr specializer)))
           (optional optionals (and optional (cdr optional)))
           (names '()))
          ((cond
            ((null? head) #t)
            ((not (pair? head))
             (display " . ")
             (display head)
             #t)
            (else #f))
           (reverse! names))
        (let ((has-specializer (and specializer
                                    (not (eq? (car specializer) <top>))))
              (is-optional (and optional (car optional))))
          (display " ")
          (if has-specializer (display "("))
          (if is-optional (display "["))
          (display (car head))
          (push! (car head) names)
          (if is-optional (display "]"))
          (if has-specializer
              (begin
                (display " ")
                (display (class-name (car specializer)))
                (display ")")))))))))

