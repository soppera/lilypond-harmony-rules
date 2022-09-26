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

(define-module (unit-testing)
  #:export (test-case current-test-path))

;; Usage:
;;
;;

;; Emacs customization:
;;
;;   (put 'test-case 'scheme-indent-function 1)
;;   (put 'eval-when 'scheme-indent-function 1)


(define *current-test-path* '())

(cond-expand
 ;; Guile >= 2.0 version.
 (guile-2
  (eval-when (expand load eval)
    (define (current-test-path) *current-test-path*)
    (define (set-current-test-path! path) (set! *current-test-path* path)))
  
  (define-syntax test-case
    (syntax-rules ()
      ((_ name content ...)
       (let ((previous-test-path (current-test-path))
             (new-test-path (cons name (current-test-path))))
         (dynamic-wind
             (lambda () (set-current-test-path! new-test-path))
             (lambda () content ...)
             (lambda () (set-current-test-path! previous-test-path))))))))

 ;; Guile 1.8 version.
 (guile
  (use-modules (ice-9 syncase))

  (define (current-test-path) *current-test-path*)
  (define (set-current-test-path! path) (set! *current-test-path* path))
  
  (define-macro (test-case name . content)
    (let ((previous-test-path-symb (make-symbol "previous-test-path"))
          (new-test-path-symb (make-symbol "new-test-path")))
      `(let ((,previous-test-path-symb (,current-test-path))
             (,new-test-path-symb (cons ,name (,current-test-path))))
         (dynamic-wind
             (lambda () (,set-current-test-path! ,new-test-path-symb))
             (lambda () ,@content)
             (lambda () (,set-current-test-path! ,previous-test-path-symb))))))))

(define (current-test-path)
  "Returns the path of the current test."
  (reverse *current-test-path*))
