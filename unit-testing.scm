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
  #:export (tests test-case test-that current-test-path)
  #:use-module (logging))

;; Usage:
;;
;;

;; Emacs customization:
;;
;;   (put 'test-case 'scheme-indent-function 1)
;;   (put 'eval-when 'scheme-indent-function 1)


;; Define an empty eval-when for Guile 1.8.
(cond-expand
 (guile-2)
 (guile
  (define-macro (eval-when conds . code)
    `(begin ,@code))))

;; Define symbols used by macros
(eval-when (expand load eval)
  (define *state* 'not-started)
  (define (state) *state*)
  (define (set-state! state) (set! *state* state))
  (define *current-test-path* '())
  (define (current-test-path)
    "Returns the path of the current test."
    *current-test-path*)
  (define (set-current-test-path! path) (set! *current-test-path* path))
  (define *errors* '())
  (define (errors) *errors*)
  (define (push-errors! err)
    (set! *errors* (cons err *errors*))
    (let* ((source (assq-ref err 'source))
           (filename (or (assq-ref source 'filename) "?"))
           (line (cond ((assq-ref source 'line) => 1+) (else "?")))
           (column (cond ((assq-ref source 'column) => 1+) (else "?"))))
      (display filename (current-error-port))
      (display ":" (current-error-port))
      (display line (current-error-port))
      (display ":" (current-error-port))
      (display column (current-error-port))
      (display ": in test " (current-error-port))
      (write (assq-ref err 'test-path))
      (display ": ")
      (display (assq-ref err 'message))
      (newline))))

(cond-expand
 ;; Guile >= 2.0 version.
 (guile-2
  (define-syntax tests
    (syntax-rules ()
      ((_ content ...)
       (dynamic-wind
           (lambda ()
             (if (not (eq? *state* 'not-started))
                 (error (call-with-output-string
                         (lambda (port)
                           (log-info "only one (tests) can be used" port)))))
             (set! *state* 'started))
           (lambda () (begin content ...))
           (lambda ()
             (set! *state* 'done)
             (display "errors: ") (write *errors*) (newline))))))

  (define-syntax test-case
    (syntax-rules ()
      ((_ name content ...)
       (let ((previous-test-path (current-test-path))
             (new-test-path (cons name (current-test-path))))
         (dynamic-wind
             (lambda ()
               (if (not (eq? *state* 'started))
                   (error (call-with-output-string
                           (lambda (port)
                             (log-info "(test-case) can't be used outside (tests)"
                                       port)))))
               (set-current-test-path! new-test-path))
             (lambda ()
               (catch #t
                 (lambda () content ...)
                 (lambda (key . args)
                   (display "** caught key: ")
                   (write key)
                   (display "; args: ")
                   (write args)
                   (newline))))
             (lambda ()
               (set-current-test-path! previous-test-path)))))))

  (define-syntax test-that
    (lambda (s)
      (syntax-case s ()
        ((_ pred got want)
         (with-syntax ((pred-name (with-output-to-string
                                    (lambda ()
                                      (write (syntax->datum #'pred)))))
                       (got-name (with-output-to-string
                                   (lambda ()
                                     (write (syntax->datum #'got)))))
                       (want-name (with-output-to-string
                                    (lambda ()
                                      (write (syntax->datum #'want)))))
                       (loc (datum->syntax s (syntax-source s))))
           #'(begin
               (if (not (eq? *state* 'started))
                   (error (call-with-output-string
                           (lambda (port)
                             (log-info "(test-that) can't be used outside (tests)"
                                       port)))))
               (let ((g got) (w want))
                 (if (not (pred g w))
                     (push-errors!
                      (list
                       (cons 'source 'loc)
                       (cons 'test-path (reverse *current-test-path*))
                       (cons 'message
                             (string-append
                              "("
                              pred-name
                              " " got-name " " want-name ") failed with "
                              got-name " = "
                              (with-output-to-string (lambda () (write g)))
                              " and " want-name " = "
                              (with-output-to-string (lambda () (write w))))))))))))))))

 ;; Guile 1.8 version.
 (guile
  (define-macro (tests . content)
    (let ((state-symb (make-symbol "state"))
          (set-state!-symb (make-symbol "set-state!"))
          (errors-symb (make-symbol "errors")))
      `(let ((,state-symb ,state)
             (,set-state!-symb ,set-state!)
             (,errors-symb ,errors))
         (dynamic-wind
             (lambda ()
               (if (not (eq? (,state-symb) 'not-started))
                   (error "only one (tests) can be used"))
               (,set-state!-symb 'started))
             (lambda () ,@content)
             (lambda ()
               (,set-state!-symb 'done)
               (display "errors: ") (write (,errors-symb)) (newline))))))
  (define-macro (test-case name . content)
    (let ((previous-test-path-symb (make-symbol "previous-test-path"))
          (new-test-path-symb (make-symbol "new-test-path")))
      `(let ((,previous-test-path-symb (,current-test-path))
             (,new-test-path-symb (cons ,name (,current-test-path))))
         (dynamic-wind
             (lambda ()
               (if (not ,previous-test-path-symb)
                   (error "(test-case) can't be used outside (tests)"))
               (,set-current-test-path! ,new-test-path-symb))
             (lambda () ,@content)
             (lambda () (,set-current-test-path! ,previous-test-path-symb))))))
  (define-macro (test-that pred got want)
    (let ((g-symb (make-symbol "g"))
          (w-symb (make-symbol "w"))
          (current-test-path-symb (make-symbol "current-test-path"))
          (push-errors!-symb (make-symbol "push-errors!")))
      (define (to-string expr)
        (with-output-to-string
          (lambda () (write expr))))
      `(let ((,g-symb ,got) (,w-symb ,want)
             (,push-errors!-symb ,push-errors!)
             (,current-test-path-symb ,current-test-path))
         (if (not (,pred ,g-symb ,w-symb))
             (,push-errors!-symb
              (list
               (cons 'source '())
               (cons 'test-path (reverse (,current-test-path-symb)))
               (cons 'message
                     (string-append
                      "(" ,(to-string pred)
                      " " ,(to-string got) " " ,(to-string want) ") failed with "
                      ,(to-string got) " = " (with-output-to-string (lambda () (write ,g-symb)))
                      " and " ,(to-string want) " = " (with-output-to-string (lambda () (write ,w-symb)))))))))))))

