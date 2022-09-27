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
         (column (cond ((assq-ref source 'column) => 1+) (else "?")))
         (*err* (current-error-port)))
    (flush-all-ports)
    (newline *err*)
    (display filename *err*)
    (display ":" *err*)
    (display line *err*)
    (display ":" *err*)
    (display column *err*)
    (display ": ERROR in test " *err*)
    (write (assq-ref err 'test-path) *err*)
    (display ": " *err*)
    (display (assq-ref err 'message) *err*)
    (newline *err*)
    (let ((s (assq-ref err 'stack)))
      (if s
          (begin (display "STACK:" *err*) (newline *err*)
                 (display-backtrace s *err*) (newline *err*))))))

(define (tests-done)
  (if (not (null? *errors*))
      (let ((err (current-error-port)))
        (flush-all-ports)
        (newline err)
        (display "** " err)
        (display (length *errors*) err)
        (display " errors in tests" err)
        (newline err)
        (force-output err)
        (let ((ly:error-var (module-variable (resolve-module '(lily)) 'ly:error)))
          (if ly:error-var
              ((variable-ref ly:error-var) "** some tests failed **")
              (exit 1))))))

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
             (tests-done))))))

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
               (let ((stack #f))
                 (catch #t
                   (lambda () content ...)
                   (lambda (key . args)
                     (push-errors!
                      (list
                       (cons 'source '())
                       (cons 'stack stack)
                       (cons 'test-path (reverse *current-test-path*))
                       (cons 'message
                             (string-append
                              "an exception occurred with key: "
                              (with-output-to-string (lambda () (write key)))
                              " and args: "
                              (with-output-to-string (lambda () (write args))))))))
                   (lambda args (set! stack (make-stack #t))))))
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
          (tests-done-symb (make-symbol "tests-done")))
      `(let ((,state-symb ,state)
             (,set-state!-symb ,set-state!)
             (,tests-done-symb ,tests-done))
         (dynamic-wind
             (lambda ()
               (if (not (eq? (,state-symb) 'not-started))
                   (error "only one (tests) can be used"))
               (,set-state!-symb 'started))
             (lambda () ,@content)
             (lambda ()
               (,set-state!-symb 'done)
               (,tests-done-symb))))))
  (define-macro (test-case name . content)
    (let ((previous-test-path-symb (make-symbol "previous-test-path"))
          (new-test-path-symb (make-symbol "new-test-path"))
          (push-errors!-symb (make-symbol "push-errors!"))
          (stack-symb (make-symbol "stack")))
      `(let ((,previous-test-path-symb (,current-test-path))
             (,new-test-path-symb (cons ,name (,current-test-path)))
             (,push-errors!-symb ,push-errors!))
         (dynamic-wind
             (lambda ()
               (if (not ,previous-test-path-symb)
                   (error "(test-case) can't be used outside (tests)"))
               (,set-current-test-path! ,new-test-path-symb))
             (lambda ()
               (let ((,stack-symb #f))
                 (catch #t
                   (lambda () ,@content)
                   (lambda (key . args)
                     (,push-errors!-symb
                      (list
                       (cons 'source '())
                       (cons 'stack (make-stack #t))
                       (cons 'test-path (reverse ,new-test-path-symb))
                       (cons 'message
                             (string-append
                              "an exception occurred with key: "
                              (with-output-to-string (lambda () (write key)))
                              " and args: "
                              (with-output-to-string (lambda () (write args))))))))
                   (lambda args (set! ,stack-symb (make-stack #t))))))
             (lambda () (,set-current-test-path! ,previous-test-path-symb))))))
  (define test-that
    (procedure->memoizing-macro
     (lambda (s env)
       (let ((l (length s)))
         (cond
          ((< l 4)
           `(scm-error 'wrong-number-of-args "test-that" "not enough arguments" '() '()))
          ((> l 4)
           `(scm-error 'wrong-number-of-args "test-that" "not enough arguments" '() '()))
          (else
           (let* ((pred (cadr s))
                  (got (caddr s))
                  (want (cadddr s))
                  (the-loc (source-properties s)))
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
                        (cons 'source ',the-loc)
                        (cons 'test-path (reverse (,current-test-path-symb)))
                        (cons 'message
                              (string-append
                               "(" ,(to-string pred)
                               " " ,(to-string got) " " ,(to-string want) ") failed with "
                               ,(to-string got) " = " (with-output-to-string (lambda () (write ,g-symb)))
                               " and " ,(to-string want) " = " (with-output-to-string (lambda () (write ,w-symb)))))))))))))))))))

