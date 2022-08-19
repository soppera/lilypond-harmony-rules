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

(define-module (logging)
  #:export (log-info))

;; Usage:
;;
;;   (log-info "something")
;;   (log-info (with-output-to-string
;;                (labmda ()
;;                  (display "x: ")
;;                  (display x))))
;;
(cond-expand
 (guile-2
  (define-syntax log-info
    (lambda (x)
      (syntax-case x ()
        ((log-info msg)
         (with-syntax ((loc (datum->syntax x (syntax-source x))))
           #'(let ()
               (display (assq-ref 'loc 'filename))
               (display ":")
               (display (1+ (assq-ref 'loc 'line)))
               (display ":")
               (display (1+ (assq-ref 'loc 'column)))
               (display ": ")
               (display msg)
               (newline)
               (force-output))))))))
 (guile ;; Version Guile 1.8.
  (define log-info
    (procedure->memoizing-macro
     (lambda (x env)
       (let* ((the-loc (source-properties x))
              (filename (assq-ref the-loc 'filename))
              (line (and (assq-ref the-loc 'line) (1+ (assq-ref the-loc 'line))))
              (column (and (assq-ref the-loc 'column) (1+ (assq-ref the-loc 'column)))))
         `(begin
            (display ,filename)
            (display ":")
            (display ,line)
            (display ":")
            (display ,column)
            (display ": ")
            (display ,(cadr x))
            (newline)
            (force-output))))))))
  
