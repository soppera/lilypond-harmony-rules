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

(define-module (tools)
  #:use-module (documentation)
  #:export (push! map-vector options->alist dolist))

(define-macro-with-doc (push! elt var)
  "A macro that push an element to the front of a list.

‘(push! elt var)’ is equivalent to ‘(set! var (const elt var))’."
  `(set! ,var (cons ,elt ,var)))

(define-macro-with-doc (dolist head . body)
  "(dolist (var list [result]) body...) a macro that iterate on a list.

Example:
  (dolist (x '(1 2 3))
    (display x) (newline))

To have correct indentation in Emacs, use:
  (put 'dolist 'scheme-indent-function 1)."
  (if (or (not (list? head))
          (< (length head) 2)
          (> (length head) 3))
      (error "dolist macro expect a list of at least two elements and at most 3"))
  (let ((var (car head))
        (l (cadr head))
        (has-ret (= (length head) 3))
        (tmp (gensym))
        (loop (gensym)))
    `(let ((,tmp ,l))
       (cond
        ((not (null? ,tmp))
         (let ,loop ((,var (car ,tmp))
                    (,tmp (cdr ,tmp)))
           ,@body
           (if (null? ,tmp)
               ,(if has-ret (caddr head))
               (,loop (car ,tmp) (cdr ,tmp)))))
        (else ,(if has-ret (caddr head)))))))
  
(define (map-vector vector list)
  "Map each integer in @var{list} with the corresponding value in the @{vector}."
  (map (lambda (i) (vector-ref vector i)) list))

(define (options->alist options)
  "Take the elements of options two by two to create an alist.

For example:
  (options->alist '(#:a 3 #:b #t))
will return:
  '((#:a . 3) (#:b . #t))"
  (do ((l options (cddr l))
       (al '()))
      ((null? l) al)
    (if (null? (cdr l))
        (error "uneven number of elements in OPTIONS"))
    (push! (cons (car l) (cadr l)) al)))

