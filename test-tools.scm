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

(define-module (test-tools)
  #:use-module (lily)
  #:use-module (oop goops)
  #:use-module (harmony-rules)
  #:export (test-start
	    test-pretty-print
	    make-note
	    timed-note
	    remove-length-and-origin))

(define (test-start name)
  (display (format "\n========== Test: ~a ==========\n" name))
  (add-text #{ \markup { "Test:" $name } #}))

(define (test-pretty-print obj indent)
  "Pretty print the input object with INDENT as prefix of each line."
  (define (rec obj indent)
    (let ((strings '()))
      (cond
       ((null? obj)
        (set! strings (cons "'()" strings)))         
       ((list? obj)
        (set! strings (cons "(" strings))
        (do ((head obj (cdr head))
             (sub-indent (string-append indent " "))
             (first-line #t #f))
            ((null? head))
          (if (not first-line)
              (begin
                (set! strings (cons "\n" strings))
                (set! strings (cons sub-indent strings))))
          (set! strings
                (cons (rec
                       (car head)
                       sub-indent)
                      strings)))
        (set! strings (cons ")" strings)))
       ((pair? obj)
        (set! strings
              (cons (format "(~a . ~a)"
                            (rec (car obj) indent)
                            (rec (cdr obj) indent))
                    strings)))
       ((ly:music? obj)
        (set! strings
              (cons (format-music obj
                                  'hidden-properties '(types)
                                  'one-line #t)
                    strings)))
       (#t (set! strings (cons (object->string obj) strings))))
      (string-join (reverse strings) "")))
  (string-append indent (rec obj indent)))

(define (make-note pitch duration)
  (let ((duration (ly:make-duration duration)))
    (make-music
     'NoteEvent
     'pitch pitch
     'duration duration)))

(define (timed-note moment pitch duration)
  (make <timed-note>
    #:moment (ly:make-moment moment)
    #:music (make-note pitch duration)))

(define (remove-length-and-origin music)
  (music-map
   (lambda (m)
     (let ((new-m (ly:make-music (ly:prob-immutable-properties m)))
           (props (sort (filter (lambda (pair)
                                  (not (or (eq? (car pair) 'origin)
                                           (eq? (car pair) 'length))))
                                (ly:music-mutable-properties m))
                        (lambda (a b) (string<? (symbol->string (car a))
                                                (symbol->string (car b)))))))
       (for-each (lambda (pair) (ly:music-set-property! new-m (car pair) (cdr pair)))
                 props)
       new-m))
   music))
