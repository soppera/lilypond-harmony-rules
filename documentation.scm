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

(define-module (documentation)
  #:use-module (oop goops)
  #:export (define-macro-with-doc
             define-class-with-doc
             set-current-module-documentation!))

(define-macro (define-macro-with-doc name-args doc . body)
  `(begin
     (define-macro ,name-args . ,body)
     (set-object-property! (module-ref (current-module) (quote ,(car name-args)))
                           'documentation ,doc)
     ))

(define-macro-with-doc (define-class-with-doc class supers doc . rest)
  "Same as (define-class) but with an additional ‘doc’ parameter.

Syntax: (define-class-with-doc CLASS (SUPER ...)
           DOC
           SLOT-DESCRIPTION ...
           CLASS-OPTION ...)

The provided documentation string will be set as the 'documentation
object property."
  `(begin
     (define-class ,class ,supers . ,rest)
     (set-object-property! ,class 'documentation ,doc)
     *unspecified*))

(define (set-current-module-documentation! doc)
  "Sets the 'documentation object-property of the current module to DOC."
  (set-object-property! (current-module) 'documentation doc))

