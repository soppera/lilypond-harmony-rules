(define-module (documentation)
  #:use-module (oop goops)
  #:export (define-macro-with-doc
             define-class-with-doc))

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
