(define-module (docgen module-map-tests-module)
  #:export (<something> width some-proc some-macro)
  #:use-module (documentation)
  #:use-module (oop goops))

(set-current-module-documentation! "Module to test module-map.")

(define-class-with-doc <something> ()
  "The documentation of <something>."
  (width #:init-value 0 #:accessor width))

;; Private generic function (used to test 
(define-method (generic-fn (s <something>) (x <integer>))
  (+ (width s) x))

(define (some-proc) #f)

(define-macro-with-doc (some-macro x)
  "Documentation."
  x)
