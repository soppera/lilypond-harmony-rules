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

(define-module (generate-doc)
  #:export (write-texinfo)
  #:use-module ((lily)
                #:select (ly:music-function?
                          ly:music-function-signature
                          ly:music-function-extract))
  #:use-module (documentation)
  #:use-module (tools)
  #:use-module ((graph)
                #:select (digraph
                          subgraph
                          push-arc!
                          connected-components
                          topological-sort))
  #:use-module (ice-9 documentation)
  #:use-module (oop goops)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex))

;; In Guile 2.2, ‘procedure-source’ seems to always return #f, but we
;; now can use ‘program-lambda-list’ to get the data directly. This is
;; not available in Guile 1.8 though.
(define use-program-api #f)
(if (string>= (major-version) "2")
    (begin 
      (use-modules (system vm program))
      (set! use-program-api #t)))

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

If SPECIALIZERS is #f, print only argument names.

This function is based on the procedure source (Guile 1.8) or
introspection (Guile 2.2); it may not alway work. It prints '?' if it
fails..

Returns the arguments names are found, else #f."
  (let ((lambda-list (proc-lambda-list proc)))
    (cond
     ((not lambda-list)
      (error "not found!" proc)
      (display " ?")
      #f)
     ((not (pair? lambda-list))
      ;; We can have (lambda args ...).
      (display " ")
      (display (cadr src))
      #f)
     (else
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

(define (display-documentation obj)
  "Display the documentation of OBJ if available.

Returns #t if the documentation was printed; else #f."
  (let ((doc (or (and (procedure? obj)
                      (or 
                        (procedure-documentation obj)
                        (procedure-property obj 'documentation)))
                 (object-property obj 'documentation))))
    (cond
     (doc
      (newline)
      (display (regexp-substitute/global #f "[@{}]" doc 'pre "@" 0 'post))
      (newline)
      #t)
     (else
      #f))))

(define-class-with-doc <module-map> ()
  "An set of maps from module content to unique ids."
  (object-unique-id #:init-thunk make-hash-table #:getter object-unique-id)
  (unique-ids #:init-thunk make-hash-table #:getter unique-ids)
  (object-is-public #:init-thunk make-hash-table #:getter object-is-public)
  (module-documentation #:init-value #f #:accessor module-documentation)
  (generics #:init-value '() #:accessor generics)
  (generic-is-partial #:init-thunk make-hash-table #:getter generic-is-partial)
  (classes #:init-value '() #:accessor classes)
  (procedures #:init-value '() #:accessor procedures)
  (macros #:init-value '() #:accessor macros))

(define-method (unique-id (mmap <module-map>) value)
  "Return the unique id of the object as a string or #f."
  (hashq-ref (object-unique-id mmap) value))

(define-method (set-unique-id! (mmap <module-map>) (symbol <symbol>) value)
  "Set if necessary a unique id for the the object."
  (or (hashq-ref (object-unique-id mmap) value)
      (let ((wanted-name
             (cond
              ((is-a? value <generic>)
               (symbol->string symbol))
              ((is-a? value <method>)
               (let* ((names
                       (list (or (hashq-ref (object-unique-id mmap)
                                            (method-generic-function value))
                                 (error "generic function not registered" symbol)))))
                 (for-each
                  (lambda (spec) (push! (symbol->string (class-name spec)) names))
                  (method-specializers value))
                 (string-join (reverse! names) " ")))
              (else
               (string-append (symbol->string symbol))))))
        (do ((name wanted-name (string-append wanted-name "<" (number->string i) ">"))
             (i 0 (1+ i))
             (selected-name #f))
            (selected-name selected-name)
          (if (not (hash-ref (unique-ids mmap) name))
              (begin
                (set! selected-name name)
                (hash-set! (unique-ids mmap) name #t)
                (hashq-set! (object-unique-id mmap) value name)))))))

(define-method (display-anchor (mmap <module-map>) value)
  "Display an @anchor using the VALUE unique-id."
  (let ((uuid (unique-id mmap value)))
    (if (not uuid) (error "can't find the uuid" value))
    (display "@anchor{") (display uuid) (display "}")))

(define-method (display-procedure (mmap <module-map>) (symbol <symbol>) proc)
  (if (not (procedure? proc))
      (error "expected a procedure" proc))
  (display-anchor mmap proc)
  (let ((name (symbol->string symbol)))
    (display "@deffn Procedure ")
    (if (string-index name #\space)
        (begin
          (display "{")
          (display name)
          (display "}"))
        (display name)))
  (display-arguments proc #f #f)
  (newline)
  (display-documentation proc)
  (display "@end deffn\n\n"))

(define-method (display-music-functions (mmap <module-map>) (symbol <symbol>) proc)
  (if (not (ly:music-function? proc))
      (error "expected a ly:music-function" proc))
  (display-anchor mmap proc)
  (let ((name (symbol->string symbol)))
    (display "@deffn {Music Function} ")
    (if (string-index name #\space)
        (begin
          (display "{")
          (display name)
          (display "}"))
        (display name)))
  (let* ((signature (ly:music-function-signature proc))
         (inner-function (ly:music-function-extract proc))
         (optional-params (map pair? (cdr signature)))
         (names (display-arguments inner-function #f optional-params)))
    (display " -> ")
    (display (procedure-name (let ((first (car signature)))
                               (cond
                                ((pair? first) (car first))
                                (else first)))))
    (newline)
    (if (not (null? signature))
        (begin
          (display "Signature:\n")
          (display "@indentedblock\n")
          (display "@table @code\n")
          (do ((params (cdr signature) (cdr params))
               (names names (and names (cdr names))))
              ((null? params))
            (let ((param (car params))
                  (name (and names (car names))))
              (display "@item ")
              (write name)
              (newline)
              (cond
               ((pair? param)
                (display "@code{")
                (write (procedure-name (car param)))
                (display "}")
                (display "; optional")
                (if (not (null? (cdr param)))
                    (begin 
                      (display ", default value: ")
                      (write (cdr param))))
                (newline))
               (else
                (display "@code{")
                (write (procedure-name param))
                (display "}")
                (newline)))))
          (display "@end table\n")
          (display "@end indentedblock\n")))
    (newline)
    (display-documentation inner-function))
  (display "@end deffn\n\n"))

(define-method (display-method (mmap <module-map>) (symbol <symbol>) (meth <method>))
  (display-anchor mmap meth)
  (display "@defgoopmethod{")
  (display (symbol->string symbol))
  (display ", ")
  (let ((proc (method-procedure meth)))
    (if proc
        (display-arguments proc
                           (method-specializers meth)
                           #f)
        (begin
          (let ((p (current-error-port)))
            (display "WARNING: procedure not found for method " p)
            (display meth p)
            (display " of generic function " p)
            (display (method-generic-function meth) p)
            (newline p))
          (display " ?"))))
  (display ",")
  (for-each
   (lambda (spec)
     (display " ")
     (write (class-name spec)))
   (method-specializers meth))
  (display "}")
  (newline)
  (if (display-documentation (method-procedure meth))
      (newline)
      (newline))
  (display "@enddefgoopmethod\n"))
  

(define-method (display-generic-function
                (mmap <module-map>) (symbol <symbol>) (generic <generic>))
  (display-anchor mmap generic)
  (let ((name (symbol->string symbol)))
    (display "@deffn {Generic Function} ")
    (if (string-index name #\space)
        (begin
          (display "{")
          (display name)
          (display "}"))
        (display name)))
  (newline)
  (let ((is-partial (hashq-ref (generic-is-partial mmap) generic)))
    (for-each
     (lambda (meth)
       (if (or (not is-partial)
               (unique-id mmap meth))
           (display-method mmap symbol meth)))
     (generic-function-methods generic)))
  (display "@end deffn\n\n"))

(define-method (display-classes (mmap <module-map>))
  (let* ((n (length (classes mmap)))
         (g (digraph n))
         (vertex->symbol (list->vector (map car (classes mmap))))
         (vertex->class (list->vector (map cdr (classes mmap))))
         (class->vertex (make-hash-table)))
    ;; Create an hash-table mapping a class to its vertex in the graph.
    (do ((v 0 (1+ v)))
        ((= v n))
      (hashq-set! class->vertex (vector-ref vertex->class v) v))
    ;; Add arcs defined by "super" relations.
    (do ((from 0 (1+ from)))
        ((= from n))
      (for-each
       (lambda (to-cl)
         (let ((to (hashq-ref class->vertex to-cl)))
           (if to
               (push-arc! g from to))))
       (class-direct-supers (vector-ref vertex->class from))))
    ;; Extract connected components.
    (let* ((ccs (connected-components g))
           ;; Topologically sort all components.
           (topo-sorted-ccs
            (map
             (lambda (cc)
               (let* ((sub-g (subgraph g cc))
                      (sorted-sub-vs (topological-sort sub-g)))
                 (map-vector (list->vector cc) sorted-sub-vs)))
             ccs))
           ;; Extract the singletons and group them.
           (singletons-group (map car (filter (lambda (cc) (= (length cc) 1))
                                              topo-sorted-ccs)))
           ;; Create a list of non singletons.
           (non-singletons (filter (lambda (cc) (> (length cc) 1))
                                   topo-sorted-ccs)))

      ;; Add one-subsection for each non-singleton connected
      ;; component, using top class as name.
      ;;
      ;; FIXME: this logic breaks if a the components has mutliple roots.
      (for-each
       (lambda (cc)
         (let ((label (symbol->string (vector-ref vertex->symbol (car cc)))))
         (display "@node Class ") (display label) (display " Hierarchy\n")
         (display "@subsection Class ") (display label) (display " Hierarchy\n")
         (newline) (newline))
         (for-each
          (lambda (v) (display-class mmap
                                     (vector-ref vertex->symbol v)
                                     (vector-ref vertex->class v)))
          cc))
       non-singletons)

      (display "@node Isolated Classes\n")
      (display "@subsection Isolated Classes\n\n")
      (for-each
       (lambda (v) (display-class mmap
                                  (vector-ref vertex->symbol v)
                                  (vector-ref vertex->class v)))
       singletons-group))))

(define-method (display-class (mmap <module-map>) (symbol <symbol>) class)
  (display-anchor mmap class)
  (display "@deftp Class ")
  (let ((name (symbol->string symbol)))
    (if (string-index name #\space)
        (begin
          (display "{")
          (display name)
          (display "}"))
        (display name)))
  (display " (")
  (display (string-join (map (lambda (cl) (symbol->string (class-name cl)))
                             (filter (lambda (cl) (not (eq? cl <object>)))
                                     (class-direct-supers class)))
                        " "))
  (display ")")
  (newline)

  (let ((settable-instance-slots
         (filter (lambda (slot) (and (eq? (slot-definition-allocation slot) #:instance)
                                     (slot-definition-init-keyword slot)))
                 (class-slots class))))
    (if settable-instance-slots
        (begin
          (display "Keywords for @code{make}:\n\n")
          (display "@indentedblock\n")
          (display "@table @code\n")
          (for-each
           (lambda (slot)
             (let* ((options (slot-definition-options slot))
                    (not-set (list 'unset))
                    (set? (lambda (v) (not (eq? v not-set))))
                    (init-value (get-keyword #:init-value options not-set))
                    (init-form (get-keyword #:init-form options not-set))
                    (init-thunk (get-keyword #:init-thunk options not-set))
                    (has-default (or (set? init-value) (set? init-form) (set? init-thunk))))
               (display "@item ")
               (write (slot-definition-init-keyword slot))
               (newline)
               (if (set? init-value)
                   (begin (display "default value: ") (write init-value) (newline)))))
           settable-instance-slots)
          (display "@end table\n")
          (display "@end indentedblock\n\n"))))
                 
  (display-documentation class)
  (newline)

  (let ((methods (class-direct-methods class)))
    (if (not (null? methods))
        (begin
          (display "Direct methods:\n\n")
          (display "@indentedblock\n")
          (display "@table @asis\n")
          (for-each
           (lambda (meth)
             (display "@item -- ")
             (let ((uuid (unique-id mmap meth)))
               (if uuid
                   (begin (display "@ref{") (display uuid) (display "} "))
                   (begin
                     ;; FIXME: display full method in this case.
                     (display (generic-function-name (method-generic-function meth)))
                     (for-each
                      (lambda (spec)
                        (display " ")
                        (write (class-name spec)))
                      (method-specializers meth))))
               (newline)))
           (class-direct-methods class))
          (display "@end table\n")
          (display "@end indentedblock\n"))))

  (display "@end deftp")
  (newline)
  (newline))

(define-method (display-texinfo (mmap <module-map>))
  "Display to the default port the mapped module as a Texinfo file."
  (let ()
    (define (wrap-fn-call fn)
      "Returns a lambda that takes a pair and call a method with MMAP
as first argument, then the car of the pair and finally its cdr."
      (lambda (p) (fn mmap (car p) (cdr p))))

    (if (module-documentation mmap)
        (begin
          (display (module-documentation mmap))
          (newline) (newline)))
    
    (display "@node Music Functions\n")
    (display "@section Music Functions\n")
    (for-each (wrap-fn-call display-music-functions)
              (filter (lambda (p) (ly:music-function? (cdr p))) (procedures mmap)))
    (display "@node Procedures\n")
    (display "@section Procedures\n")
    (for-each (wrap-fn-call display-procedure)
              (filter (lambda (p) (not (ly:music-function? (cdr p))))
                      (procedures mmap)))
    (display "@node Generic Functions\n")
    (display "@section Generic Functions\n")
    (for-each (wrap-fn-call display-generic-function) (generics mmap))
    (display "@node Classes\n")
    (display "@section Classes\n")
    (display-classes mmap)
  ;;  ((macro? value)
  ;;   (display "(")
  ;;   (display k)
  ;;   (let ((proc (macro-transformer value)))
  ;;     (if proc
  ;;         (display-arguments proc #f)
  ;;         (display " <can't get the macro-transformer>")))
  ;;   (display ") [macro]")
  ;;   (newline)
  ;;   (if (not (display-documentation (macro-transformer value)))
  ;;       (display-documentation value)))
  ))

(define (map-module module-name)
  "Return a new <module-map> for the given module."
  (let* ((mmap (make <module-map>))
         (mod (resolve-module module-name))
         (itf (resolve-interface module-name))
         (itf-obarray (module-obarray itf))
         (unbound (list 'unbound)))
    (define (unbound? v) (eq? v unbound))
    (define (car-symbol-less? a b)
      (string< (symbol->string (car a)) (symbol->string (car b))))

    (set! (module-documentation mmap) (object-property mod 'documentation))
    
    (hash-for-each
     (lambda (k var)
       (let ((value (if (variable-bound? var)
                        (variable-ref var)
                        unbound)))
         (define (register-object)
           ;; Store the public/private status.
           (hashq-set! (object-is-public mmap) value (hashq-get-handle itf-obarray k))
           ;; Declare a unique id.
           (set-unique-id! mmap k value))
         (cond
          ((eq? k '%module-public-interface))
          ((unbound? value)
           (error "unbound symbol" k))
          ((is-a? value <generic>)
           (register-object)
           (push! (cons k value) (generics mmap))
           (hashq-set! (generic-is-partial mmap) value #f)
           (for-each
            (lambda (meth)
              (set-unique-id! mmap k meth))
            (generic-function-methods value)))
          ((is-a? value <class>)
           (register-object)
           (push! (cons k value) (classes mmap)))
          ((procedure? value)
           (register-object)
           (push! (cons k value) (procedures mmap)))
          ((macro? value)
           (register-object)
           (push! (cons k value) (macros mmap)))
          (else
           (display "Ignore module ")
           (write module-name)
           (display " ")
           (write k)))))
     (module-obarray mod))
    ;; Register all methods attached to objects that are not methods
    ;; of generic functions from this module (like ‘equal?’,
    ;; ‘write’, …).
    (dolist (symbol/class (classes mmap))
      (dolist (meth (class-direct-methods (cdr symbol/class)))
        (let* ((generic (method-generic-function meth))
               (symbol (generic-function-name generic)))
          (if (not (unique-id mmap generic))
              (begin
                (set-unique-id! mmap symbol generic)
                (push! (cons symbol generic) (generics mmap))
                (hashq-set! (generic-is-partial mmap) generic #t)))
          (if (not (unique-id mmap meth))
              (begin
                (set-unique-id! mmap symbol meth))))))
    (for-each
     (lambda (access)
       (set! (access mmap) (sort! (access mmap) car-symbol-less?)))
     (list generics classes procedures macros))
    mmap))

(define (display-all port)
  (do ((line (read-line port 'concat) (read-line port 'concat)))
      ((eof-object? line))
    (display line)))

(define (write-texinfo directory)
  (let* ((module-name '(harmony-rules))
         (mmap (map-module module-name)))
    (with-output-to-file (string-append directory "/harmony-rules.texi")
      (lambda ()
        (call-with-input-file "generate-doc-header.texi" display-all)
        (display-texinfo mmap)
        (call-with-input-file "generate-doc-footer.texi" display-all)))))

