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

(define-module (docgen module-map)
  #:export (<module-map> unique-id map-module
                         object-unique-id unique-ids object-is-public
                         module-documentation generics generic-is-partial
                         classes procedures macros)
  #:use-module (oop goops)
  #:use-module (documentation)
  #:use-module (tools)
  )

(define-class-with-doc <module-map> ()
  "An set of maps from module content to unique ids.

Attributes:

* ‘object-unique-id’: hash table object → string which maps every
object in the module to a unique identifying string. Prefer using
‘(unique-id map obj)’ to accessing ‘object-unique-id’ directly.

* ‘unique-ids’: hash table string → bool. Contains #t for objects in
’object-unique-id’. Use ‘hash-ref’ to use ‘equal?’.

* ‘object-is-public’: hash table object → bool. Contains #t for public
objects.

* ‘module-documentation’: module documentation string, set by
‘set-current-module-documentation!’.

* ‘generics’: alist symbol → generic function.

* ‘generic-is-partial’: hash table generic function → bool. #t if the
generic has methods in other modules. Use ‘hashq-ref’.

* ‘classes’: alist symbol → class.

* ‘procedures’: alist symbol → procedure.

* ‘macros’: alist symbol → macro.
"
  (object-unique-id #:init-thunk make-hash-table #:getter object-unique-id)
  (unique-ids #:init-thunk make-hash-table #:getter unique-ids)
  (object-is-public #:init-thunk make-hash-table #:getter object-is-public)
  (module-documentation #:init-value #f #:accessor module-documentation)
  (generics #:init-value '() #:accessor generics)
  (generic-is-partial #:init-thunk make-hash-table #:getter generic-is-partial)
  (classes #:init-value '() #:accessor classes)
  (procedures #:init-value '() #:accessor procedures)
  (macros #:init-value '() #:accessor macros))

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
           (hashq-set! (object-is-public mmap) value (not (not (hashq-get-handle itf-obarray k))))
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
  
