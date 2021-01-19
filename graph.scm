(define-module (graph)
  #:use-module (documentation)
  #:use-module ((tools)
                #:select (push! map-vector dolist))
  #:use-module (oop goops)
  #:export (<digraph>
            digraph
            subgraph
            size
            push-arc!
            connected-components
            topological-sort))

(define-class-with-doc <digraph> ()
  "Directed graph. Use @code{(digraph n)} to instantiate a new one."
  (arcs #:getter arcs #:init-keyword #:arcs))

(define-method (digraph (n <integer>))
  "Instantiate a @code{<digraph>} with @var{n} vertices."
  (make <digraph>
    #:arcs (make-vector n '())))

(define-method (subgraph (g <digraph>) (vertices <list>))
  "Return a new graph composed of the vertices in @var{vertices}.
The new vertex 0 corresponds to @code{(car vertices)},…. Duplicates
are not tested, two new vertices will be created. An error is raised
if some arcs in @var{g} points to vertices not in @var{vertices}."
  (let ((sub-g (digraph (length vertices)))
        (g->sub-g (make-vector (size g) #f)))
    ;; Initialize the map ‘g->sub-g’ that associate to each vertex in
    ;; ‘g’ the index of the vertex in ‘sub-g’; or have #f when it does
    ;; not exists.
    (do ((sub-v 0 (1+ sub-v))
         (vs vertices (cdr vs)))
        ((null? vs))
      (vector-set! g->sub-g (car vs) sub-v))
    ;; Add the arcs.
    (dolist (from vertices)
      (let ((sub-from (vector-ref g->sub-g from)))
        (dolist (to (vector-ref (arcs g) from))
          (push-arc!
           sub-g
           sub-from
           (or (vector-ref g->sub-g to)
               (error
                (with-output-to-string
                  (lambda ()
                    (display "arc ")
                    (write from)
                    (display " -> ")
                    (write to)
                    (display " don't map to the sub-graph")))))))))
    sub-g))

(define-method (size (g <digraph>))
  "Return the number of vertices in the graph."
  (vector-length (arcs g)))

(define-method (push-arc! (g <digraph>) (from <integer>) (to <integer>))
  "Add an arc between vertices @var{from} and @var{to}.

This function does not check for duplicates so the same arc can be
pushed multiple times."
  (vector-set! (arcs g) from
               (cons to (vector-ref (arcs g) from))))

(define-method (reverse-arcs (g <digraph>))
  "Return a vector of lists representing the reversed arcs of the graph."
  (do ((from 0 (1+ from))
       (reversed (make-vector (size g) '())))
      ((= from (size g)) reversed)
    (dolist (to (vector-ref (arcs g) from))
       (vector-set! reversed to
                    (cons from (vector-ref reversed to))))))

(define-method (opposite-arcs (g <digraph>))
  "Return a vector of lists representing all graph's initial arcs and all the reversed arcs.

It does not check for duplicates."
  (let ((n (size g))
        (reversed (reverse-arcs g)))
    (do ((v 0 (1+ v))
         (new-arcs (make-vector n)))
        ((= v n) new-arcs)
      (vector-set! new-arcs v (append (vector-ref (arcs g) v)
                                      (vector-ref reversed v))))))

(define-method (connected-components (g <digraph>))
  "Return the sorted connected components of the input graph.
It is returned a list of lists of vertex indices. This can be used
with @code{subgraph} to create subgraphs for components."
  (let* ((n (size g))
         (all-arcs (opposite-arcs g))
         (vertex-in-comp? (make-vector n #f))
         (components '()))
    (do ((origin 0 (1+ origin)))
        ((= origin n) (reverse! components))
      (let ((comp '()))
        ;; Traverse all nodes.
        (do ((stack (list origin)))
            ((null? stack))
          ;; FIXME: add (push-children!)
          (let* ((top (car stack)))
            (set! stack (cdr stack))
            (if (not (vector-ref vertex-in-comp? top))
                (begin
                  (vector-set! vertex-in-comp? top #t)
                  (push! top comp)
                  ;; Add all vertices reachable.
                  (dolist (to (vector-ref all-arcs top))
                    (push! to stack))))))
        (if (not (null? comp))
            (push! (sort! comp <) components))))))

(define-method (topological-sort (g <digraph>))
  "Return the stable topological sort of the graph. #f if there are loops."
  (let* ((n (size g))
         (reversed-arcs (reverse-arcs g))
         (in-degree (make-vector n 0)))
    (define (+in-degree! v inc)
      "Increment the ‘in-degree’ of the input V by INC."
      (vector-set! in-degree v (+ (vector-ref in-degree v) inc)))
    (define (zero-in-degree-vertices)
      "Return the list of vertices with zero in-degree."
      (do ((v 0 (1+ v))
           (zero-ins '()))
          ((= v n) (reverse! zero-ins))
        (if (= (vector-ref in-degree v) 0)
            (push! v zero-ins))))
    
    ;; Add 1 to in-degree to the target of the reversed arcs.
    (do ((from 0 (1+ from)))
        ((= from n))
      (dolist (v (vector-ref reversed-arcs from))
        (+in-degree! v 1)))

    (let ((sorted-vertices '()))
      ;; Do the topological sort by adding the vertices with zero
      ;; in-degree to the output, decremeting the in-degree of all the
      ;; other vertices they point.
      ;;
      ;; This maintains the invariant that a vertex is pushed to the
      ;; output iff all the vertices it points have already been
      ;; pushed.
      ;;
      ;; Not that we push vertices in layers so that we keep the order
      ;; of vertices that are in the same group of vertices that
      ;; reached 0 when we traversed all the previous vertices at 0.
      (let next-layer ((zero-ins (zero-in-degree-vertices)))
        (let ((next-zero-ins '()))
          (dolist (from zero-ins)
            (push! from sorted-vertices)
            (dolist (to (vector-ref reversed-arcs from))
              (+in-degree! to -1)
              (if (= (vector-ref in-degree to) 0)
                  (push! to next-zero-ins))))
          (if (not (null? next-zero-ins))
              (next-layer (sort! next-zero-ins <) ))))
      (if (not (= (length sorted-vertices) n))
          ;; If at the end of the previous algorithm not all vertices
          ;; has been pushed to the output, it means that there is a
          ;; loop.
          #f
          (reverse! sorted-vertices)))))

(define (test)
  (define g (digraph 8))

  (let ((arcs '((1 . 0)
                (2 . 1)
                (3 . 1)
                (4 . 1)
                (5 . 2)
                (5 . 3)
                (6 . 3)
                (7 . 4)
                (6 . 4)
                )))
    (dolist (p arcs)
      (push-arc! g (car p) (cdr p))))


  (let* ((c-comps (connected-components g)))
    (define (sub-graph->graph sub-g->g sub-g-vertices)
      "Convert vertices from in a sub-graph to the initial vertices.
@var{sub-g->g} is the list of the graph's vertices that was used to
build the sub-graph. @var{sub-g-vertices} is the list of sub-graph
vertices to convert."
      (map-vector (list->vector sub-g->g) sub-g-vertices))
    (display "connected components: ")
    (write c-comps)
    (newline)
    (dolist (cc c-comps)
      (let ((sub-g (subgraph g cc)))
        (display "topo sort: ")
        (write (sub-graph->graph cc (topological-sort sub-g)))
        (newline)))))

;; (test)
