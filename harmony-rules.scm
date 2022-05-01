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

(define-module (harmony-rules)
  #:use-module (lily)
  #:use-module (documentation)
  #:use-module (tools)
  #:use-module (oop goops)
  #:use-module ((srfi srfi-1)
		#:select (fold every concatenate))
  #:use-module (ice-9 receive)
  #:use-module (ice-9 syncase)
  #:export (format-music
	    dump-all
	    <timed>
	    <timed-music>
	    <finite-timed-music>
	    active?
	    <infinite-timed-music>
	    <timed-note>
	    <timed-key>
	    <timed-figure>
	    <events>
	    finite-timed-musics/time
	    infinite-timed-musics/time
	    events!
	    events-split
	    <voices>
	    note-event
	    pitch
	    movement
	    movement-interval
	    interval
	    pitches
	    chord-from-notes
	    chords-from-notes
	    chord-from-figures
	    chords-from-figures
	    get-voices
	    <interval>
	    quality<?
	    compound?
	    simple
            compound
	    pitches->interval
	    semi-tones
	    ascending
	    figures?
	    normalize-pitch
	    <chord>
	    type
	    next-octave
	    previous-octave
	    music
	    bass
	    chord
	    key-pitches
	    <parallel-consonance>
	    parallel-consonance
	    <augmented-second>
	    end-moment
	    moment
	    notes
	    figures
	    keys
	    bars
	    voice-count
	    moments
	    number
	    quality
	    tonic
	    third
	    fifth
	    seventh
	    inversion
	    moment-index
	    voice1
	    voice2
	    interval
	    kind
	    voice
	    key-pitch-alist
	    major-pitch-alist
	    minor-pitch-alist
	    augmented-second
	    unique
	    write-short
	    write-long
	    checkHarmonyRules
	    automaticChords
	    splitVoices))

(set-current-module-documentation!
 "Module defining functions used to analyse harmony.

It also defines music functions to be used in Lilypond,
\\checkHarmonyRules, \\automaticChords, \\splitVoices.")

(define-method (write-short o)
  "Shortcut without port."
  (write-format o (current-output-port) #f))

(define-method (write-short o port)
  "Public API of @code{write-format}."
  (write-format o port #f))

(define-method (write-long o)
  "Shortcut without port."
  (write-format o (current-output-port) ""))

(define-method (write-long o port)
  "Public API of @code{write-format}."
  (write-format o port ""))

(define-method (write-format o port indent)
  "Default method calling (write)."
  (write o port))

;; Lilypond 2.23 does not define <Prob> as a public symbol.
(define <LyMusicClass> (class-of (ly:make-music 'x)))

(define-method (write-format (o <LyMusicClass>) port indent)
  "Display @code{ly:music} specifically.

All other types are printed using @code{write}."
  (cond
   ((ly:music? o)
    (let ((indent (and indent (string-append indent " "))))
      (display "(music " port)
      (display (ly:music-property o 'name) port)
      (for-each
       (lambda (p)
	 (if (not (memq (car p) '(origin)))
	     (begin
	       (if indent
		   (begin
		     (display "\n" port)
		     (display indent port))
		   (display " " port))
	       (display "'" port)
	       (display (car p) port)
	       (display " " port)
	       (write-format (cdr p) port indent))))
       (ly:music-mutable-properties o))
      (display ")" port)))
   (else (write o port))))

(define-method (write-format (o <pair>) port indent)
  "Specific formatting for @code{<pair>}."
  (let ((indent (and indent (string-append indent " "))))
    (display "(" port)
    (if indent
	(begin
	  (display "\n" port)
	  (display indent port)))
    (write-format (car o) port indent)
    (let loop ((head (cdr o)))
      (cond
       ((null? head))
       ((pair? head)
	(if indent
	    (begin
	      (display "\n" port)
	      (display indent port))
	    (display " " port))
	(write-format (car head) port indent)
	(loop (cdr head)))
       (else
	(display " . " port)
	(write-format head port indent))))
    (display ")" port)))

(define-method (write-format (v <vector>) port indent)
  "Specific formatting for @code{<vector>}."
  ;; FIXME: write a direct format for vectors.
  (display "#" port)
  (write-format (vector->list v) port indent))

(define-method (write-format (o <object>) port indent)
  "Specific formatting for @code{<object>}."
  (let ((indent (and indent (string-append indent " "))))
    (display "(" port)
    (display (class-name (class-of o)) port)
    (for-each
     (lambda (slot-def)
       (let ((slot (slot-definition-name slot-def)))
	 (if indent
	     (begin
	       (display "\n" port)
	       (display indent port))
	     (display " " port))
	 (display slot port)
	 (display " " port)
	 (if (slot-bound? o slot)
	     (write-format (slot-ref o slot) port indent)
	     (display "<unbound>" port))))
     (class-slots (class-of o)))
    (display ")" port)))

(define (format-music music . config)
  "Format input @var{music}.

The @var{config} parameter is parameters identified by symbols
followed by their value. For example @code{(format-music m 'a 3 'b 2)}
will specify parameter 'a with value 3 and parameter 'b with value 2.

The supported parameters are:
- 'initial-indent: a string used as base indentation. By default an
  empty string is used.
- 'hidden-properties: a list of symbols representing the music
  properties that shoul be hidden.
- 'one-line: #t to use a one-line format, #f to use multi-line
  format. By default #f."
  (let ((hidden-properties
         '(elements
           element
           articulations
           name
           origin
           iterator-ctor
           length-callback
           to-relative-callback
           start-callback
           elements-callback))
        (initial-indent "")
        (one-line #f)
        (strings '()))
    ;; Parse CONFIG.
    (if (not (list? config))
        (error "unexpected CONFIG, should be a list"))
    (let ((n (length config))
          (initial-config config))
      (if (not (even? n))
          (error (format "list CONFIG length is not even: ~a" n)))
      (do ((head initial-config (cddr head)))
          ((null? head))
        (let ((name (car head))
              (value (cadr head)))
          (if (not (symbol? name))
              (error "list CONFIG must be like '(symbol value symbol value...)"))
          (case name
            ((initial-indent) (set! initial-indent value))
            ((hidden-properties) (set! hidden-properties (append hidden-properties value)))
            ((one-line) (set! one-line value))
            (else (error (format "unexpected property ~s in CONFIG" name)))))))
    ;; Do stuff.
    (let rec ((music music)
              (indent initial-indent))
      (let ((n (ly:music-property music 'name))
            (e (ly:music-property music 'element))
            (es (ly:music-property music 'elements))
            (as (ly:music-property music 'articulations)))
        (if one-line
            (push! (format "[~a" n) strings)
            (push! (format "~a* ~a\n" indent n) strings))
        (for-each
         (lambda (p)
           (let ((v (ly:music-property music p)))
             (if (and (not (null? v))
                      (not (memq p hidden-properties)))
                 (if one-line
                     (push! (format " ~a:~a" (symbol->string p) v) strings)
                     (push! (format "~a  ~a: ~a\n" indent (symbol->string p) v)
                            strings)))))
         all-music-properties)
        (if (not (null? es))
            (begin
              (if one-line
                  (push! (format " elements: [" indent) strings)
                  (push! (format "~a  elements:\n" indent) strings))
              (for-each (lambda (m) (rec m (string-append indent "   "))) es)
              (if one-line
                  (push! "]" strings))))
        (if (not (null? as))
            (begin
              (if one-line
                  (push! (format " articulations: [") strings)
                  (push! (format "~a  articulations:\n" indent) strings))
              (for-each (lambda (m) (rec m (string-append indent "   "))) as)
              (if one-line
                  (push! "]" strings))))
        (if (not (null? e))
            (begin
              (if one-line
                  (push! " element: " strings)
                  (push! (format "~a  element:\n" indent) strings))
              (rec e (string-append indent "   "))
              (if one-line
                  (push! "]" strings))))
        (if one-line
            (push! "]" strings))))
    (string-join (reverse strings) "")))

(define (dump-all music)
  (display (format-music music)))

(define-public (best-duration-of-length moment)
  "Return a @code{ly:duration} from the input length as @code{ly:moment}.

Contrary to Lilypond's @code{make-duration-of-length} that scales a
whole note, this function tries to find the best note that fits the
input length."
  (if (not (= (ly:moment-grace moment) 0))
      (error "this function does not support grace notes"))
  (let ((moment-frac (ly:moment-main moment)))
    (define (log2 v) (/ (log v) (log 2)))
    (cond
     ((< moment-frac 0) (error "negative moment"))
     ((= moment-frac 0) (error "moment is zero"))
     (else
      (let* ((l (inexact->exact (- (floor (log2 moment-frac)))))
	     (d (inexact->exact
		 (floor
		  (- (+ 1
			(log2 (- 1 (/ moment-frac
				      (* 2 (expt 2 (- l)))))))))))
	     (dur (ly:make-duration l d)))
	(if (not (equal? (ly:duration-length dur)
			 moment))
	    ;; By default (for example with tuplets), we return the
	    ;; basic duration.
	    (make-duration-of-length moment)
	    dur))))))

(define-class-with-doc <timed> ()
  "Some event that has a time specified by a @code{ly:moment}."
  (moment #:init-value (ly:make-moment 0)
	  #:getter moment
	  #:init-keyword #:moment))

(define-method (equal? (a <timed>) (b <timed>))
  (equal? (moment a) (moment b)))

(define-method (write (e <timed>) port)
  (display "#<" port)
  (display (class-name (class-of e)) port)
  (display " " port)
  (write (moment e) port)
  (display ">" port))

(define-class-with-doc <timed-music> (<timed>)
  "A @code{ly:music} happening at a given @code{ly:moment}."
  (music #:getter music
	 #:init-keyword #:music))

(define-method (equal? (a <timed-music>) (b <timed-music>))
  (and (equal? (music a) (music b))
       (next-method)))

(define-method (write (e <timed-music>) port)
  (display "#<" port)
  (display (class-name (class-of e)) port)
  (display " " port)
  (write (moment e) port)
  (display " (make-music '" port)
  (display (symbol->string
	    (ly:music-property (music e) 'name))
	   port)
  (for-each
   (lambda (prop)
     (display " '" port)
     (display (symbol->string (car prop)) port)
     (display " " port)
     (write (cdr prop) port))
   (ly:music-mutable-properties (music e)))
  (display ")>" port))

(define-class-with-doc <finite-timed-music> (<timed-music>)
  "A @code{<timed-music>} for music that have a 'duration.")

(define-method (duration (ftm <finite-timed-music>))
  "Returns the @code{'duration} of the @code{ly:music}."
  (if (eq? (ly:music-property (music ftm) 'name) 'EventChord)
      ;; For chords we use the duration of the first element, if present.
      (if (not (null? (ly:music-property (music ftm) 'elements)))
	  (ly:music-property (car (ly:music-property (music ftm) 'elements)) 'duration)
	  (ly:make-duration 0 0 0 1))
      (ly:music-property (music ftm) 'duration)))

(define-method (end-moment (ftm <finite-timed-music>))
  "Returns the end @code{ly:moment} of this finite event (not included)."
  (ly:moment-add
   (moment ftm)
   (ly:duration-length (duration ftm))))

(define-method (active? (ftm <finite-timed-music>) m)
  "Returns #t if input @code{ly:moment} @var{m} is in [start, end) of FTM.

Here start is FTM's moment and end is this moment shifted by its
duration."
  (and (not (ly:moment<? m (moment ftm)))
       (ly:moment<? m (end-moment ftm))))

(define-class-with-doc <infinite-timed-music> (<timed-music>)
  "A @code{<timed-music>} for music that has an \"infinite\" duration
(i.e. music that is active until replaced by another music).")

(define-method (active? (itm <infinite-timed-music>) m)
  "Returns @code{#t} if input @code{ly:moment} M is greater of equal
to the moment fo ITM."
  (not (ly:moment<? m (moment itm))))

(define-class-with-doc <timed-note> (<finite-timed-music>)
  "Slot @code{'music} contains @code{'NoteEvent}.")

(define-class-with-doc <timed-key> (<infinite-timed-music>)
  "Slot @code{'music} contains @code{'KeyChangeEvent}.")

(define-class-with-doc <timed-figure> (<finite-timed-music>)
  "Slot @code{'music} contains @code{'EventChord} with @code{BassFigureEvent} elements.")

(define-class <events> ()
  ;; A list of <timed-note>.
  (notes #:init-value '() #:getter notes #:init-keyword #:notes)
  ;; A list of <timed-key>.
  (keys #:init-value '() #:getter keys #:init-keyword #:keys)
  ;; A list of <timed-figure>.
  (figures #:init-value '() #:getter figures #:init-keyword #:figures)
  ;; A list of <ly:moment> for specific bars.
  (bars #:init-value '() #:getter bars #:init-keyword #:bars))

(define-method (equal? (a <events>) (b <events>))
  (and (equal? (notes a) (notes b))
       (equal? (keys a) (keys b))
       (equal? (figures a) (figures b))
       (equal? (bars a) (bars b))))

(define-method (write (o <events>) port)
  (display "#<" port)
  (display (class-name (class-of o)) port)
  (display " #:notes (" port)
  (for-each
   (lambda (e)
     (display "\n  " port)
     (display e port))
   (notes o))
  (display ")" port)
  (display " #:keys (" port)
  (for-each
   (lambda (e)
     (display "\n  " port)
     (display e port))
   (keys o))
  (display ")" port)
  (display " #:figures (" port)
  (for-each
   (lambda (e)
     (display "\n  " port)
     (display e port))
   (figures o))
  (display ")" port)
  (display " #:bars " port)
  (display (bars o) port)
  (display ">" port))

(define (expand-repeats! music)
  "Expand pitchless repeated notes and chords.

This function modifies the input music."
  (expand-repeat-notes!
   (expand-repeat-chords!
    (cons 'rhythmic-event
	  (ly:parser-lookup '$chord-repeat-events))
    music)))

(define (events! music)
  "Returns a <events> record.

The input music is modified to expand repeats."
  (let ((notes '())
        (keys '())
	(figures '())
        (bars '()))
    (let rec ((music (expand-repeats! music))
              (start-t (ly:make-moment 0)))
      (let ((elts (ly:music-property music 'elements))
            (elt (ly:music-property music 'element))
            (simultaneous (music-is-of-type? music 'simultaneous-music))
            (name (ly:music-property music 'name)))
        (cond
         ((eq? name 'NoteEvent)
          (push! (make <timed-note> #:moment start-t #:music music)
                 notes)
          (ly:moment-add
           start-t
           (ly:duration-length (ly:music-property music 'duration))))
	 ((figures? music)
	  (push! (make <timed-figure> #:moment start-t #:music music)
                 figures)
	  (ly:moment-add
	   start-t
	   ;; Use the duration of the first BassFigureEvent.
	   (ly:duration-length (ly:music-property
				(car (ly:music-property music 'elements))
				'duration))))
         ((eq? name 'KeyChangeEvent)
          (push! (make <timed-key> #:moment start-t #:music music)
                 keys)
          start-t)
         ((or
           ;; Lilypond 2.23 uses 'BarEvent with 'bar-type.
           (eq? name 'BarEvent)
           ;; Lilypond <= 2.22 uses 'ContextSpeccedMusic of type
           ;; 'Timing with property 'whichBar.
           (and (eq? name 'ContextSpeccedMusic)
                (eq? (ly:music-property music 'context-type) 'Timing)
                (eq? (ly:music-property elt 'name) 'PropertySet)
                (eq? (ly:music-property elt 'symbol) 'whichBar)))
          (push! start-t bars)
          start-t)
         ((music-is-of-type? music 'rhythmic-event)
          ;; Here we cover all other rhythmic events, including
          ;; SkipEvent and RestEvent.
          (ly:moment-add
           start-t
           (ly:duration-length (ly:music-property music 'duration))))
         ((eq? name 'UnfoldedRepeatedMusic)
          (do ((i 0 (+ i 1)))
              ((= i (ly:music-property music 'repeat-count)) start-t)
            (set! start-t (rec elt start-t))))
         ((not (null? elts))
          (let ((new-start-t start-t))
            (while (not (null? elts))
                   (let ((end-t (rec (car elts) start-t)))
                     (if simultaneous
                         (if (ly:moment<? new-start-t end-t)
                             (set! new-start-t end-t))
                         (begin
                           (set! start-t end-t)
                           (set! new-start-t end-t))))
                   (set! elts (cdr elts)))
            new-start-t))
         ((not (null? elt))
          (rec elt start-t))
         (else start-t))))
    (make <events>
     #:notes (reverse notes)
     #:keys (reverse keys)
     #:figures (reverse figures)
     #:bars
     (do ((head (sort bars (lambda (a b) (ly:moment<? a b))) (cdr head))
                        (unique '())
                        (last #f (car head)))
                       ((null? head) (reverse unique))
                     (if (not (equal? (car head) last))
                         (push! (car head) unique))))))

(define-method (events-split (events <events>))
  "Splits the input <events> at bars and returns a list of new
<events>."
  (let ((sorted-notes
         (sort (notes events)
               (lambda (a b)
                 (ly:moment<? (moment a) (moment b)))))
	(sorted-keys
	 (sort (keys events)
	       (lambda (a b)
		 (ly:moment<? (moment a) (moment b)))))
	(sorted-figures
         (sort (figures events)
               (lambda (a b)
                 (ly:moment<? (moment a) (moment b)))))
        (sorted-bars
         (sort (bars events) ly:moment<?)))
    (define (end start t)
      "Find the first <timed> that has a moment >= t."
      (do ((head start (cdr head)))
	  ((or (null? head)
	       (and (not (null? t))
		    (not (ly:moment<? (moment (car head))
				      (car t)))))
	   head)))
    (define (slice start end)
      (do ((head start (cdr head))
	   (ret '()))
	  ((eq? head end)
	   (reverse ret))
	(push! (car head) ret)))
    (do ((notes-start sorted-notes notes-end)
         (notes-end '())
	 (figures-start sorted-figures figures-end)
         (figures-end '())
	 (keys-first sorted-keys keys-next)
	 (keys-next '())
         (splits '())
         (t sorted-bars (if (null? t) '() (cdr t))))
        ((null? notes-start) (reverse splits))
      ;; Find the first note that has a moment >= t.
      (set! notes-end (end notes-start t))
      ;; Find the first note that has a moment >= t.
      (set! figures-end (end figures-start t))
      ;; Find the last key that has a moment <= t.
      (set! keys-next
	    (do ((head keys-first (cdr head))
		 (last '() head))
		((or (null? head)
		     (and (not (null? t))
			  (ly:moment<? (car t)
				       (moment (car head)))))
		 ;; Here head point either to '() or to the the first
		 ;; event with a moment > t. Hence last should point
		 ;; to the last event with a moment <= t.
		 last)))
      ;; Create a split.
      (let ((notes (slice notes-start notes-end))
	    (figures (slice figures-start figures-end))
	    (keys (do ((head keys-first (cdr head))
		       (keys '()))
		      ((or (null? head)
			   (and (not (null? t))
				(not (ly:moment<? (moment (car head))
						  (car t)))))
		       (reverse keys))
		    (push! (car head) keys))))
	(push! (make <events> #:notes notes #:figures figures #:keys keys)
               splits)))))

(define (finite-timed-musics/time finite-timed-musics moments)
  "Returns a vector of the same length as MOMENTS that contains lists
of the <finite-timed-music> events in FINITE-TIMED-MUSICS that are
active at each moment.

The MOMENTS must be sorted and without duplicates."
  (if (or (not (sorted? moments ly:moment<?))
	  (do ((head moments (cdr head))
	       (last #f (car head)))
	      ((or (null? head)
		   (equal? (car head) last))
	       ;; We have a duplicate if we did not stop at end.
	       (not (null? head)))))
      (error "input MOMENTS must be sorted and without duplicates"))
  (let ((v (make-vector (length moments))))
    (do ((i 0 (+ i 1))
	 (head moments (cdr head)))
	((null? head) v)
      ;; TODO: this could be optimized by creating start and stop
      ;; events for each <finite-time-music>.
      (vector-set!
       v i
       (filter (lambda (ftm) (active? ftm (car head)))
	       finite-timed-musics)))))

(define (last-moment<= itms m)
  "Find the last items whose moment is less or equal than M. It
returns (values FOUND TAIL). FOUND is the found item, #f if it does
not exist (all elements are greater than M). TAIL is the tail of the
list to consider for the next search."
  (do ((head itms (cdr head))
       (last '() head))
      ((or (null? head) (ly:moment<? m (moment (car head))))
       (if (null? last)
	   (values #f head)
	   (values (car last) last)))))

(define (infinite-timed-musics/time infinite-timed-musics moments)
  "Returns a vector of the same length as MOMENTS that contains the
latest <infinite-timed-music> event in INFINITE-TIMED-MUSICS that is
active at each moment.

The MOMENTS must be sorted and without duplicates."
  (if (or (not (sorted? moments ly:moment<?))
	  (do ((head moments (cdr head))
	       (last #f (car head)))
	      ((or (null? head)
		   (equal? (car head) last))
	       ;; We have a duplicate if we did not stop at end.
	       (not (null? head)))))
      (error "input MOMENTS must be sorted and without duplicates"))
  (let ((sorted (sort infinite-timed-musics
		      (lambda (a b) (ly:moment<? (moment a) (moment b)))))
	(v (make-vector (length moments))))
    (let loop ((i 0)
	       (moments-head moments)
	       (sorted-head sorted))
      (if (null? moments-head)
	  v
	  (receive
	   (found sorted-next)
	   (last-moment<= sorted-head (car moments-head))
	   (vector-set! v i found)
	   (loop (+ i 1) (cdr moments-head) sorted-next))))))

(define-class <voices> ()
  ;; Number of voices.
  (voice-count #:init-value 0
	       #:getter voice-count
	       #:init-keyword #:voice-count)
  ;; A sorted verctor of ly:moment.
  (moments #:init-value (make-vector 0)
	   #:getter moments
	   #:init-keyword #:moments)
  ;; All <timed-note> in a packed vector. For voice index v and moment index t, the
  ;; corresponding note is at index `v + voice-count * t`.
  (notes #:init-value (make-vector 0)
	       #:getter notes
	       #:init-keyword #:notes)
  ;; All <timed-figure> in a vector. Value #f is used to indicate no
  ;; figure exists at a given moment.
  (figures #:init-value (make-vector 0)
	   #:getter figures
	   #:init-keyword #:figures)
  ;; All <timed-key> in a vector. Value #f is used to indicate no key
  ;; exists at a given moment.
  (keys #:init-value (make-vector 0)
	#:getter keys
	#:init-keyword #:keys))

(define-method (equal? (a <voices>) (b <voices>))
  (and (equal? (voice-count a) (voice-count b))
       (equal? (moments a) (moments b))
       (equal? (notes a) (notes b))
       (equal? (figures a) (figures b))
       (equal? (keys a) (keys b))))

(define-method (write (o <voices>) port)
  (display "#<" port)
  (display (class-name (class-of o)) port)
  (display " #:voice-count " port)
  (display (voice-count o) port)
  (display " #:moments " port)
  (write (moments o) port)
  (display " #:notes " port)
  (display (notes o) port)
  (display " #:figures " port)
  (display (figures o) port)
  (display " #:keys " port)
  (display (keys o) port)
  (display ">" port))

(define-method (note (voices <voices>) (voice <integer>) (moment-index <integer>))
  "Return the <timed-note> of the voice at index VOICE and at the
moment indentified by MOMENT-INDEX. #f if the voice is silent."
  (vector-ref (notes voices)
	      (+ voice (* (voice-count voices)
			  moment-index))))

;; FIXME: support #f.
(define-method (note-event (voices <voices>) (voice <integer>) (moment-index <integer>))
  "Return the NoteEvent of the voice at index VOICE and at the moment
indentified by MOMENT-INDEX."
  (music (note voices voice moment-index)))

;; FIXME: support #f.
(define-method (pitch (voices <voices>) (voice <integer>) (moment-index <integer>))
  "Return the 'pitch of the NoteEvent of the VOICE and at the MOMENT-INDEX."
  (ly:music-property (note-event voices voice moment-index) 'pitch))

;; FIXME: support #f.
(define-method (figure (voices <voices>) (moment-index <integer>))
  "Return the <timed-figure> at MOMENT-INDEX. Returns #f if there is no such figure."
  (vector-ref (figures voices) moment-index))

(define-method (key (voices <voices>) (moment-index <integer>))
  "Return the <timed-key> at MOMENT-INDEX. Returns #f if there is no such key."
  (vector-ref (keys voices) moment-index))

;; FIXME: support #f returning 'undefined.
(define-method (movement (voices <voices>) (voice <integer>) (moment-index <integer>))
  "Return the direction of movement of the voice at index VOICE
between moment MOMENT-INDEX - 1 and MOMENT-INDEX.

The returned value is a symbol, either 'none, 'up or 'down."
  (if (= moment-index 0)
      'none
      (let ((p1 (pitch voices voice (- moment-index 1)))
            (p2 (pitch voices voice moment-index)))
        (cond
         ((ly:pitch<? p1 p2) 'up)
         ((ly:pitch<? p2 p1) 'down)
         (else 'none)))))

;; FIXME: support #f.
(define-method (movement-interval (voices <voices>) (voice <integer>) (moment-index <integer>))
  "Return the interval of movement of the voice VOICE between moment
MOMENT-INDEX - 1 and MOMENT-INDEX.

Returns unisson if the MOMENT-INDEX is 0."
  (if (= moment-index 0)
      (make <interval> #:number 1 #:quality 'perfect)
      (pitches->interval
       (pitch voices voice (- moment-index 1))
       (pitch voices voice moment-index))))

;; FIXME: support #f.
(define-method (interval (voices <voices>) (v1 <integer>) (v2 <integer>) (moment-index <integer>))
  "Return the interval between voices V1 and V2 at given MOMENT-INDEX."
  (pitches->interval
   (pitch voices v1 moment-index)
   (pitch voices v2 moment-index)))

(define-method (pitches (voices <voices>) (moment-index <integer>))
  "Returns the list of pitches of all voices at given MOMENT-INDEX in order of voices."
  (let loop ((v (voice-count voices))
	     (ret '()))
    (if (= v 0)
	ret
	(let ((n (note voices (- v 1) moment-index)))
	  (if n
	      (loop (- v 1)
		    (cons (pitch voices (- v 1) moment-index)
			  ret))
	      (loop (- v 1) ret))))))

;; FIXME: add key changes
;; FIXME: add time signatures
(define-method (music (voices <voices>) (voice <integer>))
  "Returns a sequential music with all notes of the given VOICE."
  (do ((t 0 (+ t 1))
       (events (if (and (> (vector-length (moments voices)) 0)
			(ly:moment<? (ly:make-moment 0) (vector-ref (moments voices) 0)))
		   (list (make-music
			  'SkipEvent
			  'duration (best-duration-of-length
				     (vector-ref (moments voices) 0))))
		   '())))
      ((= t (vector-length (moments voices)))
       (make-sequential-music (reverse events)))
    (let ((duration (best-duration-of-length
		     (if (< t (- (vector-length (moments voices)) 1))
			 (ly:moment-sub (vector-ref (moments voices) (+ t 1))
					(vector-ref (moments voices) t))
			 ;; FIXME: need to store durations.
			 (ly:make-moment 1/4)))))
      (push! (if (note voices voice t)
                (make-music 'NoteEvent
                            'duration duration
                            'pitch (pitch voices voice t))
                (make-music 'SkipEvent 'duration duration))
             events))))

(define-method (chords-from (voices <voices>) chord-from)
  "Returns a sequential music suitable to be displayed in a ChordNames context.

CHORD-FROM is a function that takes ((voices <voices>) (moment-index
<integer>)) parameters."
  (do ((t 0 (+ t 1))
       (event-chords
	(if (and (> (vector-length (moments voices)) 0)
		 (ly:moment<? (ly:make-moment 0) (vector-ref (moments voices) 0)))
	    ;; Initialize with a skip if the voice does not start at 0.
	    (list (make-music
		   'SkipEvent
		   'duration (best-duration-of-length
			      (vector-ref (moments voices) 0))))
	    '())))
      ((= t (vector-length (moments voices)))
       (make-sequential-music (reverse event-chords)))
    (let ((chord (chord-from voices t))
	  (duration (best-duration-of-length
		     (if (< t (- (vector-length (moments voices)) 1))
			 (ly:moment-sub (vector-ref (moments voices) (+ t 1))
					(vector-ref (moments voices) t))
			 ;; FIXME: need to store durations.
			 (ly:make-moment 1/4)))))
      (push! (if chord
                 (music chord duration)
                 (make-music 'SkipEvent 'duration duration))
             event-chords))))

(define-method (chords-from-notes (voices <voices>))
  "Returns a sequential music suitable to be displayed in a ChordNames context."
  (chords-from voices chord-from-notes))

(define-method (chords-from-figures (voices <voices>))
  "Returns a sequential music suitable to be displayed in a ChordNames context."
  (chords-from voices chord-from-figures))

(define-method (chord-from-notes (voices <voices>) (moment-index <integer>))
  "Returns the <chord> computed from the notes of VOICES at given MOMENT-INDEX."
  (chord (pitches voices moment-index)))

(define-method (chord-from-figures (voices <voices>) (moment-index <integer>))
  "Returns the <chord> computed from the figures and key of VOICES at
given MOMENT-INDEX.

Returns #f if no bass note exist at a given time."
  (and (note voices 0 moment-index)     ; Is there a bass note?
       (chord (let ((k (key voices moment-index)))
                (if k
                    (music k)
                    ;; Use C major if no key is given.
                    #{ \key c \major #}))
              (pitch voices 0 moment-index)
              (let ((fig (figure voices moment-index)))
                (and fig
                     (music fig))))))

;; FIXME: support missing voices (starting with less voices than at the end).
(define-method (get-voices (events <events>))
  "Return all voices of a given <events> as an <voices> record."
  (let* ((moments
	  (unique
	   ly:moment<?
	   (concatenate
	    (list
	     (map moment (notes events))
	     ;; FIXME: include end-moments to support end and silences.
	     ;; (map end-moment (notes events))
	     (map moment (figures events))
	     ;; (map end-moment (figures events))
	     ))))
	 (notes-per-moment (finite-timed-musics/time (notes events) moments))
	 (figures-per-moment (finite-timed-musics/time (figures events) moments))
	 (voice-count (let loop ((i 0) (count 0))
			(if (= i (vector-length notes-per-moment))
			    count
			    (loop (+ i 1) (max count
					       (length (vector-ref notes-per-moment i))))))))
    (define (notes)
      (do ((i 0 (+ i 1))
	   (moment-head moments (cdr moment-head))
	   ;; FIXME: replace #f with <rest> or <silence> to use
	   ;; generic functions for automatic behavior.
	   (ret (make-vector (* voice-count (length moments)) #f)))
	  ((null? moment-head) ret)
	(let ((sorted (sort (vector-ref notes-per-moment i)
			    (lambda (a b)
			      (ly:pitch<? (ly:music-property (music a) 'pitch)
					  (ly:music-property (music b) 'pitch))))))
	  (do ((v 0 (+ v 1))
	       (notes-head sorted (cdr notes-head)))
	      ((null? notes-head))
	    (vector-set! ret (+ v (* voice-count i)) (car notes-head))))))
    ;; FIXME: unit test this
    (define (figures)
      (do ((i 0 (+ i 1))
	   (moment-head moments (cdr moment-head))
	   (ret (make-vector (length moments) #f)))
	  ((null? moment-head) ret)
	(let ((figures (vector-ref figures-per-moment i)))
	  (if (> (length figures) 1)
	      (for-each (lambda (f)
			  (ly:input-warning (ly:music-property (music f) 'origin)
					    "multiple figures at this moment, we ignored this one"))
			figures))
	  (if (not (null? figures))
	      (vector-set! ret i (car figures))))))
    (make <voices>
      #:voice-count voice-count
      #:moments (list->vector moments)
      #:notes (notes)
      #:figures (figures)
      ;; FIXME: unit-test this
      #:keys (infinite-timed-musics/time (keys events) moments))))
    

(define-class <interval> ()
  ;; The "number" of letter names it encompasses. 1 for unisson, 8 for
  ;; octave. For compound intervals it contains the compound
  ;; number. For a ninth (c - d') it will be 9, even though the
  ;; corresponding simple interval number is 2.
  ;;
  ;; This number must be >= 1.
  (number #:init-value 1
	  #:getter number
	  #:init-keyword #:number)
  ;; The "quality": 'perfect, 'major, 'minor, 'augmented or 'diminished.
  ;;
  ;; Not all qualities are valid, depending on the interval number:
  ;; 
  ;;  - 'perfect can only be used for numbers (for all n >= 0):
  ;;    * 1 + 7n
  ;;    * 4 + 7n
  ;;    * 5 + 7n 
  ;;  
  ;;  - 'minor and 'major can only be used for numbers (for all n >= 0):
  ;;    * 2 + 7n
  ;;    * 3 + 7n
  ;;    * 6 + 7n
  ;;    * 7 + 7n
  ;;
  ;;  - augmented and diminished can be used for all numbers.
  (quality #:init-value 'perfect
	   #:getter quality
	   #:init-keyword #:quality))

(define (quality<? a b)
  "Compare qualities A and B as defined by (quality <interval>)."
  (define (quality->integer quality)
    (case quality
      ((diminished) 0)
      ((minor) 1)
      ((perfect) 2)
      ((major) 3)
      ((augmented) 4)
      (else (error (with-output-to-string
		     (display "unexpected QUALITY ")
		     (write quality))))))
  (< (quality->integer a) (quality->integer b)))

(define-method (< (a <interval>) (b <interval>))
  "Returns #t if interval A is smaller than B, taking into account the number first."
  (if (not (= (number a) (number b)))
      (< (number a) (number b))
      (quality<? (quality a) (quality b))))

(define-method (equal? (a <interval>) (b <interval>))
  (and (= (number a) (number b))
       (eq? (quality a) (quality b))))

(define-method (write (i <interval>) port)
  (display "#<" port)
  (display (class-name (class-of i)) port)
  (display " " port)
  (display (number i) port)
  (display " " port)
  (display (quality i) port)
  (display ">" port))

(define-method (compound? (itv <interval>))
  "Return #t if ITV is a compound interval."
  (>= (number itv) 9))

(define-method (simple (itv <interval>))
  "Return the simple interval corresponding to this interval.

If this interval is already simple then the input is returned. If this
interval is compound it returns a new interval."
  (if (compound? itv)
      (let ((n (+ 1 (modulo (- (number itv) 1) 7))))
        (make <interval>
         ;; We have a special case for the octave since we
         ;; differentiate an unisson and an octave as we defined the
         ;; first compound interval to be the ninth.
         #:number (if (= n 1) 8 n)
         #:quality (quality itv)))
      itv))

(define-method (compound (itv <interval>))
  "Return the first compound interval from this interval.

If this interval is already compound then the input is returned. It
this is a simple interval a new interval is returned.

The unisson and octave are considered both simple but the first
compound interval of the unisson is defined as the octave while the
first computed interval of the octave is the 15th."
  (if (not (compound? itv))
      (make <interval>
        #:number (+ (number itv) 7)
        #:quality (quality itv))
      itv))

(define (pitches->interval p1 p2)
  "Returns the interval between two pitches.

This function is symmetric, the result does not depend on the order of
the input pitches."
  ;; We always make sure p1 <= p2 lexicographically to simplify
  ;; computation below.
  (if (ly:pitch<? p2 p1)
      (let ((t p1))
        (set! p1 p2)
        (set! p2 t)))
  (let* ((o1 (ly:pitch-octave p1))
         (o2 (ly:pitch-octave p2))
         (n1 (ly:pitch-notename p1))
         (n2 (ly:pitch-notename p2))
         (a1 (ly:pitch-alteration p1))
         (a2 (ly:pitch-alteration p2))
         ;; When o1 == o2 we know that n1 <= n2 since p1 <= p2. But
         ;; with o1 < o2, we may have two new cases:
         ;; 
         ;; - n1 == n2: this is an octave (or two octaves...); the
         ;;   simple interval number is 7 + 1 = 8.
         ;;   
         ;; - n1 > n2: the simple interval number is 7 + n2 - n1.
         ;;
         ;; In those two cases we have added 7 to n2 - n1, i.e. we
         ;; have added an octave. We need to take that into account
         ;; when computing the compound number.
         (is-octave-counted (and (> o2 o1) (>= n1 n2)))
         (simple-number-minus-one
          (- (+ (if is-octave-counted 7 0) n2) n1))
         (notename-semitones #(0 2 4 5 7 9 11))
         (simple-semitones
          (- (+ (vector-ref notename-semitones n2) (* 2 a2)
                (if (and (> o2 o1) (<= n2 n1)) 12 0))
             (+ (vector-ref notename-semitones n1) (* 2 a1))))
         (qualities
          #(((0 . perfect) (1 . augmented)) ; unison
            ((0 . diminished) (1 . minor) (2 . major) (3 . augmented)) ; second
            ((2 . diminished) (3 . minor) (4 . major) (5 . augmented)) ; third
            ((4 . diminished) (5 . perfect) (6 . augmented)) ; fourth
            ((6 . diminished) (7 . perfect) (8 . augmented)) ; fifth
            ((7 . diminished) (8 . minor) (9 . major) (10 . augmented)) ; sixth
            ((9 . diminished) (10 . minor) (11 . major) (12 . augmented)) ; seventh
            ((11 . diminished) (12 . perfect) (13 . augmented)))) ; octave
         (quality (assq simple-semitones (vector-ref qualities simple-number-minus-one))))
    (if (not quality)
        (error (format "Unsupported interval between ~s and ~s is ~a with ~a semi-tones"
                       p1 p2 (+ 1 simple-number-minus-one) simple-semitones)))
    (make <interval>
      #:number (+ 1 (* 7 (- o2 o1 (if is-octave-counted 1 0))) simple-number-minus-one)
      #:quality (cdr quality))))

(define-method (semi-tones (itv <interval>))
  "Returns the number of semi-tones represented by this interval."
  (let* ((num (- (number itv) 1))
	 (mod (modulo num 7))
	 (octaves 
	  ;; Here we don't use (quotient) since the
	  ;; result is not compatible with
	  ;; (modulo). Instead we use (/) but we remove
	  ;; the modulo first.
	  (/ (- num mod) 7))
	 (base (vector-ref #(0 2 4 5 7 9 11) mod)))
    (case (+ 1 mod)
      ((1 4 5)
       (+ base
	  (assq-ref '((augmented . 1) 
		      (perfect . 0)
		      (diminished . -1))
		    (quality itv))
	  (* octaves 12)))
      ((2 3 6 7)
       (+ base
	  (assq-ref '((augmented . 1)
		      (major . 0)
		      (minor . -1)
		      (diminished . -2))
		    (quality itv))
	  (* octaves 12)))
      (else (error "internal error")))))


(define-method (ascending (itv <interval>) pitch)
  "Returns the pitch corresponding to the ascending intervale ITV from PITCH."
  (let* ((base-pitch (ly:make-pitch
		      (ly:pitch-octave pitch)
		      ;; Here we don't use (modulo) since
		      ;; (ly:make-pitch) already does a normalization.
		      (+ (ly:pitch-notename pitch)
			 (- (number itv) 1))
		      ;; We begin without any alteration.
		      0))
	 (alteration (- (semi-tones itv)
			(- (ly:pitch-semitones base-pitch)
			   (ly:pitch-semitones pitch)))))
    (ly:make-pitch (ly:pitch-octave base-pitch)
		   (ly:pitch-notename base-pitch)
		   (/ alteration 2))))

(define (figures? figures)
  "Returns #t if the input FIGURES is an EventChord with BassFigureEvent elements."
  (and (eq? (ly:music-property figures 'name) 'EventChord)
       (not (null? (ly:music-property figures 'elements)))
       (every (lambda (m) (eq? (ly:music-property m 'name) 'BassFigureEvent))
	      (ly:music-property figures 'elements))))

(define (normalize-pitch pitch)
  "Returned the normalization of input PITCH, i.e. the transpotion in the -1 octave."
  (ly:make-pitch -1 (ly:pitch-notename pitch) (ly:pitch-alteration pitch)))

(define-class <chord> ()
  ;; A normalized ly:pitch.
  (tonic #:getter tonic #:init-keyword #:tonic)
  ;; An <interval> or #f.
  (third #:init-value #f #:getter third #:init-keyword #:third)
  ;; An <interval> or #f.
  (fifth #:init-value #f #:getter fifth #:init-keyword #:fifth)
  ;; An <interval> or #f.
  (seventh #:init-value #f #:getter seventh #:init-keyword #:seventh)
  ;; An <interval> or #f.
  (ninth #:init-value #f #:getter ninth #:init-keyword #:ninth)
  ;; 'root, 'first, 'second, 'third
  (inversion #:getter inversion #:init-keyword #:inversion))

(define-method (type (c <chord>))
  "Returns the type of chord.

Types are:
  - three notes chords:
    'major, 'minor, 'augmented, 'diminished
  - four notes chords:
    'dominant-seventh, 'major-seventh, 'minor-seventh,
    'minor-major-seventh, 'diminished-seventh, 'augmented-seventh,
    'half-diminished-seventh
  - five notes chords:
    'major-ninth, 'minor-ninth
  - else #f"
  (cond
   ((or (not (third c))
	(not (fifth c)))
    #f)
   ((not (seventh c))
    (let ((qualities (cons (quality (third c)) (quality (fifth c)))))
      (cond
       ((equal? qualities '(major . perfect)) 'major)
       ((equal? qualities '(minor . perfect)) 'minor)
       ((equal? qualities '(minor . diminished)) 'diminished)
       ((equal? qualities '(major . augmented)) 'augmented)
       (else #f))))
   ((not (ninth c))
    (let ((qualities (list (quality (third c))
			   (quality (fifth c))
			   (quality (seventh c)))))
      (cond
       ((equal? qualities '(major perfect minor)) 'dominant-seventh)
       ((equal? qualities '(major perfect major)) 'major-seventh)
       ((equal? qualities '(major augmented augmented)) 'augmented-seventh)
       ((equal? qualities '(minor diminished diminished)) 'diminished-seventh)
       ((equal? qualities '(minor diminished minor)) 'half-diminished-seventh)
       ((equal? qualities '(minor perfect minor)) 'minor-seventh)
       ((equal? qualities '(minor perfect major)) 'minor-major-seventh)
       (else #f))))
   (else
    (let ((qualities (list (quality (third c))
                           (quality (fifth c))
                           (quality (seventh c))
                           (quality (ninth c)))))
      (cond
       ((equal? qualities '(major perfect minor major)) 'major-ninth)
       ((equal? qualities '(major perfect minor minor)) 'minor-ninth)
       (else #f))))))

(define-method (equal? (a <chord>) (b <chord>))
  (and (equal? (tonic a) (tonic b))
       (equal? (third a) (third b))
       (equal? (fifth a) (fifth b))
       (equal? (seventh a) (seventh b))
       (equal? (ninth a) (ninth b))
       (eq? (inversion a) (inversion b))))

(define-method (write (c <chord>) port)
  (display "#<" port)
  (display (class-name (class-of c)) port)
  (display " #:tonic " port)
  (write (tonic c) port)
  (display " #:third " port)
  (write (third c) port)
  (display " #:fifth " port)
  (write (fifth c) port)
  (display " #:seventh " port)
  (write (seventh c) port)
  (display " #:ninth " port)
  (write (ninth c) port)
  (display " #:inversion '" port)
  (write (inversion c) port)
  (display ">" port))

;; FIXME: unit test me.
(define-method (pitch (c <chord>) (interval <symbol>))
  "Returns the normalized pitch from TONIC pitch and the chord INTERVAL, #f if the INTERVAL value is #f.

Values for INTERVAL are 'tonic, 'third, 'fifth, 'seventh, 'ninth."
  (if (eq? interval 'tonic)
      (normalize-pitch (tonic c))
      (let ((itv (slot-ref c interval)))
        (and itv
             (normalize-pitch (ascending itv (tonic c)))))))

(define (next-octave pitch)
  "Returns the PITCH transposed up by one octave."
  (ly:make-pitch (+ 1 (ly:pitch-octave pitch))
		 (ly:pitch-notename pitch)
		 (ly:pitch-alteration pitch)))

(define (previous-octave pitch)
  "Returns the PITCH transposed down by one octave."
  (ly:make-pitch (- (ly:pitch-octave pitch) 1)
		 (ly:pitch-notename pitch)
		 (ly:pitch-alteration pitch)))

(define-method (music (c <chord>) duration)
  "Returns an EventChord music usable with ChordNames context of given ly:duration."
  (let ()
    (define (make-note pitch inversion octavation)
      (and pitch
	   (let ((props '()))
	     (if inversion
		 (begin
		   (push! #t props)
		   (push! 'inversion props)))
	     (if octavation
		 (begin
		   (push! octavation props)
		   (push! 'octavation props)))
	     (push! pitch props)
	     (push! 'pitch props)
	     (push! duration props)
	     (push! 'duration props)
	     (apply make-music 'NoteEvent props))))

    (if (not (ly:duration? duration))
	(error "DURATION must be a ly:duration"))
    (let* ((p1 (next-octave (tonic c)))
	   (p3 (and (third c)
		    (ascending (third c) p1)))
	   (p5 (and (fifth c)
		    (ascending (fifth c) p1)))
	   (p7 (and (seventh c)
		    (ascending (seventh c) p1)))
	   (p9 (and (ninth c)
		    (ascending (ninth c) p1)))
	   (notes
	    (case (inversion c)
	      ((root) (list (make-note p1 #f #f)
			    (make-note p3 #f #f)
			    (make-note p5 #f #f)
			    (make-note p7 #f #f)
                            (make-note p9 #f #f)))
	      ((first) (list (make-note (and p3 (previous-octave p3)) #t -1)
			     (make-note (and p5 (previous-octave p5)) #f -1)
			     (make-note (and p7 (previous-octave p7)) #f -1)
                             (make-note (and p9 (previous-octave p9)) #f -1)
			     (make-note p1 #f #f)))
	      ((second) (list (make-note (and p5 (previous-octave p5)) #t -1)
			      (make-note (and p7 (previous-octave p7)) #f -1)
			      (make-note (and p9 (previous-octave p9)) #f -1)
			      (make-note p1 #f #f)
			      (make-note p3 #f #f)))
	      ((third) (list (make-note (and p7 (previous-octave p7)) #t -1)
                             (make-note (and p9 (previous-octave p9)) #f -1)
			     (make-note p1 #f #f)
			     (make-note p3 #f #f)
			     (make-note p5 #f #f)))
	      (else (error "unknown inversion")))))
      (make-music
       'EventChord
       'elements
       ;; Remove #f values.
       (filter values notes)))))

(define-public (degree tonic-pitch pitch)
  "Returns the degree given PITCH in the tonality of the TONIC-PITCH.

Only the 'notename of pitches are considered here, not their alternative.

The returned value is in [1, 7] range, 1 when both pitches have the
same notename.
"
  (+ 1 (modulo (- (ly:pitch-notename pitch)
		  (ly:pitch-notename tonic-pitch))
	       7)))
      
(define-method (unique less (l <list>))
  "Returns the list form L in sorted order where all duplicates have
been removed. LESS must be a callable with two parameters."
  (fold
   (lambda (e prev)
     (if (or (null? prev)
	     (less e (car prev))
	     (less (car prev) e))
	 (cons e prev)
	 prev))
   '()
   ;; We sort in reverse order since (fold) will reverse the list order.
   (sort l (lambda (a b) (not (less a b))))))

(define (bass pitches)
  "Returns the lowest pitch in the input pitches, #f if the input is empty."
  (fold
   (lambda (p ret)
     (or (and ret
	      (if (ly:pitch<? p ret) p ret))
	 p))
   #f
   pitches))

(define-method (chord pitches)
  "Returns the <chord> built from the given list of PITCHES or #f if
no chord is found."
  (if (null? pitches)
      #f
      (let ((normalized-bass (normalize-pitch (bass pitches))))
	(define (guess root)
	  "Returns either #f or <chord>."
	  (let* ((itvs
		  ;; Sort intervals and remove duplicates.
		  (unique
		   <
		   ;; Remove octaves.
		   (filter
		    (lambda (itv) (not (= (number itv) 8)))
		    (map (lambda (p) (simple (pitches->interval
					      (normalize-pitch root)
					      (next-octave (normalize-pitch p)))))
			 pitches))))
		 (numbers (map number itvs))
		 (tonic (normalize-pitch root))
                 (inversion (case (number (simple (pitches->interval tonic (next-octave normalized-bass))))
                              ((8) 'root)
                              ((3) 'first)
                              ((5) 'second)
                              ((7) 'third)
                              (else #f))))
	    (cond
             ((not inversion) #f)
	     ((equal? numbers '(3))
	      (make <chord>
		#:tonic tonic
		#:third (car itvs)
		#:inversion inversion))
	     ((equal? numbers '(5))
	      (make <chord>
		#:tonic tonic
		#:fifth (car itvs)
		#:inversion inversion))
	     ((equal? numbers '(3 5))
	      (make <chord>
		#:tonic tonic
		#:third (car itvs)
		#:fifth (cadr itvs)
		#:inversion inversion))
	     ((equal? numbers '(3 5 7))
	      (make <chord>
		#:tonic tonic
		#:third (car itvs)
		#:fifth (cadr itvs)
		#:seventh (caddr itvs)
		#:inversion inversion))
	     ((equal? numbers '(3 7))
	      (make <chord>
		#:tonic tonic
		#:third (car itvs)
		#:seventh (cadr itvs)
		#:inversion inversion))
	     ((equal? numbers '(5 7))
	      (make <chord>
		#:tonic tonic
		#:fifth (car itvs)
		#:seventh (cadr itvs)
		#:inversion inversion))
             ((equal? numbers '(2))
              (make <chord>
                #:tonic tonic
                #:ninth (compound (car itvs))
                #:inversion inversion))
             ((equal? numbers '(2 7))
              (make <chord>
                #:tonic tonic
                #:seventh (cadr itvs)
                #:ninth (compound (car itvs))
                #:inversion inversion))
             ((equal? numbers '(2 3 7))
              (make <chord>
                #:tonic tonic
                #:third (cadr itvs)
                #:seventh (caddr itvs)
                #:ninth (compound (car itvs))
                #:inversion inversion))
             ((equal? numbers '(2 3 5 7))
              (make <chord>
                #:tonic tonic
                #:third (cadr itvs)
                #:fifth (caddr itvs)
                #:seventh (cadddr itvs)
                #:ninth (compound (car itvs))
                #:inversion inversion))
	     (else #f))))
	(let ((guesses (filter values (map guess pitches))))
	  (and (not (null? guesses))
	       ;; Returns the first guess.
	       (car guesses))))))

(define-method (chord key bass figures)
  "Return the <chord> built from the given KEY, BASS pitch and FIGURES chord.

If FIGURES is #f, consider that the figure is empty (i.e. root
position of the triad)."
  (if (not (eq? (ly:music-property key 'name) 'KeyChangeEvent))
      (error (with-output-to-string (display "input KEY is not a KeyChangeEvent but ")
				    (display (ly:music-property key 'name)))))
  (if (not (ly:pitch? bass))
      (error "input BASS is not a ly:pitch"))
  (if (and figures
	   (not (figures? figures)))
      (error "input FIGURES is neither #f nor an EventChord with figures"))
  (let* ((pitches-alist (key-pitches-alist key))
	 (figures-event-alist
	  (if figures
	      (map (lambda (evt)
		     (cons (let ((figure (ly:music-property evt 'figure)))
			     ;; When using <_+> the '_' should be considered as '3'.
			     (if (null? figure) 3 figure))
			   evt))
		   (ly:music-property figures 'elements))
	      '())))
    (define (parse-pitch figure-number)
      (let* ((base (assq-ref pitches-alist
			     (modulo (+ (- figure-number 1)
					(ly:pitch-notename bass))
				     7)))
	     (alteration (let ((evt (assq-ref figures-event-alist figure-number)))
			   (and evt
				(ly:music-property evt 'alteration))))
	     (altered (ly:make-pitch (ly:pitch-octave base)
				     (ly:pitch-notename base)
				     (if (or (not alteration) (null? alteration))
					 (ly:pitch-alteration base)
					 alteration))))
	(if (= figure-number 1)
	    ;; To have the proper inversion we need to make sure that
	    ;; the 1 is the lowest pitch. Simply transposing to
	    ;; previous octave is enough.
	    ;;
	    ;; Moreover we need to take into account the alteration of
	    ;; the input bass that may not be in the key.
	    (previous-octave (normalize-pitch bass))
	    altered)))
    (define (parse-pitches figure-numbers)
      (map parse-pitch figure-numbers))
    (chord
     (cond
      ((assq-ref figures-event-alist 9) (parse-pitches '(1 3 5 7 9)))
      ((assq-ref figures-event-alist 2) (parse-pitches '(2 4 6 1)))
      ((assq-ref figures-event-alist 7) (parse-pitches '(1 3 5 7)))
      ;; We must test <4 3> first since we can have <6 4 3> (for
      ;; example in minor we can have <6! 4 3>). So if we test <6 4>
      ;; first won't detect the the seventh.
      ((and (assq-ref figures-event-alist 4)
	    (assq-ref figures-event-alist 3))
       (parse-pitches '(4 6 1 3)))
      ((assq-ref figures-event-alist 6)
       (cond
	((assq-ref figures-event-alist 5) (parse-pitches '(6 1 3 5)))
	((assq-ref figures-event-alist 4) (parse-pitches '(4 6 1)))
	(else (parse-pitches '(6 1 3)))))
      (else (parse-pitches '(1 3 5)))))))

(define (key-pitches-alist key)
  "Returns a alist of ((notename . pitch) ...) for the given KEY.

Contrary to 'pitch-alist that is actually a list of alterations, this
is a list of ly:pitch."
  (map (lambda (pitch) (cons (ly:pitch-notename pitch) pitch))
       (key-pitches key)))
	
(define (key-pitches key)
  "Returns the list of ly:pitch of the given KEY in order starting at tonic.

Returned values are normalized; they are in the same octave."
  (if (not (eq? (ly:music-property key 'name) 'KeyChangeEvent))
      (error (with-output-to-string (display "input KEY is not a KeyChangeEvent but ")
				    (display (ly:music-property key 'name)))))
  (let ((tonic-number (ly:pitch-notename (ly:music-property key 'tonic)))
	(alterations (ly:music-property key 'pitch-alist)))
    (let loop ((i 0))
      (if (= i 7)
	  '()
	  (let ((notename (modulo (+ tonic-number i) 7)))
	    (cons (ly:make-pitch -1 notename (assq-ref alterations notename))
		  (loop (+ i 1))))))))

(define (key-pitch-alist key)
  "Return the 'pitch-alist of the input KeyChangeEvent as if the 'tonic was C.

The functions `major-pitch-alist' and `minor-pitch-alist' can then be
used to identify major and minor keys."
  (if (not (eq? (ly:music-property key 'name)
                'KeyChangeEvent))
      (error (format "input KEY is not a KeyChangeEvent but ~s"
                     (ly:music-property key 'name))))
  (let ((m (ly:music-deep-copy key))
        (tonic (ly:music-property key 'tonic)))
    (ly:music-property
     (ly:music-transpose m (ly:pitch-negate tonic))
     'pitch-alist)))

(define (major-pitch-alist pitch-alist)
  "Return #t if the input pitch-alist is the major mode of C."
  (do ((want '(0 0 0 0 0 0 0) (cdr want))
       (i 0 (+ 1 i)))
      ((or (null? want)
           (not (= (cdr (assq i pitch-alist)) (car want))))
       (null? want))))

(define (minor-pitch-alist pitch-alist)
  "Return #t if the input pitch-alist is the minor mode of C."
  (do ((want '(0 0 -1/2 0 0 -1/2 -1/2) (cdr want))
       (i 0 (+ 1 i)))
      ((or (null? want)
           (not (= (cdr (assq i pitch-alist)) (car want))))
       (null? want))))

(define-class <parallel-consonance> ()
  ;; The time of the detection.
  (moment-index #:getter moment-index #:init-keyword #:moment-index)
  ;; The two voices moving in parallel from t-1 to t.
  (voice1 #:getter voice1 #:init-keyword #:voice1)
  (voice2 #:getter voice2 #:init-keyword #:voice2)
  ;; The consonant <interval>.
  (interval #:getter interval #:init-keyword #:interval)
  ;; The kind of parallelism, either 'open or 'hidden.
  (kind #:getter kind #:init-keyword #:kind))

(define-method (equal? (a <parallel-consonance>) (b <parallel-consonance>))
  (and (= (moment-index a) (moment-index b))
       (= (voice1 a) (voice1 b))
       (= (voice2 a) (voice2 b))
       (equal? (interval a) (interval b))
       (eq? (kind a) (kind b))))

(define-method (write (o <parallel-consonance>) port)
  (display "#<" port)
  (display (class-name (class-of o)) port)
  (display " #:moment-index " port)
  (display (moment-index o) port)
  (display " #:voice1 " port)
  (display (voice1 o) port)
  (display " #:voice2 " port)
  (display (voice2 o) port)
  (display " #:interval " port)
  (write (interval o) port)
  (display " #:kind '" port)
  (display (kind o) port)
  (display ">" port))

(define-method (draw (voices <voices>) (p-consonance <parallel-consonance>) options)
  "Modifies the NoteEvent and emit a warning message using the NAME at its origin."
  (let ((n1 (note-event voices
			(voice1 p-consonance)
			(moment-index p-consonance)))
	(n2 (note-event voices
			(voice2 p-consonance)
			(moment-index p-consonance)))
	(col (case (kind p-consonance)
	       ((open) '(color 1.0 0.0 0.0))
	       ((hidden) '(color 0.74 0.0 1.0))
	       (else (error (format "unknown kind ~a"
                                    (kind p-consonance)))))))
    (set! (ly:music-property n1 'tweaks) (list col))
    (set! (ly:music-property n2 'tweaks) (list col))
    (ly:input-warning (ly:music-property n1 'origin)
		      "Parallel ~a ~a from this voice..."
		      (kind p-consonance) (or (assq-ref options 'name) "??"))
    (ly:input-warning (ly:music-property n2 'origin)
		      "...to this voice")))

(define-method (parallel-consonance (voices <voices>)
				    (consonance <interval>))
  "Return the parallel consonnance of the given interval as list of
<parallel-consonance>."
  (do ((t 1 (+ 1 t))
       (consonances '() '())
       (last-consonances '() consonances)
       (parallel-consonances '()))
      ((>= t (vector-length (moments voices)))
       (reverse parallel-consonances))
    (do ((v1 0 (+ 1 v1)))
        ((= v1 (voice-count voices)))
      (do ((v2 (+ 1 v1) (+ 1 v2)))
          ((>= v2 (voice-count voices)))
	;; Validate first that voices are not #f and that previous
	;; note exists.
	(if (and (note voices v1 t)
		 (note voices v2 t)
		 ;; TODO: unit test case where voices exists at t but not at t-1.
		 (note voices v1 (- t 1))
		 (note voices v2 (- t 1)))
	    (let ((interval (interval voices v1 v2 t)))
	      (if (equal? (simple interval) consonance)
		  (push! (cons v1 v2) consonances))))))
    (set! consonances (reverse! consonances))
    (for-each
     (lambda (pair)
       (let ((m1 (movement voices (car pair) t))
             (m2 (movement voices (cdr pair) t))
             (open
              (and (> t 0)
                   (equal? (simple
                            (interval voices (car pair) (cdr pair) (- t 1)))
                           consonance))))
         (if (and (eq? m1 m2)
                  (not (eq? m1 'none))
                  (or open              ; Open consonances.
                      ;; Hidden consonances.
                      (and (= (car pair) 0)
                           (= (cdr pair)
                              (- (voice-count voices) 1))) ; Outer voices.
                      (not
                       (or
                        ;; Step-wise motion of one of the two voices.
                        (= (number
                            (movement-interval voices (car pair) t))
                           2)
                        (= (number
                            (movement-interval voices (cdr pair) t))
                           2)
                        ;; One of the tone belonged to the previous
                        ;; harmony.
                        (let ()
                          (define (octave-less-pitch v t)
                            (let ((p (pitch voices v t)))
                              (cons (ly:pitch-notename p)
                                    (ly:pitch-alteration p))))
                          (let ((previous-pitches
                                 (do ((v 0 (+ 1 v))
                                      (pitches '()))
                                     ((= v (voice-count voices))
                                      (reverse! pitches))
				   ;; TODO: unit test this
				   (if (note voices v (- t 1))
				       (push! (octave-less-pitch v (- t 1))
                                              pitches)))))
                            (or (member (octave-less-pitch (car pair) t) previous-pitches)
                                (member (octave-less-pitch (cdr pair) t) previous-pitches))))))))
             (push! (make <parallel-consonance>
                      #:moment-index t
                      #:voice1 (car pair) #:voice2 (cdr pair)
                      #:interval consonance
                      #:kind (if open 'open 'hidden))
                    parallel-consonances))))
     consonances)))

;; Identifies a single note.
(define-class <single-note> ()
  ;; The time of the detection.
  (moment-index #:getter moment-index #:init-keyword #:moment-index)
  ;; The voice index.
  (voice #:getter voice #:init-keyword #:voice))

(define-method (equal? (a <single-note>) (b <single-note>))
  (and (= (moment-index a) (moment-index b))
       (= (voice a) (voice b))))

(define-method (write (o <single-note>) port)
  (display "#<" port)
  (display (class-name (class-of o)) port)
  (display " #:moment-index " port)
  (display (moment-index o) port)
  (display " #:voice " port)
  (display (voice o) port)
  (display ">" port))

(define-method (draw (voices <voices>)
		     (single-note <single-note>)
		     options)
  "Modifies the NoteEvent and emit a warning MESSAGE at its origin."
  (let ((note (note-event voices
			  (voice single-note)
			  (moment-index single-note))))
    (set! (ly:music-property note 'tweaks) (list (or (assq-ref options 'color)
						     '(color 1.0 0.0 0.0))))
    (ly:input-warning (ly:music-property note 'origin)
		      (or (assq-ref options 'message) "??"))))

;; Detected augmented seconds.
(define-class <augmented-second> (<single-note>))

(define-method (augmented-second (voices <voices>))
  "Return the augmented seconds found."
  (do ((v 0 (+ 1 v))
       (found '())
       (aug-second (make <interval>
		     #:number 2
		     #:quality 'augmented)))
      ((= v (voice-count voices))
       (reverse found))
    (do ((t 1 (+ 1 t)))
        ((= t (vector-length (moments voices))))
      (if (and (note voices v (- t 1))
	       (note voices v t))
	  (let ((itv (pitches->interval 
		      (pitch voices v (- t 1))
		      (pitch voices v t))))
	    (if (equal? (simple itv) aug-second)
		(push! (make <augmented-second>
                         #:moment-index t
                         #:voice v)
                       found)))))))

;; Detect missing thirds in chords.
(define-class <missing-third> (<single-note>))

(define-method (missing-third (voices <voices>))
  "Return the figured bass chords that are missing a third in the realization."
  (do ((t 0 (+ t 1))
       (ret '()))
      ((= t (vector-length (moments voices)))
       (reverse! ret))
    (if (note voices 0 t)
        (let* ((chord (chord-from-figures voices t))
               (p3 (pitch chord 'third))
               (non-empty 0)
               (seen-thrids 0))
          ;; We start at 0 since, thanks to inversions, the bass may be the third.
          (do ((v 0 (+ 1 v)))
              ((= v (voice-count voices)))
            (if (note voices v t)
                (begin
                  (set! non-empty (+ non-empty 1))
                  (if (equal? (normalize-pitch (pitch voices v t)) p3)
                      (set! seen-thrids (+ seen-thrids 1))))))
          (if (and (> non-empty 1)      ; not only the bass
                   (= seen-thrids 0))
              (push! (make <missing-third>
                       #:moment-index t
                       ;; We mark the bass.
                       #:voice 0)
                     ret))))))

;; Detected notes not in harmony.
(define-class <not-in-harmony> (<single-note>))

;; FIXME: unit test this
(define-method (figured-bass (voices <voices>))
  "Return the notes not in harmony."
  (do ((t 0 (+ 1 t))
       (ret '()))
      ((= t (vector-length (moments voices)))
       (reverse ret))
    ;; We can only compute chord if we have a bass. So we need to test
    ;; vocie 0.
    (if (note voices 0 t)
        (let* ((chord (chord-from-figures voices t))
               (p1 (pitch chord 'tonic))
               (p3 (pitch chord 'third))
               (p5 (pitch chord 'fifth))
               (p7 (pitch chord 'seventh))
               (p9 (pitch chord 'ninth))
               (pitches (list p1 p3 p5 p7 p9)))
          (do ((v 0 (+ 1 v)))
              ((= v (voice-count voices)))
            (if (and (note voices v t)  ; We may not have note.
                     (not (member (normalize-pitch (pitch voices v t))
                                  pitches)))
                (push! (make <not-in-harmony>
                         #:moment-index t
                         #:voice v)
                       ret)))))))


(define-class <seventh-resolution> (<single-note>)
  (message #:getter message #:init-keyword #:message))

;; FIXME: add base class to support automatic equal based on introspection.
(define-method (equal? (a <seventh-resolution>) (b <seventh-resolution>))
  (and (string=? (message a) (message b))
       (next-method)))

(define-method (seventh-resolution (voices <voices>))
  (do ((t 0 (+ t 1))
       (ret '()))
      ((>= t (- (vector-length (moments voices)) 1))
       (reverse ret))
    (let ()
      (define (add-error sr) (push! sr ret))
      (define (voices-with-note chord itv)
	"Returns the list of voices that have pitches with the same note
name as pitch of ITV interval from root of the CHORD.

If ITV is #f returns '()."
	(if (not itv)
	    '()
	    (let ((p (ascending itv (tonic chord))))
	      (let loop ((v 0) (ret '()))
		(if (= v (voice-count voices))
		    (reverse ret)
		    (loop (+ v 1)
			  (if (and (note voices v t)
                                   (= (ly:pitch-notename (pitch voices v t))
                                      (ly:pitch-notename p)))
			      (cons v ret)
			      ret)))))))
      (define (inner-voice? voice)
	(and (> voice 0)
	     (< voice (- (voice-count voices) 1))))
      ;; FIXME: share this code
      (define (movement? voice itv-number direction)
        (cond ((not (note voices voice (+ t 1))) #f)
              (else 
               (and (eq? (movement voices voice (+ t 1))
                         direction)
                    (= (number (movement-interval voices voice (+ t 1)))
                       itv-number)))))
      (define (movement-stepwise? voice)
	(or (movement? voice 2 'down)
	    (movement? voice 2 'up)))
      (let ((chord-t (chord-from-notes voices t))
	    (chord-t+1 (chord-from-notes voices (+ t 1))))
	(cond
	 ((not chord-t))
	 ((not chord-t+1)
	  ;; We only report the error if the previous chord is a
	  ;; seventh chord.
	  (if (seventh chord-t)
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice 0
			   #:message "can't compute next chord"))))
	 (else 
	  (let* ((key (key voices t))
		 (key-tonic (if key
				(ly:music-property (music key) 'tonic)
				(normalize-pitch #{c#})))
		 (degree-t (degree key-tonic (tonic chord-t)))
		 (degree-t+1 (degree key-tonic (tonic chord-t+1)))
		 (seventh-vs (voices-with-note chord-t (seventh chord-t)))
		 (fifth-vs (voices-with-note chord-t (fifth chord-t)))
		 (third-vs (voices-with-note chord-t (third chord-t)))
		 (tonic-vs (voices-with-note chord-t (make <interval>)))
		 (non-stationary-tonic-vs
		  (filter (lambda (v)
			    (and (not (= v 0))
				 (not (movement? v 1 'none))))
			  tonic-vs)))
	    (cond
	     ((not (seventh chord-t)))
	     ((seventh chord-t+1)
	      ;; We don't expect two seventh one after the other;
	      ;; unless that is the same chord.
              (if (not (equal? chord-t chord-t+1))
                  (add-error (make <seventh-resolution>
                               #:moment-index t
                               #:voice 0
                               #:message "seventh followed by seventh"))))
	     ((and
               (not (= degree-t 5))
               (not (= degree-t 7)))
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice 0
			   #:message (with-output-to-string
				       (lambda ()
					 (display "only seventh on fifth and seventh degrees are supported, not ")
					 (display degree-t))))))
	     ((not (= degree-t+1 1))
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice 0
			   #:message (with-output-to-string
				       (lambda ()
					 (display "dominant seventh chord should be resolved on first degree, not ")
					 (display degree-t+1))))))
	     ((not (= (length seventh-vs) 1))
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice (cadr seventh-vs)
			   #:message "more that one voice on seventh")))
	     ((if (and (eq? (inversion chord-t+1) 'first)
		       (movement? 0 3 'down))
		  ;; If the resolution interval is the first
		  ;; inversion and the bass goes down a third, the
		  ;; resolution of the seventh should be up.
		  (not (movement? (car seventh-vs) 2 'up))
		  ;; Default resolution is down, except in fundamental form for inner voices.
		  (if (and (eq? (inversion chord-t) 'root)
			   (inner-voice? (car seventh-vs)))
		      (not (or (movement? (car seventh-vs) 2 'down)
			       (movement? (car seventh-vs) 2 'up)))
		      (not (movement? (car seventh-vs) 2 'down))))
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice (car seventh-vs)
			   #:message (string-append "seventh should move on degree "
						    (if (and (eq? (inversion chord-t+1) 'first)
							     (movement? 0 3 'down))
							"upward"
							(if (and (eq? (inversion chord-t) 'root)
								 (inner-voice? (car seventh-vs)))
							    "upward or downward"
							    "downward"))))))
	     ((> (length fifth-vs) 1)
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice (cadr fifth-vs)
			   #:message "more that one voice on fifth")))
	     ((and (not (null? fifth-vs))
		   (not (movement-stepwise? (car fifth-vs)))
		   ;; For Soprano and Tenor in third inversion, the
		   ;; fifth may move more freely.
		   (or (not (and (eq? (inversion chord-t) 'third)
				 (or (= (car fifth-vs) (- (voice-count voices) 1)) ; Soprano
				     (= (car fifth-vs) 1)))) ; Tenor
		       (not (or (movement? (car fifth-vs) 4 'up)
				(movement? (car fifth-vs) 5 'down)))))
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice (car fifth-vs)
			   ;; FIXME: proper message.
			   #:message "fifth should move with stepwise motion, or 4up 5down for soprano and alto of the third inversion")))
	     ((> (length third-vs) 1)
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice (cadr third-vs)
			   #:message "more that one voice on third")))
	     ((if (or (null? third-vs)
                      ;; This test only applies to the seventh chord on the fith degree.
                      (not (= degree-t 5)))
		  #f
		  (if (and (inner-voice? (car third-vs))
			   (eq? (inversion chord-t) 'root))
		      (not (or (movement? (car third-vs) 2 'up)
			       (movement? (car third-vs) 3 'down)))
		      (not (movement? (car third-vs) 2 'up))))
	      (add-error (make <seventh-resolution>
			   #:moment-index t
			   #:voice (car third-vs)
			   #:message (string-append "third should move one step upward"
						    (if (and (inner-voice? (car third-vs))
							     (eq? (inversion chord-t) 'root))
							" or two steps downward"
							"")))))
	     ((and (= degree-t 5)
                   (not (null? non-stationary-tonic-vs)))
	      (for-each
	       (lambda (v)
		 (add-error (make <seventh-resolution>
			      #:moment-index t
			      #:voice v
			      #:message "tonic in upper voices should stay stationary")))
	       non-stationary-tonic-vs))))))))))

(define-class <ninth-resolution> (<single-note>)
  (message #:getter message #:init-keyword #:message))

;; FIXME: add base class to support automatic equal based on introspection.
(define-method (equal? (a <ninth-resolution>) (b <ninth-resolution>))
  (and (string=? (message a) (message b))
       (next-method)))

(define-method (ninth-resolution (voices <voices>))
  (do ((t 0 (+ t 1))
       (errors '()))
      ((= t (vector-length (moments voices)))
       (reverse! errors))
    (let ((chord (chord-from-notes voices t)))
      (if (and chord (ninth chord))
          (let* ((ninth-pitch (pitch chord 'ninth))
                 (ninth-voice
                  (do ((v 0 (+ v 1)))
                      ((or (= v (voice-count voices))
                           (let ((tn (note voices v t)))
                             (and tn
                                  (equal?
                                   (normalize-pitch
                                    (ly:music-property (music tn) 'pitch))
                                   ninth-pitch))))
                       (if (= v (voice-count voices))
                           (error "we should not reach this case"))
                       v)))
                 (next-chord
                  (and (< (+ t 1) (vector-length (moments voices)))
                       (chord-from-notes voices (+ t 1))))
                 (key (key voices t))
		 (key-tonic (if key
				(ly:music-property (music key) 'tonic)
				(normalize-pitch #{c#}))))
            (cond
             ((or (= t 0)
                  (not (note voices ninth-voice (- t 1))))
              (push! (make <ninth-resolution> 
                       #:moment-index t
                       #:voice ninth-voice
                       #:message "ninth must be prepared")
                     errors))
             ((or (= (+ t 1) (vector-length (moments voices)))
                  (not (note voices ninth-voice (+ t 1))))
              (push! (make <ninth-resolution>
                       #:moment-index t
                       #:voice ninth-voice
                       #:message "ninth must be resolved")
                     errors))
             ((not (= (degree key-tonic (tonic chord)) 5))
              (push! (make <ninth-resolution>
                       #:moment-index t
                       #:voice ninth-voice
                       #:message "ninth must be the dominant")
                     errors))
             ((not (and next-chord
                        (= (degree key-tonic (tonic next-chord)) 1)))
              (push! (make <ninth-resolution>
                       #:moment-index t
                       #:voice ninth-voice
                       #:message "dominant ninth must be resolved by I")
                     errors))
             ((and (not (eq? (movement voices ninth-voice t) 'none))
                   (not (equal? (pitches voices (- t 1))
                                (pitches voices (+ t 1)))))
              (push! (make <ninth-resolution>
                       #:moment-index t
                       #:voice ninth-voice
                       #:message "ninth must exist in the previous code in the same voice; of the shape fo the previous chord must be the same as the shape of the next chord")
                     errors))
             ((not (and (eq? (movement voices ninth-voice (+ t 1)) 'down)
                        (= (number (movement-interval voices ninth-voice (+ t 1)))
                           2)))
              (push! (make <ninth-resolution>
                       #:moment-index t
                       #:voice ninth-voice
                       #:message "ninth must be resolved by going a second down")
                     errors))))))))

(define-class <voice-range> (<single-note>)
  (message #:getter message #:init-keyword #:message))

;; FIXME: add base class to support automatic equal based on introspection.
(define-method (equal? (a <voice-range>) (b <voice-range>))
  (and (string=? (message a) (message b))
       (next-method)))

;; FIXME: support missing voices when proper clefs are used.
(define-method (four-voice-ranges (voices <voices>))
  "Test the range of voices when four voices are present at any given time."
  (let ((ranges
         (list
          (list #{f,#} #{d'#} "bass")
          (list #{c#} #{a'#} "tenor")      
          (list #{f#} #{d''#} "alto")     
          (list #{c'#} #{a''#} "soprano"))))
    (define (out-of-range p range)
      (or (ly:pitch<? p (car range))    ; p < min
          (ly:pitch<? (cadr range) p))) ; max < p
    (do ((t 0 (+ t 1))
         (ret '()))
        ((= t (vector-length (moments voices)))
         (reverse! ret))
      (let ((ps (pitches voices t)))
        (if (= (length ps) 4)
            (do ((v 0 (+ v 1))
                 (p ps (cdr p))
                 (r ranges (cdr r)))
                ((= v 4))
              (if (out-of-range (car p) (car r))
                  (begin
                    (push! (make <voice-range>
                             #:voice v
                             #:moment-index t
                             #:message (string-append "voice out of " (caddr (car r)) " range"))
                           ret)))))))))

(define checkHarmonyRules
  (define-music-function (options music)
    ((list? '()) ly:music?)
    "Check the harmony rules in the input MUSIC.

OPTIONS is an Alist of symbols that control the activation of rules:

- 'figured-bass: test that notes in voices match the figured
bass (default is #f) and that all harmonies contains a third.
- 'seventh-resolution: test resolution of seventh chord.
- 'ninth-resolution: test resolution of ninth chord.

Example:

  \\checkHarmonyRules #'((figured-bass . #t)) \new PianoStaff <<
    ...
  >>
"
    (for-each
     (lambda (p) (if (not (member (car p) '(figured-bass
					    seventh-resolution
                                            ninth-resolution
                                            ranges)))
		     (ly:parser-error
		      (with-output-to-string
			(lambda ()
			  (display "unsupport option ")
			  (write (car p)))))))
     options)
    ;; Set the default values of options. The assq-ref will return the
    ;; first one, so the user option are taken first.
    (set! options
	  (concatenate
	   (list
	    options
	    ;; Default values.
	    '((figured-bass . #f)
	      (seventh-resolution . #t)
              (ninth-resolution . #t)
              (ranges . #t)))))
    ;; We will modify events so we need to actually repeat 'unfold
    ;; repeat' music.
    (set! music
          (map-some-music
           (lambda (m)
             (and (music-is-of-type? m 'unfolded-repeated-music)
                  ;; FIXME: handle the case of 'elements
                  ;; property. Looking at lilypond code it seems we
                  ;; may have non-empty elements in some cases.
                  (make-sequential-music
                   (ly:music-deep-copy (make-list (ly:music-property m 'repeat-count)
                                                  (ly:music-property m 'element))))))
           music))
    (for-each
     (lambda (events)
       (display "\n================================================================================\n")
       (let ((voices (get-voices events)))
         (for-each (lambda (p-consonance) (draw voices p-consonance '((name . "fifth"))))
		   (parallel-consonance
		    voices
		    (make <interval> #:number 5 #:quality 'perfect)))
         (for-each (lambda (p-consonance) (draw voices p-consonance '((name . "octave"))))
		   (parallel-consonance
		    voices
		    (make <interval> #:number 8 #:quality 'perfect)))
	 (if (assq-ref options 'figured-bass)
             (begin
               (for-each (lambda (single-note)
                           (draw voices single-note
                                 '((message . "Not in the harmony of the figured bass.")
                                   (color . (color 0.8 0.5 0)))))
                         (figured-bass voices))
               (for-each (lambda (single-note)
                           (draw voices single-note '((message . "Missing third."))))
                         (missing-third voices))))
	 (if (assq-ref options 'seventh-resolution)
	     (for-each (lambda (sr)
			 (draw voices sr (list (cons 'message (message sr)))))
		       (seventh-resolution voices)))
	 (if (assq-ref options 'ninth-resolution)
	     (for-each (lambda (nr)
			 (draw voices nr (list (cons 'message (message nr)))))
		       (ninth-resolution voices)))
         (if (assq-ref options 'ranges)
             (for-each (lambda (vr)
                         (draw voices vr (list (cons 'message (message vr)))))
                       (four-voice-ranges voices)))
         (for-each (lambda (single-note)
		     (draw voices single-note '((message . "Augmented second."))))
		   (augmented-second voices))))
     (events-split (events! music)))
    music))

(define automaticChords
  (define-music-function (origin music)
    ((symbol? 'notes) ly:music?)
    "Returns a sequential music that can be used in a ChordNames
context to display chords of the music.

Depending on ORIGIN it uses either the notes when 'notes is provided
or the figures when 'figures is provided."
    (let ((voices (get-voices (events! (ly:music-deep-copy music)))))
      (case origin
	((notes) (chords-from-notes voices))
	((figures) (chords-from-figures voices))
	(else (ly:parser-error "unexpected symbol value"))))))

(define splitVoices
  (define-music-function (packed-music)
    (ly:music?)
    "Returns a parallel music with each voice in its own staff."
    (let ((voices (get-voices (events! (ly:music-deep-copy packed-music)))))
      (do ((v 0 (+ v 1))
	   (staves '()))
	  ((= v (voice-count voices))
	   ;; Here we don't use (reverse) since voice 0 is the bass,
	   ;; and we want it a the bottom.
	   (make-simultaneous-music staves))
	(let ((seq (music voices v)))
	  (push! #{ \new Staff { #seq } #} staves))))))
	   
      

