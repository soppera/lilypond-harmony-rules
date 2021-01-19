(define-module (sustain)
  #:use-module (lily)
  #:use-module ((srfi srfi-1)
		#:select (any))
  #:export (sustainRythm))

(define sustainRythm
  (define-music-function (music)
    (ly:music?)
    (let ((music (ly:music-deep-copy music))
          (sustain-on #f)
          (last-rhythmic-event #f))
      (map-some-music
       (lambda (m)
         (cond
          ((music-is-of-type? m 'rhythmic-event)
           (let* ((sustain-changes (all-sustain-changes m))
                  (set-on (any identity sustain-changes))
                  (set-off (any not sustain-changes))
                  (duration (ly:music-property m 'duration))
                  (new-sustain-on (or set-on
                                      (and (not set-on)
                                           (not set-off)
                                           sustain-on)))
                  (new-m (if new-sustain-on
                             (make-music 'NoteEvent
                                         'pitch (ly:make-pitch 0 0 0)
                                         'duration duration)
                             (make-music 'RestEvent
                                         'duration duration))))
             (if (and last-rhythmic-event
                      (eq? sustain-on new-sustain-on)
                      ;; We don't want to tie the note if we have on
                      ;; and off on the same note since the sustain is
                      ;; supposed to be interrupted.
                      (not (and set-on set-off)))
                 (ly:music-set-property!
                  last-rhythmic-event
                  'articulations (list (make-music 'TieEvent))))
             (set! sustain-on new-sustain-on)
             (set! last-rhythmic-event new-m)
             new-m))
          ;; We don't want to break ties on bar checks.
          ((eq? (ly:music-property m 'name) 'BarCheck) #f)
          ;; Break ties for other events.
          (else (set! last-rhythmic-event #f) #f)))
       music))))
            

(define (all-sustain-changes music)
  "Returns a list of boolean values from all SustainEvent in the
'articulations list of the input music."
  (map
   (lambda (e) (= (ly:music-property e 'span-direction) START))
   (filter
    (lambda (e) (music-is-of-type? e 'sustain-event))
    (ly:music-property music 'articulations))))
