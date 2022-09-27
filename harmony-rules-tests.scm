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

(define-module (harmony-rules-tests)
  #:use-module (lily)
  #:use-module (harmony-rules)
  #:use-module (oop goops)
  #:use-module (test-tools)
  #:use-module (unit-testing))

;; We can't use functions not defined in modules here so we redefine
;; (key) function.
(define major '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)))
(define minor '((0 . 0) (1 . 0) (2 . -1/2) (3 . 0) (4 . 0) (5 . -1/2) (6 . -1/2)))
(define (key pitch pitch-alist)
  #{ \key #pitch #pitch-alist #})

(define (first-element m) (car (ly:music-property m 'elements)))

(define (for-each-test fn tests)
  (for-each
   (lambda (args) (apply fn args))
   tests))

(define mOne #{
\new PianoStaff <<
  \new Staff
  \relative c' {
    a'4
    << { \voiceOne b2 }
       \new Voice
       { \voiceTwo <c e g>4 }
     >> <d e>4 |
  }
  \new Staff \relative c {
    \clef bass
    a2 b4 c8 d8 |
  }
>>#})

(define mTwo #{
\new PianoStaff <<
  \new Staff
  \relative c' {
    <c e g>4 <b d g>2 <b d g>4 |
  }
  \new Staff \relative c {
    \clef bass
    c4 g2 g4 |
  }
>>#})

(define mThree #{
\new ChoirStaff <<
  \new Staff = "sopranos"
  \with { instrumentName = "Soprano" }
  <<
    \new Voice = "sopranos" \relative c' {
      c'1 | d1 | e1 | c1 |
    }
  >>
  \new Staff = "altos"
  \with { instrumentName = "Alto" }
  <<
    \new Voice = "altos" \relative g' {
      g1 | a1 | b1 | g1 |
    }
  >>
  \new Staff = "tenors"
  \with { instrumentName = "Tenor" }
  <<
    \new Voice = "tenors" \relative c {
      c'1 | d1 | e1 | c1 |
    }
  >>
  \new Staff = "basses"
  \with { instrumentName = "Bass" }
  <<
    \new Voice = "basses" \relative g, {
      \clef "bass"
      g1 | a1 | b1 | g1 |
    }
  >>
>>#})

(define mFour #{
\new ChoirStaff <<
  \new Staff = "sopranos"
  \with { instrumentName = "Soprano" }
  <<
    \new Voice = "sopranos" \relative c'' {
      s1 | \bar "||" d1 | e1 | c1 |
    }
  >>
  \new Staff = "altos"
  \with { instrumentName = "Alto" }
  <<
    \new Voice = "altos" \relative g' {
      g1 | a1 | b1 | \bar "||" g1 |
    }
  >>
  \new Staff = "tenors"
  \with { instrumentName = "Tenor" }
  <<
    \new Voice = "tenors" \relative c {
      c'1 | \bar "||" d1 | e1 | c1 | \bar "|."
    }
  >>
  \new Staff = "basses"
  \with { instrumentName = "Bass" }
  <<
    \new Voice = "basses" \relative g, {
      \clef "bass"
      g1 | \bar "||" a1 | r1 | g1 |
    }
  >>
>>#})

(define mFive #{
\new PianoStaff <<
  \new Staff
  \relative c' {
    <g c e>1 | <d' g b>1 | <e a c>1 | <g b d>1  | <f a c>1 | <a, f' c'>1 | <g' b d>1 |
  }
  \new Staff \relative c {
    \clef bass
    c1       | g1        | c1       | g1        | f1       | e1       | g1        |
  }
>>#})

(define mSix #{
\new PianoStaff <<
  \new Staff
  \relative c' {
    <c e g>4 <b d g>2 <b d g>4 |
  }
  \new Staff \relative c {
    \clef bass
    c4 g2. |
  }
>>#})

(define mSeven #{
\new PianoStaff <<
  \new Staff \relative c' {
    <c e g>4 <c e aes> <c e g> <c e ais> |
  }
  \new Staff \relative c {
    \clef bass
    c4 aes c ais |
  }
>>#})

(define mEight #{
\new PianoStaff <<
  \new Staff <<
    \clef "treble"
    \relative { e''1 | d1 | }
    \\
    \relative { c''1 | g1 | }
  >>
  \new Staff \relative {
    \clef bass
    a1 | b1 |
  }
>> #})

(define mNine #{
\new PianoStaff <<
  \new Staff <<
    \clef "treble"
    \relative { d''1 | b1 | }
  >>
  \new Staff <<
    \clef bass
    \relative { a1 | d1 | }
    \\
    \relative { f1 | g1 | }
  >>
>>#})

(define mTen #{
\new PianoStaff <<
  \new Staff <<
    \clef "treble"
    \relative { d''1 | c1 | }
    \\
    \relative { g'1 | c,1 | }
  >>
  \new Staff \relative {
    \clef bass
    b,1 | c1 |
  }
>>#})
  
(define mEleven #{
\new PianoStaff <<
  \new Staff <<
    \clef "treble"
    \relative { a'1 | b1 | }
    \\
    \relative { c'1 | g'1 | }
  >>
  \new Staff \relative {
    \clef bass
    f1 | g1 |
  }
>>#})

(define mTwelve #{
\new PianoStaff <<
  \new Staff <<
    \clef "treble"
    \relative { g''1 | c,1 | }
    \\
    \relative { c'1 | e1 | }
  >>
  \new Staff \relative {
    \clef bass
    e1 | a1 |
  }
>>#})
  
(define mThirteen #{
\new PianoStaff <<
  \new Staff <<
    \clef "treble"
    \relative { g''1 | b,1 | }
    \\
    \relative { c'1 | g'1 | }
  >>
  \new Staff \relative {
    \clef bass
    e1 | g1 |
  }
>>#})


(tests
 (test-case "pitches->interval"
   (for-each-test
    (lambda (p1 p2 want)
      (test-case (format "~S ~S" p1 p2)
        (test-that equal? (pitches->interval p1 p2) want)))
    (list
     (list #{c#} #{c'#} (make <interval> #:number 8 #:quality 'perfect))
     (list #{c#} #{c,#} (make <interval> #:number 8 #:quality 'perfect))
     (list #{c,#} #{c'#} (make <interval> #:number 15 #:quality 'perfect))
     (list #{c#} #{g#} (make <interval> #:number 5 #:quality 'perfect))
     (list #{g,#} #{c#} (make <interval> #:number 4 #:quality 'perfect))
     (list #{g#} #{c#} (make <interval> #:number 5 #:quality 'perfect))
     (list #{c'#} #{g#} (make <interval> #:number 4 #:quality 'perfect))
     (list #{c#} #{e#} (make <interval> #:number 3 #:quality 'major))
     (list #{d#} #{f#} (make <interval> #:number 3 #:quality 'minor))
     (list #{c#} #{b'#} (make <interval> #:number 14 #:quality 'major))
     (list #{b'#} #{c#} (make <interval> #:number 14 #:quality 'major))
     (list #{c#} #{b#} (make <interval> #:number 7 #:quality 'major))
     (list #{b#} #{c#} (make <interval> #:number 7 #:quality 'major))
     (list #{c#} #{b,#} (make <interval> #:number 2 #:quality 'minor))
     (list #{b,#} #{c#} (make <interval> #:number 2 #:quality 'minor))
     (list #{c#} #{deses#} (make <interval> #:number 2 #:quality 'diminished))
     (list #{c#} #{cis#} (make <interval> #:number 1 #:quality 'augmented))
     (list #{cis#} #{cis#} (make <interval> #:number 1 #:quality 'perfect)))))

 (test-case "compound? <interval>"
   (for-each
    (lambda (test)
      (let* ((itv (car test))
             (want (cdr test))
             (got (compound? itv)))
        (test-that equal? got want)))
    (list (cons (make <interval> #:number 1 #:quality 'perfect) #f)
          (cons (make <interval> #:number 2 #:quality 'minor) #f) 
          (cons (make <interval> #:number 2 #:quality 'major) #f) 
          (cons (make <interval> #:number 5 #:quality 'diminished) #f) 
          (cons (make <interval> #:number 5 #:quality 'perfect) #f) 
          (cons (make <interval> #:number 6 #:quality 'major) #f) 
          (cons (make <interval> #:number 7 #:quality 'major) #f) 
          (cons (make <interval> #:number 7 #:quality 'augmented) #f) 
          (cons (make <interval> #:number 8 #:quality 'perfect) #f) 
          (cons (make <interval> #:number 9 #:quality 'diminished) #t) 
          (cons (make <interval> #:number 9 #:quality 'minor) #t) 
          (cons (make <interval> #:number 9 #:quality 'major) #t) 
          (cons (make <interval> #:number 10 #:quality 'major) #t))))

 (test-case "simple <interval>"
   (for-each
    (lambda (test)
      (let* ((itv (car test))
             (want (cdr test))
             (got (simple itv)))
        (test-that equal? got want)))
    (list (cons (make <interval> #:number 1 #:quality 'perfect) (make <interval> #:number 1 #:quality 'perfect))
          (cons (make <interval> #:number 2 #:quality 'minor) (make <interval> #:number 2 #:quality 'minor))
          (cons (make <interval> #:number 2 #:quality 'major) (make <interval> #:number 2 #:quality 'major))
          (cons (make <interval> #:number 5 #:quality 'diminished) (make <interval> #:number 5 #:quality 'diminished))
          (cons (make <interval> #:number 5 #:quality 'perfect) (make <interval> #:number 5 #:quality 'perfect))
          (cons (make <interval> #:number 6 #:quality 'major) (make <interval> #:number 6 #:quality 'major))
          (cons (make <interval> #:number 7 #:quality 'major) (make <interval> #:number 7 #:quality 'major))
          (cons (make <interval> #:number 7 #:quality 'augmented) (make <interval> #:number 7 #:quality 'augmented))
          (cons (make <interval> #:number 8 #:quality 'perfect) (make <interval> #:number 8 #:quality 'perfect))
          (cons (make <interval> #:number 15 #:quality 'perfect) (make <interval> #:number 8 #:quality 'perfect))
          (cons (make <interval> #:number 9 #:quality 'diminished) (make <interval> #:number 2 #:quality 'diminished))
          (cons (make <interval> #:number 9 #:quality 'minor) (make <interval> #:number 2 #:quality 'minor))
          (cons (make <interval> #:number 9 #:quality 'major) (make <interval> #:number 2 #:quality 'major))
          (cons (make <interval> #:number 10 #:quality 'major) (make <interval> #:number 3 #:quality 'major))
          (cons (make <interval> #:number 19 #:quality 'perfect) (make <interval> #:number 5 #:quality 'perfect)))))

 (test-case "compound <interval>"
   (for-each
    (lambda (test)
      (let* ((itv (car test))
             (want (cdr test))
             (got (compound itv)))
        (test-that equal? got want)))
    (list (cons (make <interval> #:number 1 #:quality 'perfect) (make <interval> #:number 8 #:quality 'perfect))
          (cons (make <interval> #:number 2 #:quality 'minor) (make <interval> #:number 9 #:quality 'minor))
          (cons (make <interval> #:number 2 #:quality 'major) (make <interval> #:number 9 #:quality 'major))
          (cons (make <interval> #:number 5 #:quality 'diminished) (make <interval> #:number 12 #:quality 'diminished))
          (cons (make <interval> #:number 5 #:quality 'perfect) (make <interval> #:number 12 #:quality 'perfect))
          (cons (make <interval> #:number 6 #:quality 'major) (make <interval> #:number 13 #:quality 'major))
          (cons (make <interval> #:number 7 #:quality 'major) (make <interval> #:number 14 #:quality 'major))
          (cons (make <interval> #:number 7 #:quality 'augmented) (make <interval> #:number 14 #:quality 'augmented))
          (cons (make <interval> #:number 8 #:quality 'perfect) (make <interval> #:number 15 #:quality 'perfect))
          (cons (make <interval> #:number 15 #:quality 'perfect) (make <interval> #:number 15 #:quality 'perfect))
          (cons (make <interval> #:number 9 #:quality 'diminished) (make <interval> #:number 9 #:quality 'diminished))
          (cons (make <interval> #:number 9 #:quality 'minor) (make <interval> #:number 9 #:quality 'minor))
          (cons (make <interval> #:number 9 #:quality 'major) (make <interval> #:number 9 #:quality 'major))
          (cons (make <interval> #:number 10 #:quality 'major) (make <interval> #:number 10 #:quality 'major))
          (cons (make <interval> #:number 19 #:quality 'perfect) (make <interval> #:number 19 #:quality 'perfect)))))

 (test-case "events!"
   (for-each
    (lambda (test)
      (let* ((m (car test))
             (want (cdr test))
             (got (events! m)))
        ;; FIXME: make this more generic (or implement a comparision that
        ;; ignores some properties for music).
        (define (make-comparable events)
          (make <events>
            #:notes
            (map
             (lambda (evt) (make <timed-note>
                             #:moment (moment evt)
                             #:music (remove-length-and-origin (music evt))))
             (notes events))
            #:keys
            (map
             (lambda (evt) (make <timed-key>
                             #:moment (moment evt)
                             #:music (remove-length-and-origin (music evt))))
             (keys events))
            #:bars
            (bars events)))
        (add-score (scorify-music (ly:music-deep-copy m)))
        (let ((lines (string-split (test-pretty-print got "    ") #\newline)))
          (for-each
           (lambda (line)
             (add-text #{ \markup { \column { \wordwrap { $line } } } #}))
           lines))
        (let ((comparable-got (make-comparable got))
              (comparable-want (make-comparable want)))
          (test-that equal? comparable-got comparable-want))))
    (list (cons mOne
                (make <events>
                  #:notes
                  (list (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{a'#} 2))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{b'#} 1))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{c''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{e''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{g''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{d''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{e''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{a,#} 1))
                        (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{b,#} 2))
                        (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{c#} 3))
                        (make <timed-note> #:moment (ly:make-moment 7/8) #:music (make-note #{d#} 3)))))
          (cons mFour
                (make <events>
                  #:notes
                  (list
                   ;; Soprano
                   ;; No time 0 since we have a SkipEvent.
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d''#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e''#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{c''#} 0))
                   ;; Alto
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{g'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{a'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{b'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{g'#} 0))
                   ;; Tenor
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{c'#} 0))
                   ;; Bass
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{g,#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{a,#} 0))
                   ;; No time 0 since we have a RestEvent.
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{g,#} 0)))
                  #:bars
                  (list (ly:make-moment 1)
                        (ly:make-moment 3)
                        (ly:make-moment 4))))
          (cons #{ \repeat unfold 2 { c'1 d' } e'1 #}
                (make <events>
                  #:notes
                  (list
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{c'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{d'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 4) #:music (make-note #{e'#} 0))
                   )))
          (cons #{ c'4 d' e' \key d \major f' g' \key c \minor a' b' #}
                (make <events>
                  #:notes
                  (list
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{d'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{e'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{f'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{g'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 5/4) #:music (make-note #{a'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 3/2) #:music (make-note #{b'#} 2)))
                  #:keys
                  (list
                   (make <timed-key> #:moment (ly:make-moment 3/4) #:music (key #{d#} major))
                   (make <timed-key> #:moment (ly:make-moment 5/4) #:music (key #{c#} minor)))))
          (cons #{ << { c4 d e f } \figuremode { <6>4 <> s2 <5>4 } >> #}
                (make <events>
                  #:notes
                  (list
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{d#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{e#} 2))
                   (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{f#} 2)))
                  #:figures
                  (list
                   (make <timed-figure> #:moment (ly:make-moment 0) #:music (first-element #{ \figuremode { <6>4 } #}))
                   (make <timed-figure> #:moment (ly:make-moment 3/4) #:music (first-element #{ \figuremode { <5>4 } #})))))
          (cons #{ { c4 4 8 8 f4 } #}
                (make <events>
                  #:notes
                  (list
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{c#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{c#} 3))
                   (make <timed-note> #:moment (ly:make-moment 5/8) #:music (make-note #{c#} 3))
                   (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{f#} 2)))))
          (cons #{ { r4 c s d } #}
                (make <events>
                  #:notes
                  (list
                   (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{c#} 2))
                   (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{d#} 2))))))))

 (test-case "events-split"
   (for-each
    (lambda (test)
      (let* ((events (car test))
             (want (cdr test))
             (got (events-split events)))
        (test-that equal? got want)))
    (list (cons (make <events>
                  #:notes
                  (list (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{a'#} 2))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{b'#} 1))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{c''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{e''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{g''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{d''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{e''#} 2))
                        (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{a,#} 1))
                        (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{b,#} 2))
                        (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{c#} 3))
                        (make <timed-note> #:moment (ly:make-moment 7/8) #:music (make-note #{d#} 3))))
                (list
                 (make <events>
                   #:notes
                   (list (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{a'#} 2))
                         (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{a,#} 1))
                         (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{b'#} 1))
                         (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{c''#} 2))
                         (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{e''#} 2))
                         (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{g''#} 2))
                         (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{b,#} 2))
                         (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{d''#} 2))
                         (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{e''#} 2))
                         (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{c#} 3))
                         (make <timed-note> #:moment (ly:make-moment 7/8) #:music (make-note #{d#} 3))))))
          (cons (make <events>
                  #:notes
                  (list
                   ;; Soprano
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d''#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e''#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{c''#} 0))
                   ;; Alto
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{g'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{a'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{b'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{g'#} 0))
                   ;; Tenor
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e'#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{c'#} 0))
                   ;; Bass
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{g,#} 0))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{a,#} 0))
                   (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{g,#} 0)))
                  #:bars
                  (list (ly:make-moment 1)
                        (ly:make-moment 3)
                        (ly:make-moment 4)))
                (list
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{g'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{g,#} 0))))
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d''#} 0))
                    (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{a'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{a,#} 0))
                    (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e''#} 0))
                    (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{b'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e'#} 0))))
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{c''#} 0))
                    (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{g'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{c'#} 0))
                    (make <timed-note> #:moment (ly:make-moment 3) #:music (make-note #{g,#} 0))))))
          (cons (make <events>
                  #:notes
                  (list
                   (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{d'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{e'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{f'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{g'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 5/4) #:music (make-note #{a'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 3/2) #:music (make-note #{b'#} 2))
                   (make <timed-note> #:moment (ly:make-moment 7/4) #:music (make-note #{c#} 2)))
                  #:keys
                  (list
                   (make <timed-key> #:moment (ly:make-moment 3/4) #:music (key #{d#} major))
                   (make <timed-key> #:moment (ly:make-moment 5/4) #:music (key #{c#} minor)))
                  #:bars
                  (list (ly:make-moment 1) (ly:make-moment 5/4) (ly:make-moment 3/2)))
                (list
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c'#} 2))
                    (make <timed-note> #:moment (ly:make-moment 1/4) #:music (make-note #{d'#} 2))
                    (make <timed-note> #:moment (ly:make-moment 1/2) #:music (make-note #{e'#} 2))
                    (make <timed-note> #:moment (ly:make-moment 3/4) #:music (make-note #{f'#} 2)))
                   #:keys
                   (list
                    (make <timed-key> #:moment (ly:make-moment 3/4) #:music (key #{d#} major))))
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{g'#} 2)))
                   #:keys
                   (list
                    (make <timed-key> #:moment (ly:make-moment 3/4) #:music (key #{d#} major))))
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 5/4) #:music (make-note #{a'#} 2)))
                   #:keys
                   (list
                    (make <timed-key> #:moment (ly:make-moment 5/4) #:music (key #{c#} minor))))
                 (make <events>
                   #:notes
                   (list
                    (make <timed-note> #:moment (ly:make-moment 3/2) #:music (make-note #{b'#} 2))
                    (make <timed-note> #:moment (ly:make-moment 7/4) #:music (make-note #{c#} 2)))
                   #:keys
                   (list
                    (make <timed-key> #:moment (ly:make-moment 5/4) #:music (key #{c#} minor))))))
          (cons
           (make <events>
             #:notes
             (list
              (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c#} 2))
              (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d#} 2))
              (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e#} 2)))
             #:figures
             (list
              (make <timed-figure> #:moment (ly:make-moment 0) #:music (first-element #{ \figuremode { <6>4 } #}))
              (make <timed-figure> #:moment (ly:make-moment 1) #:music (first-element #{ \figuremode { <5>4 } #}))
              (make <timed-figure> #:moment (ly:make-moment 2) #:music (first-element #{ \figuremode { <4>4 } #})))
             #:bars (list (ly:make-moment 1/2) (ly:make-moment 2)))
           (list
            (make <events>
              #:notes
              (list (make <timed-note> #:moment (ly:make-moment 0) #:music (make-note #{c#} 2)))
              #:figures
              (list (make <timed-figure> #:moment (ly:make-moment 0) #:music (first-element #{ \figuremode { <6>4 } #}))))
            (make <events>
              #:notes
              (list (make <timed-note> #:moment (ly:make-moment 1) #:music (make-note #{d#} 2)))
              #:figures
              (list (make <timed-figure> #:moment (ly:make-moment 1) #:music (first-element #{ \figuremode { <5>4 } #}))))
            (make <events>
              #:notes
              (list (make <timed-note> #:moment (ly:make-moment 2) #:music (make-note #{e#} 2)))
              #:figures
              (list (make <timed-figure> #:moment (ly:make-moment 2) #:music (first-element #{ \figuremode { <4>4 } #})))))))))

 (test-case "get-voices <events>"
   (for-each
    (lambda (test)
      (let* ((m (car test))
             (want (cdr test))
             (got (get-voices (events! (ly:music-deep-copy m)))))
        (add-score (scorify-music (ly:music-deep-copy m)))
        (let ((lines (string-split (test-pretty-print got "    ") #\newline)))
          (for-each
           (lambda (line)
             (add-text #{ \markup { \column { \wordwrap { $line } } } #}))
           lines))
        (test-that equal? got want)))
    (list
     (cons
      mTwo
      (make <voices>
        #:voice-count 4
        #:moments (list->vector (map ly:make-moment '(0 1/4 3/4)))
        #:notes
        (list->vector
         (list
          (timed-note 0 #{c#} 2) (timed-note 0 #{c'#} 2) (timed-note 0 #{e'#} 2) (timed-note 0 #{g'#} 2)
          (timed-note 1/4 #{g,#} 1) (timed-note 1/4 #{b#} 1) (timed-note 1/4 #{d'#} 1) (timed-note 1/4 #{g'#} 1)
          (timed-note 3/4 #{g,#} 2) (timed-note 3/4 #{b#} 2) (timed-note 3/4 #{d'#} 2) (timed-note 3/4 #{g'#} 2)))
        #:figures (make-vector 3 #f)
        #:keys (make-vector 3 #f)))
     (cons
      mThree
      (make <voices>
        #:voice-count 4
        #:moments (list->vector (map ly:make-moment '(0 1 2 3)))
        #:notes
        (list->vector
         (list
          (timed-note 0 #{g,#} 0) (timed-note 0 #{c'#} 0) (timed-note 0 #{g'#} 0) (timed-note 0 #{c''#} 0)
          (timed-note 1 #{a,#} 0) (timed-note 1 #{d'#} 0) (timed-note 1 #{a'#} 0) (timed-note 1 #{d''#} 0)
          (timed-note 2 #{b,#} 0) (timed-note 2 #{e'#} 0) (timed-note 2 #{b'#} 0) (timed-note 2 #{e''#} 0)
          (timed-note 3 #{g,#} 0) (timed-note 3 #{c'#} 0) (timed-note 3 #{g'#} 0) (timed-note 3 #{c''#} 0)))
        #:figures (make-vector 4 #f)
        #:keys (make-vector 4 #f)))
     ;; Temporary disabling test mSix that requires rewriting (voices) entirely.
     ;; (cons
     ;;  mSix
     ;;  (make <voices>
     ;;   #:voice-count 4
     ;;   #:moments (list->vector (map ly:make-moment '(0 1/4 3/4)))
     ;;   #:notes
     ;;   (list->vector
     ;;    (list
     ;;     (timed-note 0 #{c#} 2) (timed-note 0 #{c'#} 2) (timed-note 0 #{e'#} 2) (timed-note 0 #{g'#} 2)
     ;;     (timed-note 1/4 #{g,#} 1) (timed-note 1/4 #{b#} 1) (timed-note 1/4 #{d'#} 1) (timed-note 1/4 #{g'#} 1)
     ;;     (timed-note 3/4 #{g,#} 2) (timed-note 3/4 #{b#} 2) (timed-note 3/4 #{d'#} 2) (timed-note 3/4 #{g'#} 2)))))
     )))

 (test-case "pitches <voices> <integer>"
   (let ((input
          (make <voices>
            #:voice-count 4
            #:moments (list->vector (map ly:make-moment '(0 1/4 3/4)))
            #:notes
            (list->vector
             (list
              (timed-note 0 #{c#} 2) (timed-note 0 #{c'#} 2) (timed-note 0 #{e'#} 2) (timed-note 0 #{g'#} 2)
              (timed-note 1/4 #{g,#} 1) (timed-note 1/4 #{b#} 1) (timed-note 1/4 #{d'#} 1) (timed-note 1/4 #{g'#} 1)
              (timed-note 3/4 #{g,#} 2) #f #f (timed-note 3/4 #{g'#} 2)))))
         (tests (list
                 (cons 0 (list #{c#} #{c'#} #{e'#} #{g'#}))
                 (cons 1 (list #{g,#} #{b#} #{d'#} #{g'#}))
                 (cons 2 (list #{g,#} #{g'#})))))
     (for-each
      (lambda (test)
        (let* ((moment-index (car test))
               (want (cdr test))
               (got (pitches input moment-index)))
          (test-that equal? got want)))
      
      tests)))

 (test-case "major-pitch-alist"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (got (major-pitch-alist input))
             (want (cdr test)))
        (test-that eq? got want)))
    '((((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)) . #t)
      (((0 . 0) (1 . 0) (2 . 0) (4 . 0) (3 . 0) (5 . 0) (6 . 0)) . #t)
      (((0 . 1/2) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)) . #f)
      (((0 . 0) (1 . -1) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)) . #f)
      (((0 . 0) (1 . 0) (2 . 1/2) (3 . 0) (4 . 0) (5 . 0) (6 . 0)) . #f)
      (((0 . 0) (1 . 0) (2 . 0) (3 . 1) (4 . 0) (5 . 0) (6 . 0)) . #f)
      (((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 1/2) (5 . 0) (6 . 0)) . #f)
      (((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 1/2) (6 . 0)) . #f)
      (((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 1)) . #f)
      (((0 . 0) (1 . 0) (2 . -1/2) (3 . 0) (4 . 0) (5 . -1/2) (6 . -1/2)) . #f))))

 (test-case "minor-pitch-alist"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (got (minor-pitch-alist input))
             (want (cdr test)))
        (test-that eq? got want)))
    '((((0 . 0) (1 . 0) (2 . -1/2) (3 . 0) (4 . 0) (5 . -1/2) (6 . -1/2)) . #t)
      (((5 . -1/2) (0 . 0) (1 . 0) (3 . 0) (4 . 0) (2 . -1/2) (6 . -1/2)) . #t)
      (((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0) (6 . 0)) . #f))))

 (test-case "key-pitch-alist"
   (for-each
    (lambda (test)
      (test-case (car test)
        (let ((key (let* ((parser (ly:parser-clone '() '()))
                          (key (ly:parse-string-expression parser (car test) "?" 0)))
                     (if (ly:parser-has-error? parser)
                         (error "failed to parse ~S" (car test)))
                     key)))
          (test-that (lambda (got want?) (want? got))
                     (key-pitch-alist key)
                     (cdr test)))))
    (list (cons "\\key c \\major" major-pitch-alist)
          (cons "\\key c \\minor" minor-pitch-alist)
          (cons "\\key cis \\major" major-pitch-alist)
          (cons "\\key cis \\minor" minor-pitch-alist)
          (cons "\\key ces \\major" major-pitch-alist)
          (cons "\\key ces \\minor" minor-pitch-alist)
          (cons "\\key d \\major" major-pitch-alist)
          (cons "\\key d \\minor" minor-pitch-alist)
          (cons "\\key dis \\major" major-pitch-alist)
          (cons "\\key dis \\minor" minor-pitch-alist)
          (cons "\\key des \\major" major-pitch-alist)
          (cons "\\key des \\minor" minor-pitch-alist)
          (cons "\\key e \\major" major-pitch-alist)
          (cons "\\key e \\minor" minor-pitch-alist)
          (cons "\\key eis \\major" major-pitch-alist)
          (cons "\\key eis \\minor" minor-pitch-alist)
          (cons "\\key ees \\major" major-pitch-alist)
          (cons "\\key ees \\minor" minor-pitch-alist)
          (cons "\\key f \\major" major-pitch-alist)
          (cons "\\key f \\minor" minor-pitch-alist)
          (cons "\\key fis \\major" major-pitch-alist)
          (cons "\\key fis \\minor" minor-pitch-alist)
          (cons "\\key fes \\major" major-pitch-alist)
          (cons "\\key fes \\minor" minor-pitch-alist)
          (cons "\\key g \\major" major-pitch-alist)
          (cons "\\key g \\minor" minor-pitch-alist)
          (cons "\\key gis \\major" major-pitch-alist)
          (cons "\\key gis \\minor" minor-pitch-alist)
          (cons "\\key ges \\major" major-pitch-alist)
          (cons "\\key ges \\minor" minor-pitch-alist)
          (cons "\\key a \\major" major-pitch-alist)
          (cons "\\key a \\minor" minor-pitch-alist)
          (cons "\\key ais \\major" major-pitch-alist)
          (cons "\\key ais \\minor" minor-pitch-alist)
          (cons "\\key aes \\major" major-pitch-alist)
          (cons "\\key aes \\minor" minor-pitch-alist)
          (cons "\\key b \\major" major-pitch-alist)
          (cons "\\key b \\minor" minor-pitch-alist)
          (cons "\\key bis \\major" major-pitch-alist)
          (cons "\\key bis \\minor" minor-pitch-alist)
          (cons "\\key bes \\major" major-pitch-alist)
          (cons "\\key bes \\minor" minor-pitch-alist))))

 (test-case "parallel-consonance"
   (for-each
    (lambda (test)
      (let* ((m (caar test))
             (itv (cdar test))
             (want (cdr test))
             (got (parallel-consonance (get-voices (events! (ly:music-deep-copy m)))
                                       (make <interval> #:number itv #:quality 'perfect))))
        (add-score (scorify-music (ly:music-deep-copy m)))
        (let ((lines (string-split (test-pretty-print got "    ") #\newline)))
          (for-each
           (lambda (line)
             (add-text #{ \markup { \column { \wordwrap { $line } } } #}))
           lines))
        (test-that equal? got want)))
    (list (cons (cons mFive 5)
                (list (make <parallel-consonance>
                        #:moment-index 4
                        #:voice1 0 #:voice2 3
                        #:interval (make <interval> #:number 5 #:quality 'perfect)
                        #:kind 'open)
                      (make <parallel-consonance>
                        #:moment-index 4
                        #:voice1 1 #:voice2 3
                        #:interval (make <interval> #:number 5 #:quality 'perfect)
                        #:kind 'open)
                      (make <parallel-consonance>
                        #:moment-index 6
                        #:voice1 0 #:voice2 3
                        #:interval (make <interval> #:number 5 #:quality 'perfect)
                        #:kind 'hidden)))
          (cons (cons mEight 5) '())
          (cons (cons mNine 5) '())
          (cons (cons mTen 8) '())
          (cons (cons mEleven 8) '())
          (cons (cons mTwelve 5) '())
          (cons (cons mThirteen 8) '())
          (cons (cons #{#} 5) '()))))

 (test-case "augmented-second"
   (for-each
    (lambda (test)
      (let* ((m (car  test))
             (want (cdr test))
             (got (augmented-second (get-voices (events! (ly:music-deep-copy m))))))
        (add-score (scorify-music (ly:music-deep-copy m)))
        (let ((lines (string-split (test-pretty-print got "    ") #\newline)))
          (for-each
           (lambda (line)
             (add-text #{ \markup { \column { \wordwrap { $line } } } #}))
           lines))
        (test-that equal? got want)))
    (list (cons mSeven (list (make <augmented-second>
                               #:moment-index 3
                               #:voice 3))))))

 (test-case "unique"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (less (cadr test))
             (want (caddr test))
             (got (unique less input)))
        (test-that equal? got want)))
    (list (list '() < '())
          (list '(1 2 3) < '(1 2 3))
          (list '(1 2 1 3 2) < '(1 2 3))
          (list '("x" "z" "y" "x") string<? '("x" "y" "z")))))

 (test-case "figures?"
   (let ()
     (define (first-element music)
       (test-that eq? (ly:music-property music 'name) 'SequentialMusic)
       (car (ly:music-property music 'elements)))
     (for-each
      (lambda (test)
        (let* ((input (first-element (car test)))
               (want (cdr test))
               (got (figures? input)))
          (test-that equal? got want)))
      (list (cons #{\figuremode { <5> }#} #t)
            (cons #{{<e a g>}#} #f)))))

 (test-case "< <interval> <interval>"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (< (car input) (cdr input))))
        (test-that equal? got want)))
    
    (list
     ;; Same number but quality in 'perfect, 'augmented and 'diminished.
     (cons (cons (make <interval> #:number 1 #:quality 'perfect)
                 (make <interval> #:number 1 #:quality 'perfect))
           #f)
     (cons (cons (make <interval> #:number 1 #:quality 'perfect)
                 (make <interval> #:number 1 #:quality 'augmented))
           #t)
     (cons (cons (make <interval> #:number 1 #:quality 'augmented)
                 (make <interval> #:number 1 #:quality 'perfect))
           #f)
     (cons (cons (make <interval> #:number 1 #:quality 'perfect)
                 (make <interval> #:number 1 #:quality 'diminished))
           #f)
     (cons (cons (make <interval> #:number 1 #:quality 'diminished)
                 (make <interval> #:number 1 #:quality 'perfect))
           #t)
     ;; Same number but different quality in 'minor, 'major, 'augmented, 'diminished.
     (cons (cons (make <interval> #:number 3 #:quality 'diminished)
                 (make <interval> #:number 3 #:quality 'diminished))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'diminished)
                 (make <interval> #:number 3 #:quality 'minor))
           #t)
     (cons (cons (make <interval> #:number 3 #:quality 'diminished)
                 (make <interval> #:number 3 #:quality 'major))
           #t)
     (cons (cons (make <interval> #:number 3 #:quality 'diminished)
                 (make <interval> #:number 3 #:quality 'augmented))
           #t)
     (cons (cons (make <interval> #:number 3 #:quality 'minor)
                 (make <interval> #:number 3 #:quality 'diminished))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'minor)
                 (make <interval> #:number 3 #:quality 'minor))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'minor)
                 (make <interval> #:number 3 #:quality 'major))
           #t)
     (cons (cons (make <interval> #:number 3 #:quality 'minor)
                 (make <interval> #:number 3 #:quality 'augmented))
           #t)
     (cons (cons (make <interval> #:number 3 #:quality 'major)
                 (make <interval> #:number 3 #:quality 'diminished))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'major)
                 (make <interval> #:number 3 #:quality 'minor))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'major)
                 (make <interval> #:number 3 #:quality 'major))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'major)
                 (make <interval> #:number 3 #:quality 'augmented))
           #t)
     (cons (cons (make <interval> #:number 3 #:quality 'augmented)
                 (make <interval> #:number 3 #:quality 'diminished))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'augmented)
                 (make <interval> #:number 3 #:quality 'minor))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'augmented)
                 (make <interval> #:number 3 #:quality 'major))
           #f)
     (cons (cons (make <interval> #:number 3 #:quality 'augmented)
                 (make <interval> #:number 3 #:quality 'augmented))
           #f)
     ;; Different number
     (cons (cons (make <interval> #:number 1 #:quality 'perfect)
                 (make <interval> #:number 5 #:quality 'perfect))
           #t)
     (cons (cons (make <interval> #:number 1 #:quality 'perfect)
                 (make <interval> #:number 5 #:quality 'augmented))
           #t)
     (cons (cons (make <interval> #:number 1 #:quality 'augmented)
                 (make <interval> #:number 5 #:quality 'perfect))
           #t)
     (cons (cons (make <interval> #:number 1 #:quality 'perfect)
                 (make <interval> #:number 5 #:quality 'diminished))
           #t)
     (cons (cons (make <interval> #:number 1 #:quality 'diminished)
                 (make <interval> #:number 5 #:quality 'perfect))
           #t)
     (cons (cons (make <interval> #:number 5 #:quality 'perfect)
                 (make <interval> #:number 1 #:quality 'perfect))
           #f)
     (cons (cons (make <interval> #:number 5 #:quality 'perfect)
                 (make <interval> #:number 1 #:quality 'augmented))
           #f)
     (cons (cons (make <interval> #:number 5 #:quality 'augmented)
                 (make <interval> #:number 1 #:quality 'perfect))
           #f)
     (cons (cons (make <interval> #:number 5 #:quality 'perfect)
                 (make <interval> #:number 1 #:quality 'diminished))
           #f)
     (cons (cons (make <interval> #:number 5 #:quality 'diminished)
                 (make <interval> #:number 1 #:quality 'perfect))
           #f))))
  
 (test-case "bass"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (bass input)))
        (test-that equal? got want)))
    (list (cons '() #f)
          (cons (list #{d,#}) #{d,#})
          (cons (list #{d,#} #{e#} #{a#}) #{d,#}))))
		    

 (test-case "chord pitches"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (chord input)))
        (test-that equal? got want)))
    (let ((perfect-fifth (make <interval> #:number 5 #:quality 'perfect))
          (augmented-fifth (make <interval> #:number 5 #:quality 'augmented))
          (diminished-fifth (make <interval> #:number 5 #:quality 'diminished))
          (major-third (make <interval> #:number 3 #:quality 'major))
          (minor-third (make <interval> #:number 3 #:quality 'minor))
          (major-seventh (make <interval> #:number 7 #:quality 'major))
          (minor-seventh (make <interval> #:number 7 #:quality 'minor))
          (major-ninth (make <interval> #:number 9 #:quality 'major))
          (minor-ninth (make <interval> #:number 9 #:quality 'minor))
          (diminished-seventh (make <interval> #:number 7 #:quality 'diminished)))
      (list
       (cons '() #f)
       (cons (event-chord-pitches #{<c g>#})
             (make <chord> 
               #:tonic #{c#} 
               #:fifth perfect-fifth 
               #:inversion 'root))
       (cons (event-chord-pitches #{<e, c e c'>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:inversion 'first))
       (cons (event-chord-pitches #{<c c' c,,>#})
             #f)
       (cons (event-chord-pitches #{<c e g>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth perfect-fifth 
               #:inversion 'root))
       (cons (event-chord-pitches #{<e g c'>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth perfect-fifth 
               #:inversion 'first))
       (cons (event-chord-pitches #{<g c' e'>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth perfect-fifth 
               #:inversion 'second))
       (cons (event-chord-pitches #{<c, e, g,>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth perfect-fifth 
               #:inversion 'root))
       (cons (event-chord-pitches #{<e,, g c'>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth perfect-fifth 
               #:inversion 'first))
       (cons (event-chord-pitches #{<c ees g>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third minor-third 
               #:fifth perfect-fifth 
               #:inversion 'root))
       (cons (event-chord-pitches #{<c ees ges>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third minor-third 
               #:fifth diminished-fifth 
               #:inversion 'root))
       (cons (event-chord-pitches #{<ees ges c'>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third minor-third 
               #:fifth diminished-fifth 
               #:inversion 'first))
       (cons (event-chord-pitches #{<e gis c'>#})
             (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth augmented-fifth 
               #:inversion 'first))
       (cons (event-chord-pitches #{<c e g b>#})
             (make <chord>
               #:tonic #{c#}
               #:third major-third 
               #:fifth perfect-fifth
               #:seventh major-seventh
               #:inversion 'root))
       (cons (event-chord-pitches #{<c ees ges b>#})
             (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth diminished-fifth
               #:seventh major-seventh
               #:inversion 'root))
       (cons (event-chord-pitches #{<c ees ges bes>#})
             (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth diminished-fifth
               #:seventh minor-seventh
               #:inversion 'root))
       (cons (event-chord-pitches #{<c ees ges beses>#})
             (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth diminished-fifth
               #:seventh diminished-seventh
               #:inversion 'root))
       (cons (event-chord-pitches #{<c ees ges beses,>#})
             (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth diminished-fifth
               #:seventh diminished-seventh
               #:inversion 'third))
       (cons (event-chord-pitches #{<c ees beses,>#})
             (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:seventh diminished-seventh
               #:inversion 'third))
       (cons (event-chord-pitches #{<c e g bes d>#})
             (make <chord>
               #:tonic #{c#}
               #:third major-third 
               #:fifth perfect-fifth
               #:seventh minor-seventh
               #:ninth major-ninth
               #:inversion 'root))
       (cons (event-chord-pitches #{<c e g bes des>#})
             (make <chord>
               #:tonic #{c#}
               #:third major-third 
               #:fifth perfect-fifth
               #:seventh minor-seventh
               #:ninth minor-ninth
               #:inversion 'root))
       (cons (event-chord-pitches #{<c e bes d>#})
             (make <chord>
               #:tonic #{c#}
               #:third major-third 
               #:seventh minor-seventh
               #:ninth major-ninth
               #:inversion 'root))
       (cons (event-chord-pitches #{<c e bes des>#})
             (make <chord>
               #:tonic #{c#}
               #:third major-third 
               #:seventh minor-seventh
               #:ninth minor-ninth
               #:inversion 'root))
       ))))

 (test-case "type <chord>"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (type input)))
        (test-that equal? got want)))
    (let ((perfect-fifth (make <interval> #:number 5 #:quality 'perfect))
          (augmented-fifth (make <interval> #:number 5 #:quality 'augmented))
          (diminished-fifth (make <interval> #:number 5 #:quality 'diminished))
          (major-third (make <interval> #:number 3 #:quality 'major))
          (minor-third (make <interval> #:number 3 #:quality 'minor))
          (major-seventh (make <interval> #:number 7 #:quality 'major))
          (minor-seventh (make <interval> #:number 7 #:quality 'minor))
          (diminished-seventh (make <interval> #:number 7 #:quality 'diminished))
          (augmented-seventh (make <interval> #:number 7 #:quality 'augmented))
          (major-ninth (make <interval> #:number 9 #:quality 'major))
          (minor-ninth (make <interval> #:number 9 #:quality 'minor)))
      (list
       ;; Three notes.
       (cons (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth perfect-fifth 
               #:inversion 'root)
             'major)
       (cons (make <chord> 
               #:tonic #{c#} 
               #:third major-third 
               #:fifth augmented-fifth
               #:inversion 'root)
             'augmented)
       (cons (make <chord> 
               #:tonic #{c#} 
               #:third minor-third 
               #:fifth perfect-fifth 
               #:inversion 'root)
             'minor)
       (cons (make <chord> 
               #:tonic #{c#} 
               #:third minor-third 
               #:fifth diminished-fifth
               #:inversion 'root)
             'diminished)
       (cons (make <chord> 
               #:tonic #{c#} 
               #:third minor-third 
               #:fifth diminished-fifth
               #:inversion 'root)
             'diminished)
       ;; Four notes.
       (cons (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth diminished-fifth
               #:seventh diminished-seventh
               #:inversion 'third)
             'diminished-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth perfect-fifth
               #:seventh minor-seventh
               #:inversion 'third)
             'minor-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third major-third
               #:fifth perfect-fifth
               #:seventh minor-seventh
               #:inversion 'third)
             'dominant-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third major-third
               #:fifth perfect-fifth
               #:seventh major-seventh
               #:inversion 'third)
             'major-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth perfect-fifth
               #:seventh major-seventh
               #:inversion 'third)
             'minor-major-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third major-third
               #:fifth augmented-fifth
               #:seventh augmented-seventh
               #:inversion 'third)
             'augmented-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third minor-third 
               #:fifth diminished-fifth
               #:seventh minor-seventh
               #:inversion 'third)
             'half-diminished-seventh)
       (cons (make <chord>
               #:tonic #{c#}
               #:third major-third
               #:fifth perfect-fifth
               #:seventh minor-seventh
               #:ninth major-ninth
               #:inversion 'root)
             'major-ninth)
       (cons (make <chord>
               #:tonic #{c#}
               #:third major-third
               #:fifth perfect-fifth
               #:seventh minor-seventh
               #:ninth minor-ninth
               #:inversion 'root)
             'minor-ninth)))))

 (test-case "semi-tones <interval>"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (semi-tones input)))
        (test-that equal? got want)))
    (list
     ;; Unisson and octaves.
     (cons (make <interval> #:number 1 #:quality 'perfect) 0)
     (cons (make <interval> #:number 1 #:quality 'augmented) 1)
     (cons (make <interval> #:number 8 #:quality 'diminished) 11)
     (cons (make <interval> #:number 8 #:quality 'perfect) 12)
     (cons (make <interval> #:number 8 #:quality 'augmented) 13)
     (cons (make <interval> #:number 15 #:quality 'diminished) 23)
     (cons (make <interval> #:number 15 #:quality 'perfect) 24)
     (cons (make <interval> #:number 15 #:quality 'augmented) 25)
     (cons (make <interval> #:number 22 #:quality 'diminished) 35)
     (cons (make <interval> #:number 22 #:quality 'perfect) 36)
     (cons (make <interval> #:number 22 #:quality 'augmented) 37)
     ;; Fourths.
     (cons (make <interval> #:number 4 #:quality 'diminished) 4)
     (cons (make <interval> #:number 4 #:quality 'perfect) 5)
     (cons (make <interval> #:number 4 #:quality 'augmented) 6)
     (cons (make <interval> #:number 11 #:quality 'diminished) 16)
     (cons (make <interval> #:number 11 #:quality 'perfect) 17)
     (cons (make <interval> #:number 11 #:quality 'augmented) 18)
     ;; Fifths.
     (cons (make <interval> #:number 5 #:quality 'diminished) 6)
     (cons (make <interval> #:number 5 #:quality 'perfect) 7)
     (cons (make <interval> #:number 5 #:quality 'augmented) 8)
     (cons (make <interval> #:number 12 #:quality 'diminished) 18)
     (cons (make <interval> #:number 12 #:quality 'perfect) 19)
     (cons (make <interval> #:number 12 #:quality 'augmented) 20)
     ;; Seconds.
     (cons (make <interval> #:number 2 #:quality 'diminished) 0)
     (cons (make <interval> #:number 2 #:quality 'minor) 1)
     (cons (make <interval> #:number 2 #:quality 'major) 2)
     (cons (make <interval> #:number 2 #:quality 'augmented) 3)
     (cons (make <interval> #:number 9 #:quality 'diminished) 12)
     (cons (make <interval> #:number 9 #:quality 'minor) 13)
     (cons (make <interval> #:number 9 #:quality 'major) 14)
     (cons (make <interval> #:number 9 #:quality 'augmented) 15)
     ;; Thirds.
     (cons (make <interval> #:number 3 #:quality 'diminished) 2)
     (cons (make <interval> #:number 3 #:quality 'minor) 3)
     (cons (make <interval> #:number 3 #:quality 'major) 4)
     (cons (make <interval> #:number 3 #:quality 'augmented) 5)
     (cons (make <interval> #:number 17 #:quality 'diminished) 26)
     (cons (make <interval> #:number 17 #:quality 'minor) 27)
     (cons (make <interval> #:number 17 #:quality 'major) 28)
     (cons (make <interval> #:number 17 #:quality 'augmented) 29)
     ;; Sixths.
     (cons (make <interval> #:number 6 #:quality 'diminished) 7)
     (cons (make <interval> #:number 6 #:quality 'minor) 8)
     (cons (make <interval> #:number 6 #:quality 'major) 9)
     (cons (make <interval> #:number 6 #:quality 'augmented) 10)
     (cons (make <interval> #:number 13 #:quality 'diminished) 19)
     (cons (make <interval> #:number 13 #:quality 'minor) 20)
     (cons (make <interval> #:number 13 #:quality 'major) 21)
     (cons (make <interval> #:number 13 #:quality 'augmented) 22)
     ;; Sevenths.
     (cons (make <interval> #:number 7 #:quality 'diminished) 9)
     (cons (make <interval> #:number 7 #:quality 'minor) 10)
     (cons (make <interval> #:number 7 #:quality 'major) 11)
     (cons (make <interval> #:number 7 #:quality 'augmented) 12)
     (cons (make <interval> #:number 14 #:quality 'diminished) 21)
     (cons (make <interval> #:number 14 #:quality 'minor) 22)
     (cons (make <interval> #:number 14 #:quality 'major) 23)
     (cons (make <interval> #:number 14 #:quality 'augmented) 24))))

 (test-case "ascending <interval> pitch"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (ascending (car input) (cdr input))))
        (test-that equal? got want)))
    (list
     ;; Unisson and octaves.
     (cons (cons (make <interval> #:number 1 #:quality 'perfect) #{cis#})
           #{cis#})
     (cons (cons (make <interval> #:number 1 #:quality 'perfect) #{d#})
           #{d#})
     (cons (cons (make <interval> #:number 1 #:quality 'perfect) #{fes#})
           #{fes#})
     (cons (cons (make <interval> #:number 1 #:quality 'perfect) #{cis'#})
           #{cis'#})
     (cons (cons (make <interval> #:number 8 #:quality 'perfect) #{cis'#})
           #{cis''#})
     (cons (cons (make <interval> #:number 15 #:quality 'perfect) #{cis,#})
           #{cis'#})
     (cons (cons (make <interval> #:number 15 #:quality 'perfect) #{ces,#})
           #{ces'#})
     (cons (cons (make <interval> #:number 15 #:quality 'augmented) #{ces,#})
           #{c'#})
     (cons (cons (make <interval> #:number 15 #:quality 'augmented) #{c,#})
           #{cis'#})
     (cons (cons (make <interval> #:number 15 #:quality 'augmented) #{cis,#})
           #{cisis'#})
     ;; Thirds.
     (cons (cons (make <interval> #:number 3 #:quality 'major) #{gis'#})
           #{bis'#}))))


 (test-case "music <chord> duration"
   (let ((musics '()))
     (for-each
      (lambda (test)
        (let* ((input (car test))
               (want (cdr test))
               (got (music (car input) (cdr input))))
          (set! musics (cons got musics))
          (set! musics (cons want musics))
          (test-that equal? got want)))
      (let ((perfect-fifth (make <interval> #:number 5 #:quality 'perfect))
            (augmented-fifth (make <interval> #:number 5 #:quality 'augmented))
            (diminished-fifth (make <interval> #:number 5 #:quality 'diminished))
            (major-third (make <interval> #:number 3 #:quality 'major))
            (minor-third (make <interval> #:number 3 #:quality 'minor))
            (major-seventh (make <interval> #:number 7 #:quality 'major))
            (minor-seventh (make <interval> #:number 7 #:quality 'minor))
            (diminished-seventh (make <interval> #:number 7 #:quality 'diminished))
            (augmented-seventh (make <interval> #:number 7 #:quality 'augmented))
            (major-ninth (make <interval> #:number 9 #:quality 'major))
            (minor-ninth (make <interval> #:number 9 #:quality 'minor))
            (duration (ly:make-duration 1)))
        (define (first-chord music)
          (car (extract-named-music music 'EventChord)))
        (list
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:inversion 'root)
                     duration)
               (first-chord #{\chordmode { c2 }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:inversion 'first)
                     duration)
               (first-chord #{\chordmode { c2/e }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:inversion 'second)
                     duration)
               (first-chord #{\chordmode { c2/g }#}))
         (cons (cons (make <chord> 
                       #:tonic #{bes#} 
                       #:third minor-third 
                       #:fifth diminished-fifth 
                       #:inversion 'root)
                     duration)
               (first-chord #{\chordmode { bes2:dim }#}))
         (cons (cons (make <chord> 
                       #:tonic #{bes#} 
                       #:third minor-third 
                       #:fifth diminished-fifth 
                       #:inversion 'first)
                     duration)
               (first-chord #{\chordmode { bes2:dim/des }#}))
         (cons (cons (make <chord> 
                       #:tonic #{bes#} 
                       #:third minor-third 
                       #:fifth diminished-fifth 
                       #:inversion 'second)
                     duration)
               (first-chord #{\chordmode { bes2:dim/fes }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth
                       #:seventh major-seventh
                       #:inversion 'root)
                     duration)
               (first-chord #{\chordmode { c2:maj7 }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:seventh major-seventh
                       #:inversion 'first)
                     duration)
               (first-chord #{\chordmode { c2:maj7/e }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:seventh major-seventh
                       #:inversion 'second)
                     duration)
               (first-chord #{\chordmode { c2:maj7/g }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:seventh major-seventh
                       #:inversion 'third)
                     duration)
               (first-chord #{\chordmode { c2:maj7/b }#}))
         (cons (cons (make <chord> 
                       #:tonic #{c#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:seventh minor-seventh
                       #:inversion 'third)
                     duration)
               (first-chord #{\chordmode { c2:7/bes }#}))
         (cons (cons (make <chord> 
                       #:tonic #{g#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:seventh minor-seventh
                       #:ninth major-ninth
                       #:inversion 'root)
                     duration)
               (first-chord #{\chordmode { g2:9 }#}))
         (cons (cons (make <chord> 
                       #:tonic #{e#} 
                       #:third major-third 
                       #:fifth perfect-fifth 
                       #:seventh minor-seventh
                       #:ninth minor-ninth
                       #:inversion 'root)
                     duration)
               (first-chord #{\chordmode { e2:7.9- }#})))))
     (let ((chords (make-sequential-music (reverse musics))))
       (add-score (scorify-music
                   (ly:music-deep-copy
                    (make-simultaneous-music
                     (list 
                      (make-music
                       'ContextSpeccedMusic
                       'create-new #t
                       'property-operations '()
                       'context-type 'ChordNames
                       'element (ly:music-deep-copy chords))
                      chords))))))))

 (test-case "key-pitches"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (music-pitches (cdr test)))
             (got (key-pitches input)))
        (test-that equal? got want)))
    (list (cons (key #{c#} major) #{c d e f g a b#})
          (cons (key #{c#} minor) #{c d ees f g aes bes#})
          (cons (key #{g#} major) #{g a b c d e fis#})
          (cons (key #{g#} minor) #{g a bes c d ees f#})
          (cons (key #{cis#} major) #{cis dis eis fis gis ais bis#})
          (cons (key #{cis#} minor) #{cis dis e fis gis a b#}))))

 (test-case "chord key bass figures"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (apply chord input)))
        (test-that equal? got want)))
    (let ((perfect-fifth (make <interval> #:number 5 #:quality 'perfect))
          (augmented-fifth (make <interval> #:number 5 #:quality 'augmented))
          (diminished-fifth (make <interval> #:number 5 #:quality 'diminished))
          (augmented-third (make <interval> #:number 3 #:quality 'augmented))
          (major-third (make <interval> #:number 3 #:quality 'major))
          (minor-third (make <interval> #:number 3 #:quality 'minor))
          (major-seventh (make <interval> #:number 7 #:quality 'major))
          (minor-seventh (make <interval> #:number 7 #:quality 'minor))
          (diminished-seventh (make <interval> #:number 7 #:quality 'diminished))
          (augmented-seventh (make <interval> #:number 7 #:quality 'augmented))
          (major-ninth (make <interval> #:number 9 #:quality 'major))
          (minor-ninth (make <interval> #:number 9 #:quality 'minor)))
      (define (first-element m) (car (ly:music-property m 'elements)))
      (list (cons (list (key #{c#} major) #{c#} #f)
                  (make <chord>
                    #:tonic #{c#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:inversion 'root))
            (cons (list (key #{c'#} major) #{c#} (first-element #{ \figuremode { <5> } #}))
                  (make <chord>
                    #:tonic #{c#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:inversion 'root))
            (cons (list (key #{f#} major) #{f#} (first-element #{ \figuremode { <5> } #}))
                  (make <chord>
                    #:tonic #{f#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:inversion 'root))
            (cons (list (key #{f#} major) #{f#} (first-element #{ \figuremode { <5 3> } #}))
                  (make <chord>
                    #:tonic #{f#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:inversion 'root))
            (cons (list (key #{f#} major) #{a#} (first-element #{ \figuremode { <6> } #}))
                  (make <chord>
                    #:tonic #{f#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:inversion 'first))
            (cons (list (key #{f#} major) #{c#} (first-element #{ \figuremode { <6 4> } #}))
                  (make <chord>
                    #:tonic #{f#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:inversion 'second))
            (cons (list (key #{f#} major) #{f#} (first-element #{ \figuremode { <7> } #}))
                  (make <chord>
                    #:tonic #{f#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:seventh major-seventh
                    #:inversion 'root))
            (cons (list (key #{c#} major) #{b#} (first-element #{ \figuremode { <7> } #}))
                  (make <chord>
                    #:tonic #{b#}
                    #:third minor-third
                    #:fifth diminished-fifth
                    #:seventh minor-seventh
                    #:inversion 'root))
            (cons (list (key #{c#} major) #{d#} (first-element #{ \figuremode { <6 5> } #}))
                  (make <chord>
                    #:tonic #{b#}
                    #:third minor-third
                    #:fifth diminished-fifth
                    #:seventh minor-seventh
                    #:inversion 'first))
            (cons (list (key #{c#} major) #{f#} (first-element #{ \figuremode { <4 3> } #}))
                  (make <chord>
                    #:tonic #{b#}
                    #:third minor-third
                    #:fifth diminished-fifth
                    #:seventh minor-seventh
                    #:inversion 'second))
            (cons (list (key #{c#} major) #{a#} (first-element #{ \figuremode { <2> } #}))
                  (make <chord>
                    #:tonic #{b#}
                    #:third minor-third
                    #:fifth diminished-fifth
                    #:seventh minor-seventh
                    #:inversion 'third))
            (cons (list (key #{c#} major) #{a#} (first-element #{ \figuremode { <4+ 2> } #}))
                  (make <chord>
                    #:tonic #{b#}
                    #:third major-third
                    #:fifth diminished-fifth
                    #:seventh minor-seventh
                    #:inversion 'third))
            (cons (list (key #{g#} major) #{e#} (first-element #{ \figuremode { <4+ 2!> } #}))
                  (make <chord>
                    #:tonic #{f#}
                    #:third augmented-third
                    #:fifth perfect-fifth
                    #:seventh major-seventh
                    #:inversion 'third))
            (cons (list (key #{c'#} major) #{c#} (first-element #{ \figuremode { <_-> } #}))
                  (make <chord>
                    #:tonic #{c#}
                    #:third minor-third
                    #:fifth perfect-fifth
                    #:inversion 'root))
            (cons (list (key #{c#} minor) #{d#} (first-element #{ \figuremode { <6! 4 3> } #}))
                  (make <chord>
                    #:tonic #{g#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:seventh minor-seventh
                    #:inversion 'second))
            (cons (list (key #{c#} major) #{g#} (first-element #{ \figuremode { <9> } #}))
                  (make <chord>
                    #:tonic #{g#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:seventh minor-seventh
                    #:ninth major-ninth
                    #:inversion 'root))
            (cons (list (key #{c#} minor) #{g#} (first-element #{ \figuremode { <9> } #}))
                  (make <chord>
                    #:tonic #{g#}
                    #:third minor-third
                    #:fifth perfect-fifth
                    #:seventh minor-seventh
                    #:ninth minor-ninth
                    #:inversion 'root))
            (cons (list (key #{c#} minor) #{g#} (first-element #{ \figuremode { <9 _!> } #}))
                  (make <chord>
                    #:tonic #{g#}
                    #:third major-third
                    #:fifth perfect-fifth
                    #:seventh minor-seventh
                    #:ninth minor-ninth
                    #:inversion 'root))
            ))))

 (test-case "active?"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (apply active? input)))
        (test-that equal? got want)))
    (let ()
      (define (make-finite start duration)
        (make <finite-timed-music>
          #:moment (ly:make-moment start)
          #:music (make-music 'Music 'duration (ly:make-duration duration))))
      (define (make-infinite start)
        (make <infinite-timed-music>
          #:moment (ly:make-moment start)
          #:music (make-music 'Music)))
      (list
       ;; A quarter note at 0.
       (cons (list (make-finite 0 2) (ly:make-moment -1/16)) #f)
       (cons (list (make-finite 0 2) (ly:make-moment 0)) #t)
       (cons (list (make-finite 0 2) (ly:make-moment 1/8)) #t)
       (cons (list (make-finite 0 2) (ly:make-moment 1/4)) #f)
       (cons (list (make-finite 0 2) (ly:make-moment 1)) #f)
       ;; A quarter note at 1/2.
       (cons (list (make-finite 1/2 2) (ly:make-moment -1/16)) #f)
       (cons (list (make-finite 1/2 2) (ly:make-moment 0)) #f)
       (cons (list (make-finite 1/2 2) (ly:make-moment 1/4)) #f)
       (cons (list (make-finite 1/2 2) (ly:make-moment 1/2)) #t)
       (cons (list (make-finite 1/2 2) (ly:make-moment 3/4)) #f)
       ;; An inifinite note at 1/2.
       (cons (list (make-infinite 1/2) (ly:make-moment -1/16)) #f)
       (cons (list (make-infinite 1/2) (ly:make-moment 0)) #f)
       (cons (list (make-infinite 1/2) (ly:make-moment 1/4)) #f)
       (cons (list (make-infinite 1/2) (ly:make-moment 1/2)) #t)
       (cons (list (make-infinite 1/2) (ly:make-moment 3/4)) #t)
       (cons (list (make-infinite 1/2) (ly:make-moment 2)) #t)))))

 (test-case "finite-timed-musics/time"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (apply finite-timed-musics/time input)))
        (test-that equal? got want)))
    (let ()
      (define (finite start duration)
        (make <finite-timed-music>
          #:moment (ly:make-moment start)
          #:music (make-music 'Music 'duration (ly:make-duration duration))))
      (define (moments . nums)
        (map ly:make-moment nums))
      (list
       (cons (list (list) (moments)) (vector))
       (cons (list (list) (moments 1 3 4)) (vector '() '() '()))
       (cons (list (list (finite 0 1)	; [0, 1/2)
                         (finite 1/2 1)	; [1/2, 1)
                         (finite 1/2 0)	; [1/2, 3/2)
                         (finite 1 0)	; [1, 2)
                         (finite 1 -1)	; [1 3)
                         (finite 1 -2)	; [1 5)
                         (finite 7/2 2)	; [3.5 3.75)
                         (finite 3 2)	; [3 3.25)
                         (finite 5 0))	; [4 6)
                   (moments 1 3 4))
             (vector (list (finite 1/2 0) (finite 1 0) (finite 1 -1) (finite 1 -2))
                     (list (finite 1 -2) (finite 3 2))
                     (list (finite 1 -2))))))))

 (test-case "infinite-timed-musics/time"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (apply infinite-timed-musics/time input)))
        (test-that equal? got want)))
    (let ()
      (define (infinite start)
        (make <infinite-timed-music>
          #:moment (ly:make-moment start)
          #:music (make-music 'Music)))
      (define (moments . nums)
        (map ly:make-moment nums))
      (list
       (cons (list (list) (moments)) (vector))
       (cons (list (list) (moments 1 3 4)) (vector #f #f #f))
       (cons (list (list (infinite 2)) (moments 1 3 4))
             (vector #f (infinite 2) (infinite 2)))
       (cons (list (list (infinite 0)
                         (infinite 1/2)
                         (infinite 1)
                         (infinite 7/2)	; 3.5
                         (infinite 3)
                         (infinite 5))
                   (moments 1 3 4))
             (vector (infinite 1)
                     (infinite 3)
                     (infinite 7/2)))))))

 (test-case "degree"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (apply degree input)))
        (test-that equal? got want)))
    (list (cons (list #{c#} #{c#}) 1)
          (cons (list #{c#} #{c'#}) 1)
          (cons (list #{c#} #{c,#}) 1)
          (cons (list #{g#} #{g#}) 1)
          (cons (list #{g#} #{g'#}) 1)
          (cons (list #{g#} #{g,#}) 1)
          (cons (list #{c#} #{d#}) 2)
          (cons (list #{c#} #{d'#}) 2)
          (cons (list #{c#} #{d,#}) 2)
          (cons (list #{c,#} #{d#}) 2)
          (cons (list #{c,#} #{d'#}) 2)
          (cons (list #{c,#} #{d,#}) 2)
          (cons (list #{c'#} #{d#}) 2)
          (cons (list #{c'#} #{d'#}) 2)
          (cons (list #{c'#} #{d,#}) 2)
          (cons (list #{g#} #{f#}) 7)
          (cons (list #{g#} #{d''#}) 5))))

 (test-case "best-duration-of-length"
   (for-each
    (lambda (test)
      (let* ((input (car test))
             (want (cdr test))
             (got (best-duration-of-length input)))
        (test-that equal? got want)))
    (let ()
      (define (moment-of-duration log dots frac)
        (ly:duration-length (ly:make-duration log dots frac)))
      (list (cons (ly:make-moment 1/4) (ly:make-duration 2))
            (cons (ly:make-moment (* 1/4 (+ 1 1/2))) (ly:make-duration 2 1))
            (cons (moment-of-duration 1 3 1/1) (ly:make-duration 1 3))
            ;; TODO: make (best-duration-of-length) support tuplets!
            (cons (moment-of-duration 2 0 2/3)
                  ;; Here we expect the fallback result of
                  ;; make-duration-of-length.
                  (ly:make-duration 0 0 (* 1/4 2/3))))))))
	    
