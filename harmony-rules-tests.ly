\version "2.20.0"

\include "local-load-path.ly"

\header { tagline = ##f }

#(use-modules (harmony-rules-tests))

mus = \new PianoStaff <<
  \new Staff \relative {
    <c' e g>2 <d f a>4 <c g' c> |
    \key c \minor
    <c ees g>2 <d g bes> |
    <eis g b>1
  }
  \new Staff \relative {
    \clef "bass"
    c2 d4 e |
    g1 |
    g1 |
  }
  \figures {
    s2 s4 <6> |
    <6 4>2 s |
    <6>1 |
  }
>>

\include "harmony-rules.ly"

\markup { Interactive Test: }
\score {
  <<
    \new ChordNames \automaticChords \mus
    \checkHarmonyRules #'((figured-bass . #t)) \mus
    \new Staff \with { instrumentName = #"default" } { \automaticChords \mus }
    \new Staff \with { instrumentName = #"'notes" } { \automaticChords #'notes \mus }
    \new Staff \with { instrumentName = #"'figures" } { \automaticChords #'figures \mus }
  >>
}
% #(dump-all #{<c \tweak color #red e g>#})

\markup { \bold { Bugs: } }
%% FIXME: fix those bugs
\score {
  \header {
    piece = "the bass should be identified, not the tenor"
  }
  \checkHarmonyRules \new PianoStaff <<
    \new Staff \relative {
      <d' f b>2 <c e c'> |
    }
    \new Staff \relative {
      \clef "bass"
      g2 c | 
    }
  >>
}

