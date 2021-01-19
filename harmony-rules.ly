\version "2.20.0"

%% Make sure we use relatives paths for the library.
#(define harmony-rules--initial-relative-includes
  (ly:get-option 'relative-includes))
#(ly:set-option 'relative-includes #t)

\include "local-load-path.ly"

%% Resets relative path mode to the including file behavior.
#(ly:set-option 'relative-includes
  harmony-rules--initial-relative-includes)

#(use-modules ((harmony-rules)
	       #:select (checkHarmonyRules
			 automaticChords
			 splitVoices)))

% m = \new PianoStaff <<
%   \new Staff \relative c' {
%     a'4
%     << { \voiceOne b2 }
%        \new Voice
%        { \voiceTwo <c e g>4 }
%      >> <d e>4 |
%   }
%   \new Staff \relative c {
%     \clef bass
%     a2 b4 c8 d8 |
%   }
% >>

% \score {
%   \checkHarmonyRules \m
%   \layout {}
% }

