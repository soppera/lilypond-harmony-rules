\version "2.20.0"

\include "local-load-path.ly"

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

