%% Lilypond Harmony Rules tests harmony rules of Lilypond scores.
%% Copyright (C) 2021  Stéphane SOPPERA
%% 
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU General Public License for more details.
%% 
%% You should have received a copy of the GNU General Public License
%% along with this program.  If not, see <https://www.gnu.org/licenses/>.

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

