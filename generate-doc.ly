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

\include "local-load-path.ly"

\header { tagline = ##f }

#(use-modules (generate-doc))

#(let* ((basename (dirname (car (ly:input-file-line-char-column (*location*)))))
        (info-dir (string-append basename "/info")))
  (if (not (file-exists? info-dir))
   (mkdir info-dir))
  (write-texinfo info-dir))
