\version "2.20.0"

\include "local-load-path.ly"

\header { tagline = ##f }

#(use-modules (generate-doc))

#(let* ((basename (dirname (car (ly:input-file-line-char-column (*location*)))))
        (info-dir (string-append basename "/info")))
  (if (not (file-exists? info-dir))
   (mkdir info-dir))
  (write-texinfo info-dir))
