\version "2.20.0"

#(define (add-location-dir-to-load-path location)
  "Add the directory of the given location to the %load-path."
  (let* ((dir (dirname (car (ly:input-file-line-char-column location))))
         (abs-dir (if (is-absolute? dir)
                   dir
                   (string-append (ly-getcwd) "/" dir))))
   (if (defined? 'add-to-load-path)
    (add-to-load-path abs-dir)
    (set! %load-path (cons abs-dir %load-path)))))

#(add-location-dir-to-load-path
  ;; Lilypond v2.18.2 does not define *location* function.
  (if (defined? '*location*)
   (*location*)
   (ly:music-property #{a4#} 'origin)))
