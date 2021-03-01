#! /usr/bin/env scheme --script

;; Terminal code
;; Currently I have no idea how to do this from Chez, so meh...
;; save_state=$(stty -g)
;; stty raw
;; ...
;; stty "$save_state"
(define (disable-raw)
  (let ( (save-state "gfmt1:cflag=4b00:iflag=2b02:lflag=200005cb:oflag=3:discard=f:dsusp=19:eof=4:eol=ff:eol2=ff:erase=7f:intr=3:kill=15:lnext=16:min=1:quit=1c:reprint=12:start=11:status=14:stop=13:susp=1a:time=0:werase=17:ispeed=38400:ospeed=38400") )
    (system (string-append "stty " save-state)) ))
(define (enable-raw)
  (system "stty raw -echo")
  (let lop () (when (char-ready? stdin) (read-char) (lop)) ))


;; (lib misc) emulation, plus a few others
(define (dsp . l) (for-each display l))
(define (dspl . l) (for-each display l) (newline))

(define stdin (current-input-port))

;; Char functions
;; String functions


;; Data
(define ESC  "\x1b;")
(define ESEQ (str ESC "["))
(define LF   "\n")
(define CR   "\r")
(define (nlcr)            (str LF CR))
(define (cur-forward n)   (str ESEQ n "C" ))
(define (cur-down n)      (str ESEQ n "B" ))
(define (go-home)         (str ESEQ "H"   ))
(define (abs-adr l c)     (str ESEQ l ";" c "H"   ))
(define (clear-screen)    (str ESEQ "2J"  ))
(define (get-position)    (str ESEQ "6n"  ))
(define (hide-cursor)     (str ESEQ "?25l"))
(define (show-cursor)     (str ESEQ "?25h"))
(define (bot-rght)        (str (cur-forward 999) (cur-down 999)) )
(define (mov-cur-up)      (str ESEQ "A"))
(define (mov-cur-down)    (str ESEQ "B"))
(define (mov-cur-left)    (str ESEQ "D"))
(define (mov-cur-right)   (str ESEQ "C"))


;; State
(define-record-type estate
  (fields (mutable buf) (mutable screen-size) (mutable cursor-pos) (mutable nr) (mutable er)))
(define editor-state
  (let ( (state 0) (screen "") (rows 25) (cols 80) (cl 1) (cc 1)
	 (nr 0) (er "") )
    (case-lambda
      ((s)
       (cond
	((symbol=? s 'rows)   rows   )
	((symbol=? s 'cols)   cols   )
	((symbol=? s 'state)  state  )
	((symbol=? s 'cc)     cc     )
	((symbol=? s 'cl)     cl     )
	((symbol=? s 'er)     er     )
	((symbol=? s 'nr)     nr     )
	((symbol=? s 'screen) screen )
	((symbol=? s 'flush)  (set! screen "") (set! state 0))
	(else (exit-editor)  (error 'es "editor-state hates you") )) )
      ((s v)
       (cond
	((symbol=? s 'rows)   (set! rows v) )
	((symbol=? s 'cols)   (set! cols v) )
	((symbol=? s 'cc)     (set! cc v)   )
	((symbol=? s 'cl)     (set! cl v)   )
	((symbol=? s 'er)     (set! er v)   )
	((symbol=? s 'nr)     (set! nr v)   )
	((symbol=? s 'screen)
	 (set! screen (string-append screen v)) (set! state 1))
	(else (exit-editor)  (error 'es "editor-state hates you") )) )) ))



(define (get-cursor-position)
  (dsp (get-position))
  (let ( (r (split ";" (mirt "R" (trim ESEQ (read-stuff))))) )
    (cons (string->number (car r)) (string->number (cadr r))) ))

(define (get-window-size)
  (dsp (hide-cursor) (bot-rght))
  (let ( (p (get-cursor-position)) )
    (dsp (go-home) (show-cursor))
    p ))

(define (refresh-screen)  (str (clear-screen) (go-home)) )

(define (msg-at h v m)
  (let* ( (r (editor-state 'rows)) (c (editor-state 'cols))
	  (len (string-length m))
	  (p (abs-adr (exact (floor (/ r h)))
		      (exact (floor (/ (- c len) v))))) )
    (buf-append p m (go-home)) 
    ))

