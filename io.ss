#! /usr/bin/env scheme --script

;; (lib misc) emulation, plus a few others
(define (dsp . l) (for-each display l))
(define (dspl . l) (for-each display l) (newline))

(define stdin (current-input-port))

;; Char functions
(define (char->string c) (list->string `(,c)) )
(define ord char->integer)
(define chr integer->char)
(define (char-control? c)
  (if (char? c) (not (< 31 (ord c) 127))
      (error 'meh "char-control? hates you")) )
(define (ctrl-key c)
  (if (and (char? c) (char-lower-case? c))
      (chr (- (ord c) 96))
      (error 'meh "ctrl-key hates you")) )

;; String functions
(define (str . l) (with-output-to-string (lambda () (for-each display l))))
(define (string->char c) (car (string->list c)) )
(define (string-reverse s) (list->string (reverse (string->list s))) )
(define (string-empty? s)
  (if (string? s) (string=? s "") (error 'meh "string-empty? hates you")))
(define (prefix? p s) (string=? p (substring s 0 (string-length p))))
(define (right n s) (substring s n (string-length s)))
(define (left n s) (substring s 0 n) )
(define (trim p s) (if (prefix? p s) (right (string-length p) s) s))
(define (mirt p s) (string-reverse (trim (string-reverse p) (string-reverse s))))
(define (chop p s)
  (string-reverse (trim (string-reverse p) (string-reverse (trim p s)))) )

(define (collect p l r)
  (let ( (n (string-length p)) )
    (cond
     ((string=? r "") `(,l . "") )
     ((prefix? p r) `(,l . ,(right n r)))
     (else (collect p (string-append l (left n r)) (right n r)) )) ))
(define (split p s)
  (cond
   ((string=? s "") `() )
   (else
    (let ( (n (collect p "" s)) )
      (cons (car n) (split p (cdr n))) ) )) )


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

(define (get-cursor-position)
  (dsp (get-position))
  (let ( (r (split ";" (mirt "R" (trim ESEQ (read-stuff))))) )
    (cons (string->number (car r)) (string->number (cadr r))) ))

(define (get-window-size)
  (dsp (hide-cursor) (bot-rght))
  (let ( (p (get-cursor-position)) )
    (dsp (go-home) (show-cursor))
    p ))

(define (refresh-screen)  (string-append (clear-screen) (go-home)) )

;; Output   Change this to do line oriented drawing...  Somehow
(define (buf-append . s) (editor-state 'screen (apply str s)) )

(define (write-stuff)
  (dsp (hide-cursor) (editor-state 'screen) (show-cursor))
  (editor-state 'flush)
  (let ( (s (get-cursor-position)) )
    (editor-state `cl (car s))
    (editor-state `cc (cdr s)) )
  )

(define (print-char c)
  (buf-append (number->string (ord (string->char c))) " ('" c "')" (nlcr)) )

(define (draw-tildes)
  (let ( (e (- (editor-state 'rows) 1)) )
    (let lop ( (y 0) )
      (when (< y e) (buf-append "~" (nlcr)) (lop (+ y 1)) ) ))
  (buf-append "~" (go-home))
  )

(define (msg-at h v m)
  (let* ( (r (editor-state 'rows)) (c (editor-state 'cols))
	  (len (string-length m))
	  (p (abs-adr (exact (floor (/ r h)))
		      (exact (floor (/ (- c len) v))))) )
    (buf-append p m (go-home)) 
    ))

(define (move-cursor s)
  (let ( (l (editor-state 'cl))   (c (editor-state 'cc))
	 (h (editor-state 'rows)) (w (editor-state 'cols)) )
    (cond
     ((symbol=? s 'up   )
      (editor-state 'cl (if (> l 1) (- l 1) l)) (dsp (mov-cur-up)   ) )
     ((symbol=? s 'down )
      (editor-state 'cl (if (< l h) (+ l 1) l)) (dsp (mov-cur-down) ) )
     ((symbol=? s 'left )
      (editor-state 'cc (if (> c 1) (- c 1) c)) (dsp (mov-cur-left) ) )
     ((symbol=? s 'right)
      (editor-state 'cc (if (< c w) (+ c 1) c)) (dsp (mov-cur-right)) )
     (else (error 'meh "move-cursor hates you") )) )
  )


;; Input 
(define (read-stuff)
  (list->string
   (let lop ( (c (read-char)) )
     (cond
      ((not (char-ready? stdin)) `(,c) )
      (else (cons c (lop (read-char))) )) )) )


;; Handlers
(define (input-handler)  ;; delete: [3~    backspace: 127
  (let* ( (s (read-stuff)) (l (string-length s)) )
    (cond
     ((and (= l 1) (char=? (string->char s) (ctrl-key #\c))) #t )
     ((and (= l 1) (= (ord (string->char s)) 127))
      (move-cursor 'left) (dsp " " (mov-cur-left)) (editor-loop))
     ((string=? s "i") (print-char s) (editor-loop) )
     ((string=? s "c") (move-cursor 'up   ) (editor-loop) )
     ((string=? s "t") (move-cursor 'down ) (editor-loop) )
     ((string=? s "h") (move-cursor 'left ) (editor-loop) )
     ((string=? s "n") (move-cursor 'right) (editor-loop) )
     (else (editor-loop) )) ))


;; File IO
(define (open-stuff) #f)


;; Init
(define (init-editor)
  (editor-state `er "hello world")
  (editor-state `nr (string-length "hello world"))
  (enable-raw)
  (let ( (s (get-window-size)) )
    (editor-state `rows (car s))
    (editor-state `cols (cdr s)) )
  (buf-append (refresh-screen))
  (draw-tildes)
  (msg-at 3 2 "An editor -- version 0.0")
  )

(define (exit-editor)
  (buf-append (refresh-screen) "toodles" (nlcr))
  (write-stuff)
  (disable-raw)
  )


;; Main program
(define (editor-loop)
  (when (= (editor-state 'state) 1) (write-stuff))
  (if (char-ready? stdin)
      (input-handler)
      (editor-loop)) )

(define (prog)
  ;;(write (command-line)) (newline)
  ;;(write (command-line-arguments)) (newline)
  (init-editor) (editor-loop) (exit-editor) )
(prog)
