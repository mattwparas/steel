(require "srfi/srfi-28/format.scm")

(provide display-color
         displayln-color)

(define (terminal-command command)
  (format "~a~a" #\u001B command))

(define (terminal-reset)
  (terminal-command "[0m"))

(define (terminal-colors bg fg bold? underline?)
  (terminal-command (format "[~a;~a~a~am"
                            (case bg
                              [(black) "40"]
                              [(red) "41"]
                              [(green) "42"]
                              [(yellow) "43"]
                              [(blue) "44"]
                              [(magenta) "45"]
                              [(cyan) "46"]
                              [(white) "47"]
                              [(default) "49"])
                            (case fg
                              [(black) "30"]
                              [(red) "31"]
                              [(green) "32"]
                              [(yellow) "33"]
                              [(blue) "34"]
                              [(magenta) "35"]
                              [(cyan) "36"]
                              [(white) "37"]
                              [(default) "39"])
                            (if bold? ";1" "")
                            (if underline? ";4" ""))))

(define (output-color output-method datum #:fg fg #:bg bg)
  (terminal-colors bg fg #f #f)
  (output-method datum)
  (display (terminal-reset)))

(define (display-color datum #:fg [fg 'default] #:bg [bg 'default])
  (output-color display datum #:fg fg #:bg bg))

(define (displayln-color datum #:fg [fg 'default] #:bg [bg 'default])
  (display (terminal-colors bg fg #f #f))
  (display datum)
  (display (terminal-reset))
  (newline))
