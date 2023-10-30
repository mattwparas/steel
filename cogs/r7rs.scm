;; Adapted from https://github.com/ashinn/chibi-scheme/blob/master/tests/r5rs-tests.scm
;; with the following copyright notice:
;;
;; -------------------------------------------------------------------
;;
;; Copyright (c) 2009-2021 Alex Shinn
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; -------------------------------------------------------------------
;;
;; Modified by Matt Paras for use within the Steel test suite

(require "tests/unit-test.scm"
         (for-syntax "tests/unit-test.scm"))

(set-test-mode!)

(require "lists/lists.scm")

;;;; Parameters

(define location (make-parameter "here"))

(check-equal? "Simple parameterize"
              "there"
              (parameterize ([location "there"])
                (location))) ;; "there"

(check-equal? "parameter keeps original value" "here" (location)) ;; "here"

(check-equal? "Parameter changes multiple times"
              (list "in a house" "with a mouse" "in a house")
              (parameterize ([location "in a house"])
                (list (location)
                      (parameterize ([location "with a mouse"])
                        (location))
                      (location)))) ;; '("in a house" "with a mouse" "in a house")

(check-equal? "parameter keeps original value after" "here" (location)) ;; "here"

(define (would-you-could-you?)
  (and (not (equal? (location) "here")) (not (equal? (location) "there"))))

(check-equal? "Parameters refer to the same location" #false (would-you-could-you?))

(check-equal? "Parameters refer to the same location, changed to be the same"
              #true
              (parameterize ([location "on a bus"])
                (would-you-could-you?)))

(define r7rs-test-stats (get-test-stats))

(displayln "Passed: " (hash-ref r7rs-test-stats 'success-count))
(displayln "Skipped compilation (expected failure): " (hash-ref r7rs-test-stats 'failed-to-compile))
(displayln "Failed: " (hash-ref r7rs-test-stats 'failure-count))

(for-each (lambda (x) (displayln "    > " x)) (hash-ref r7rs-test-stats 'failures))
