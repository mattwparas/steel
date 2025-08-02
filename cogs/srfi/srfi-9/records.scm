; Original Copyright:

; Copyright (C) Richard Kelsey (1999). All Rights Reserved.

; Permission is hereby granted, free of charge, to any person obtaining a copy of this software
; and associated documentation files (the "Software"), to deal in the Software without restriction,
; including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
; and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so,
; subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
; NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

; Definition of DEFINE-RECORD-TYPE

(define-syntax define-record-type
  (syntax-rules ()
    [(define-record-type type
                         (constructor constructor-tag ...)
                         predicate
                         (field-tag accessor . more) ...)
     (begin
       (define type (make-record-type 'type '(field-tag ...)))
       (define constructor (record-constructor type '(constructor-tag ...)))
       (define predicate (record-predicate type))
       (define-record-field type field-tag accessor . more) ...)]))

; An auxilliary macro for define field accessors and modifiers.
; This is needed only because modifiers are optional.

(define-syntax define-record-field
  (syntax-rules ()
    [(define-record-field type field-tag accessor)
     (define accessor (record-accessor type 'field-tag))]
    [(define-record-field type field-tag accessor modifier)
     (begin
       (define accessor (record-accessor type 'field-tag))
       (define modifier (record-modifier type 'field-tag)))]))

; We define the following procedures:
;
; (make-record-type <type-name> <field-names>)    -> <record-type>
; (record-constructor <record-type> <field-names>) -> <constructor>
; (record-predicate <record-type>)               -> <predicate>
; (record-accessor <record-type <field-name>)    -> <accessor>
; (record-modifier <record-type <field-name>)    -> <modifier>
;   where
; (<constructor> <initial-value> ...)         -> <record>
; (<predicate> <value>)                       -> <boolean>
; (<accessor> <record>)                       -> <value>
; (<modifier> <record> <value>)         -> <unspecific>

; Record types are implemented using vector-like records.  The first
; slot of each record contains the record's type, which is itself a
; record.

(define (record-type record)
  (record-ref record 0))

;----------------
; Record types are themselves records, so we first define the type for
; them.  Except for problems with circularities, this could be defined as:
;  (define-record-type :record-type
;    (make-record-type name field-tags)
;    record-type?
;    (name record-type-name)
;    (field-tags record-type-field-tags))
; As it is, we need to define everything by hand.

(define :record-type (make-record 3))
(record-set! :record-type 0 :record-type) ; Its type is itself.
(record-set! :record-type 1 ':record-type)
(record-set! :record-type 2 '(name field-tags))

; Now that :record-type exists we can define a procedure for making more
; record types.

(define (make-record-type name field-tags)
  (let ([new (make-record 3)])
    (record-set! new 0 :record-type)
    (record-set! new 1 name)
    (record-set! new 2 field-tags)
    new))

; Accessors for record types.

(define (record-type-name record-type)
  (record-ref record-type 1))

(define (record-type-field-tags record-type)
  (record-ref record-type 2))

;----------------
; A utility for getting the offset of a field within a record.

(define (field-index type tag)
  (let loop ([i 1]
             [tags (record-type-field-tags type)])
    (cond
      [(null? tags) (error "record type has no such field" type tag)]
      [(eq? tag (car tags)) i]
      [else (loop (+ i 1) (cdr tags))])))

;----------------
; Now we are ready to define RECORD-CONSTRUCTOR and the rest of the
; procedures used by the macro expansion of DEFINE-RECORD-TYPE.

(define (record-constructor type tags)
  (let ([size (length (record-type-field-tags type))]
        [arg-count (length tags)]
        [indexes (map (lambda (tag) (field-index type tag)) tags)])
    (lambda args
      (if (= (length args) arg-count)
          (let ([new (make-record (+ size 1))])
            (record-set! new 0 type)
            (for-each (lambda (arg i) (record-set! new i arg)) args indexes)
            new)
          (error "wrong number of arguments to constructor" type args)))))

(define (record-predicate type)
  (lambda (thing) (and (record? thing) (eq? (record-type thing) type))))

(define (record-accessor type tag)
  (let ([index (field-index type tag)])
    (lambda (thing)
      (if (and (record? thing) (eq? (record-type thing) type))
          (record-ref thing index)
          (error "accessor applied to bad value" type tag thing)))))

(define (record-modifier type tag)
  (let ([index (field-index type tag)])
    (lambda (thing value)
      (if (and (record? thing) (eq? (record-type thing) type))
          (record-set! thing index value)
          (error "modifier applied to bad value" type tag thing)))))

; This implements a record abstraction that is identical to vectors,
; except that they are not vectors (VECTOR? returns false when given a
; record and RECORD? returns false when given a vector).  The following
; procedures are provided:
;   (record? <value>)                -> <boolean>
;   (make-record <size>)             -> <record>
;   (record-ref <record> <index>)    -> <value>
;   (record-set! <record> <index> <value>) -> <unspecific>
;
; These can implemented in R5RS Scheme as vectors with a distinguishing
; value at index zero, providing VECTOR? is redefined to be a procedure
; that returns false if its argument contains the distinguishing record
; value.  EVAL is also redefined to use the new value of VECTOR?.

; Define the marker and redefine VECTOR? and EVAL.

(define record-marker (list 'record-marker))

(define real-vector? vector?)

(define (vector? x)
  (and (real-vector? x) (or (= 0 (vector-length x)) (not (eq? (vector-ref x 0) record-marker)))))

; This won't work if ENV is the interaction environment and someone has
; redefined LAMBDA there.

; (define eval
;   (let ([real-eval eval]) (lambda (exp env) ((real-eval `(lambda (vector?) ,exp)) vector?))))

; Definitions of the record procedures.

(define (record? x)
  (and (real-vector? x) (< 0 (vector-length x)) (eq? (vector-ref x 0) record-marker)))

(define (make-record size)
  (let ([new (make-vector (+ size 1))])
    (vector-set! new 0 record-marker)
    new))

(define (record-ref record index)
  (vector-ref record (+ index 1)))

(define (record-set! record index value)
  (vector-set! record (+ index 1) value))
