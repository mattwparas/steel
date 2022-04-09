;; '__object__ => refers to the class definition
;; '__instance__ => refers to an instance of the class

;; ---------------------------- Class Object definitions --------------------------------------------

;; Class objects are defined as vectors with the following form:
;; mut-vec['ClassObject symbol? (listof class-object?) (listof symbol) (listof functions)]
(define (class-object? object)
  (and (mutable-vector? object) (equal? 'ClassObject (mut-vector-ref object 0))))

;; immutable-vec['__instance__ class-object? mutable-vector]
(define (class-instance? object)
  (and (mutable-vector? object) (equal? '__instance__ (vector-ref object 0))))

;; Classes contain:
;; A name, which is required to be a symbol (string should also work, but for now a symbol simplifies this)
;; The parents are a list of class objects -> we don't want classes pointing to arbitrary objects
;; The fields are represented by a list of symbols, to refer to using the get-slot method
;; The methods in theory could be anything, as long as they're functions.
;;
;; Here we don't restrict the hash map of symbol -> function, but rather will restrict it on the
;; function when adding the methods.
(define/contract (Class name parents fields methods)
  (->/c symbol? (listof class-object?) (listof symbol?) hash? class-object?)
  (mutable-vector 'ClassObject name parents fields methods))


;; Collect the fields
(define (collect-fields list-of-class-objects)
  (append (map Class-fields list-of-class-objects)
          (map (lambda (c) (collect-fields (Class-parents c))))))

;; Set up some accessors for the class object.
;; These all require having an instance of a class object
(define/contract (Class-name self)
  (->/c class-object? symbol?)
  (mut-vector-ref self 1))

(define/contract (Class-parents self)
  (->/c class-object? list?)
  (mut-vector-ref self 2))

(define/contract (Class-fields self)
  (->/c class-object? list?)
  (mut-vector-ref self 3))

(define/contract (Class-methods self)
  (->/c class-object? hash?)
  (mut-vector-ref self 4))

;; -----------------------------------------------------------------------------------------------------


;; Add a method to a class
;; This can also occur in the root definition of the object in the hash, but the contract
;; won't be checked there at the moment TODO
(define/contract (define-method class-object name method)
  (->/c class-object? symbol? function? any/c)
  (let ((methods (Class-methods class-object)))
    (vector-set! class-object 4 (hash-insert methods name method))))

;; Attempt to resolve the method on the class object via the class hierarchy
;; This checks the current class object, and then attempt to resolve the method upwards, returning the first
;; one that it finds. If there are multiple candidates, we bail and error out since we can't determine
;; which method we intended to call.
(define/contract (get-method class-object name)
  (->/c class-object? symbol? function?)
  (let ((local-method (-> class-object
                          (Class-methods)
                          (hash-try-get name))))
    ;; If _this_ class object contains the method, then we return this method
    ;; This way we always select the correct method in the class hierarchy
    (if local-method
        local-method
        ;; Otherwise, attempt to resolve the method on the parents
        ;; For now, methods are just a hashmap from name -> method
        (let ((possible-methods (map (lambda (x) (get-method x name)) (Class-parents class-object))))
          (if (equal? 1 (length possible-methods))
              (car possible-methods)
              (error! "Unable to resolve the method on the class instance"))))))

(define (position? lst value)
  (define list-length (length lst))
  (define (loop lst idx)
    (cond
      [(= idx list-length) => (error! "Value not a member of the vector")]
      [(equal? value (list-ref lst idx)) => idx]
      [else => (loop lst (+ idx 1))]))
  (loop lst 0))

;; Map the given field name to an index in the class' slot
(define/contract (%get-slot-idx class-object field-name)
  (->/c class-object? symbol? integer?)
  (-> class-object
      (Class-fields)
      (position? field-name)))

;; This returns whatever value is found in the slot
;; This _could_ be any value. TODO: find a way to bind a contract to a slot
(define/contract (get-slot class-instance field-name)
  (->/c class-instance? symbol? any/c)
  (list-ref (vector-ref class-instance 2)
            (%get-slot-idx (vector-ref class-instance 1) field-name)))

;; Sets the slot in the class instance found a `field-name` to `value`
;; TODO: make the contract be dependent to have the result match the value's contract
(define/contract (set-slot! class-instance field-name value)
  (->/c class-instance? symbol? any/c any/c)
  (vector-set!
   (vector-ref class-instance 2)
   ;; Get the slot index -> maps the field name to the index in the vector
   (%get-slot-idx (vector-ref class-instance 1) field-name)
   value))

;; Instances should be represented in memory (for now) just as tagged vectors
;; Each of the fields will be zero'd out
(define/contract (%allocate-instance class-object)
  (->/c class-object? class-instance?)
  ;; We can just use a normal vector here, not a mutable vector
  (vector '__instance__
          ;; Reference to the class object here
          class-object
          ;; Fields as a mutable vector
          ;; Name resolution should be done via %get-slot method
          (apply mutable-vector (map (lambda (x) void)
                                     (Class-fields class-object)))))

;; Get the method on the class object denoted by the method name
;; and call it given the arguments here
;; TODO: figure out contracts for multi arity functions
(define (call class-instance method-name . args)
  (apply (get-method (vector-ref class-instance 1) method-name) args))


;; ------------------- Examples --------------------------

;; TODO: have fields also be inherited like methods. Right now fields are only resolved locally to the scope
;; They should also be resolved via the class hierarchy, and stored locally.
;; Traverse the class hierarchy upwards, collect the fields as a set - error out if there are duplicates
;; Once the fields are found, flatten the fields into a vector, store them locally in the instance,
;; and resolve names as before using the name -> slot mapping.

;; Base object for everything in the class hierarchy
(define Object (Class 'Object
                      '()
                      '()
                      (hash 'println (lambda (self) (displayln "<#Object>")))))

;; Define the class object for Animal
;; Is a child of the Object base class
(define Animal (Class 'Animal
                      (list Object)
                      '(name color weight)
                      (hash 'println (lambda (self) (displayln "<#Animal>")))))
