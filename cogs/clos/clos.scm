;; ---------------------------- Class Object definitions --------------------------------------------

(struct Class-Object (name parents interfaces fields methods))
(struct Interface (name methods))
(struct Class-Instance (class-object fields))


(define (list-subset? left right)
  (hashset-subset? (list->hashset left)
                   (list->hashset right)))

;; Classes contain:
;; A name, which is required to be a symbol (string should also work, but for now a symbol simplifies this)
;; The parents are a list of class objects -> we don't want classes pointing to arbitrary objects
;; The fields are represented by a list of symbols, to refer to using the get-slot method
;; The methods in theory could be anything, as long as they're functions.
;;
;; Here we don't restrict the hash map of symbol -> function, but rather will restrict it on the
;; function when adding the methods.
(define/contract (Class name parents interfaces fields methods)
  (->/c symbol? (listof Class-Object?) (listof Interface?) (listof symbol?) hash? Class-Object?)
  (unless (list-subset?
           ;; Collect the list of required methods for the given interfaces
           (transduce interfaces
                      (flat-mapping Interface-methods)
                      (into-list))
           ;; Extract the methods that have been defined concretely on this class at construction
           (hash-keys->list methods))
    (error! "Not all required methods are implemented for the given interfaces"))

  (Class-Object
    name
    parents
    interfaces
    ;; Explicitly go collect the fields to flatten into this class given the
    ;; class hierarchy
    (combine-local-and-parent-fields fields parents)
    methods))

(define (Make-Class 
               name 
               #:fields (fields '())  
               #:methods (methods '())
               #:parents (parents '()) 
               #:interfaces (interfaces '()))
    (Class name parents interfaces fields methods))


(define (contains-duplicates? lst)
  (not
   (equal? (hashset-length (apply hashset lst))
            (length lst))))

;; Flatten the incoming list
(define (flatten lst) (transduce lst (flattening) (into-list)))

(define (collect-fields list-of-class-objects)
  (transduce list-of-class-objects
             (flat-mapping Class-Object-fields)
             (into-list)))

(define (combine-local-and-parent-fields local-fields list-of-class-objects)
  (let ((appended (append
                   local-fields
                   (collect-fields list-of-class-objects))))
    (if (contains-duplicates? appended)
        (error! "Class field is unresolvable")
        appended)))



;; -----------------------------------------------------------------------------------------------------


;; Add a method to a class
;; This can also occur in the root definition of the object in the hash, but the contract
;; won't be checked there at the moment TODO
(define/contract (define-method class-object name method)
  (->/c Class-Object? symbol? function? any/c)
  (let ((methods (Class-Object-methods class-object)))
    (set-Class-Object-methods! class-object (hash-insert methods name method))))

(define (resolve-parent-method class-object name)
  (let ((possible-methods (map (lambda (x) (get-method x name)) (Class-Object-parents class-object))))
    (if (equal? 1 (length possible-methods))
        (car possible-methods)
        (error! "Unable to resolve the method on the class instance"))))

;; Attempt to resolve the method on the class object via the class hierarchy
;; This checks the current class object, and then attempt to resolve the method upwards, returning the first
;; one that it finds. If there are multiple candidates, we bail and error out since we can't determine
;; which method we intended to call.
(define/contract (get-method class-object name)
  (->/c Class-Object? symbol? function?)
  (let ((local-method (-> class-object
                          (Class-Object-methods)
                          (hash-try-get name))))
    ;; If _this_ class object contains the method, then we return this method
    ;; This way we always select the correct method in the class hierarchy
    (if local-method
        local-method
        ;; Otherwise, attempt to resolve the method on the parents
        ;; For now, methods are just a hashmap from name -> method
        (resolve-parent-method class-object name))))

;; Attempt to resolve the method on the parent class to this class objects
(define/contract (get-super-method class-object name)
  (->/c Class-Object? symbol? function?)
  (resolve-parent-method class-object name))

(define/contract (position? lst value)
  (->/c list? any/c integer?)
  (define list-length (length lst))
  (define (loop lst idx)
    (cond
      [(= idx list-length) => (error! "Value not a member of the list")]
      [(equal? value (list-ref lst idx)) => idx]
      [else => (loop lst (+ idx 1))]))
  (loop lst 0))

;; Map the given field name to an index in the class' slot
(define/contract (%get-slot-idx class-object field-name)
  (->/c Class-Object? symbol? integer?)
  (-> class-object
      (Class-Object-fields)
      (position? field-name)))

;; This returns whatever value is found in the slot
;; This _could_ be any value. TODO: find a way to bind a contract to a slot
(define/contract (get-slot class-instance field-name)
  (->/c Class-Instance? symbol? any/c)
  (mut-vector-ref (Class-Instance-fields class-instance)
                  (%get-slot-idx (Class-Instance-class-object class-instance) field-name)))

;; Sets the slot in the class instance found a `field-name` to `value`
;; TODO: make the contract be dependent to have the result match the value's contract
(define/contract (set-slot! class-instance field-name value)
  (->/c Class-Instance? symbol? any/c any/c)
  (vector-set!
   (Class-Instance-fields class-instance)
   ;; Get the slot index -> maps the field name to the index in the vector
   (%get-slot-idx (Class-Instance-class-object class-instance) field-name)
   value))

;; Instances should be represented in memory (for now) just as tagged vectors
;; Each of the fields will be zero'd out
(define/contract (%allocate-instance class-object)
  (->/c Class-Object? Class-Instance?)
  ;; We can just use a normal vector here, not a mutable vector
  (Class-Instance
          ;; Reference to the class object here
          class-object
          ;; Fields as a mutable vector
          ;; Name resolution should be done via %get-slot method
          (apply mutable-vector (map (lambda (x) void)
                                     (Class-Object-fields class-object)))))

;; Get the method on the class object denoted by the method name
;; and call it given the arguments here
;; TODO: figure out contracts for multi arity functions
(define (call class-instance method-name . args)
  (apply (get-method (Class-Instance-class-object class-instance) method-name)
         ;; The instance is always an implicit first argument, and as such gets
         ;; cons onto the first of the arguments to call the function
         (cons class-instance args)))

;; Same as above, calls the method on the nearest parent object
(define (call-super class-instance method-name . args)
  (apply (get-super-method (Class-Instance-class-object class-instance) method-name)
         (cons class-instance args)))

(define/contract (class-instance-name class-instance)
  (->/c Class-Instance? symbol?)
  (Class-Object-name (Class-Instance-class-object class-instance)))

;; TODO -> check if object implements interface
;; (define/contract (class-implements-Interface? class-instance interface)
;;   (->/c Class-Object? Interface? boolean?)
;;   (member? interface
;;

;; ------------------- Examples --------------------------

;; Base object for everything in the class hierarchy
(define Object (Class 'Object
                      '()
                      '()
                      '()
                      (hash 'println
                            (lambda (self)
                              (displayln (-> "#<"
                                             (string-append
                                              (symbol->string (class-instance-name self)))
                                             (string-append ">")))))))

;; Define the class object for Animal
;; Is a child of the Object base class
(define Animal (Class 'Animal
                      (list Object)
                      '()
                      '(name color weight)
                      (hash
                       'get-weight (lambda (self) (get-slot self 'weight)))))

;; Dog inherits from Animal, adds on the 'good-boy? field
(define Dog (Class 'Dog
                   (list Animal)
                   '()
                   '(good-boy?)
                   (hash)))

;; TODO: Once keyword arguments are a thing, classes could be defined like so:
;; (define Dog (Class #:name 'Dog
;;                    #:parents (list Animal)
;;                    #:interfaces '()
;;                    #:fields '(good-boy?)
;;                    #:methods (hash)))


;; Allocates a new instance of a dog - here all of the fields are default to #<void>
(define sherman (%allocate-instance Dog))
;; Set the weight to be 25 -> this is set in the instance, and all fields are flattened on construction
(set-slot! sherman 'weight 25)
;; This will traverse upwards on the methods to find the method
(call sherman 'println)
;; This should find the method on the parent class
(call sherman 'get-weight)
;; Explicitly call the method on the super
(call-super sherman 'get-weight)
;; Setting a slot since the default allocation of the instance sets everything to void
(set-slot! sherman 'good-boy? #true)

(define Stinky (Interface 'Stinky '(smelly icky)))

(define Worm (Class 'Worm
                    (list Dog)
                    (list Stinky)
                    '()
                    (hash
                     'smelly (lambda (self) "Smelly!")
                     'icky (lambda (self) "Icky!"))))


(define New-Worm (Make-Class 'Worm
                    #:parents (list Dog)
                    #:interfaces (list Stinky)
                    #:methods 
                      (hash
                        'smelly (lambda (self) "Smelly!")
                        'icky (lambda (self) "Icky!"))))