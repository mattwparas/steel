;; Lets say we want classes and methods
;; Implementing methods and generics over this should involve creating a runtime class registry
;; For instance: 

(define *registry* (mutable-vector))

;; '__object__ => refers to the class definition
;; '__instance__ => refers to an instance of the class

;; Class definitions are just dynamic instances of structs, with a variable
;; number of fields

(define (Class name parents fields methods)
    (mutable-vector 'ClassObject name parents fields methods))

(define (Class-name self) (mut-vector-ref self 1))
(define (Class-parents self) (mut-vector-ref self 2))
(define (Class-fields self) (mut-vector-ref self 3))
(define (Class-methods self) (mut-vector-ref self 4))

;; Base object for everything in the class hierarchy
(define Object (Class 'Object '() '() (hash)))

;; Define the class object for Animal
;; Is a child of the Object base class
(define Animal (Class 'Animal (list Object) '(name color weight) (hash)))

;; Add a method to a class
(define (define-method class-object name method)
    (let ((methods (Class-methods class-object)))
        (vector-set! class-object 4 (hash-insert methods name method))))

(define-method Object 'foo (lambda () (displayln "Hi From Object")))
(define-method Animal 'foo (lambda () (displayln "Hi From Animal")))


;; Get the method on the immediate class object
(define (_get-method class-object name)
  (-> class-object
   (Class-methods)
   (hash-try-get name)))

(define (get-method class-object name)
  (let ((local-method (_get-method class-object name)))
    ;; If _this_ class object contains the method, then we return this method
    ;; This way we always select the correct method in the class hierarchy
    (if local-method
        local-method
        ;; Otherwise, attempt to resolve the method on the parents
        ;; For now, methods are just a hashmap from name -> method
        (let ((possible-methods (map get-method (Class-parents class-object))))
          (if (equal? 1 (length possible-methods))
              (car possible-methods)
              (error! "Unable to resolve the method on the class instance"))))))


(define (position? vec value)
  (define list-length (length vec))
  (define (loop vec idx)
    (cond
      [(= idx list-length) => (error! "Value not a member of the vector")]
      [(equal? value (list-ref vec idx)) => idx]
      [else => (loop vec (+ idx 1))]))
  (loop vec 0))


(define (%get-slot-idx class-object field-name)
  (-> class-object
      (Class-fields)
      (position? field-name)))

(define (get-slot class-instance field-name)
  (list-ref (vector-ref class-instance 2)
            (%get-slot-idx (vector-ref class-instance 1) field-name)))

(define (set-slot! class-instance field-name value)
  ; (displayln (Class-fields (vector-ref class-instance 1)))
  (displayln (%get-slot-idx (vector-ref class-instance 1) field-name))

  (vector-set!
   (vector-ref class-instance 2)
   ;; Get the slot index -> maps the field name to the index in the vector
   (%get-slot-idx (vector-ref class-instance 1) field-name)
   value))

;; Instances should be represented in memory (for now) just as tagged vectors
;; Each of the fields will be zero'd out
(define (%allocate-instance class-object)
  ;; We can just use a normal vector here, not a mutable vector
  (vector '__instance__
          ;; Reference to the class object here
          class-object
          ;; Fields as a mutable vector
          ;; Name resolution should be done via %get-slot method
          (apply mutable-vector (map (lambda (x) void)
                                     (Class-fields class-object)))))



;; Overwrite printing for class -> defer to printing method if possible
;;







;; TODO: mutable structs -> also normal structs really need to be fixed
;; Be able to override the printing of an object via a scheme function -> describe how it gets

;; Class instances get registered with the run time via the class registry
;; Constructors for classes resolve the constructor function via the registry
;; Inheritance chain is solved dynamically via method resolution
;; Class instance should store information about its parents

; (define (make-class class)
;     (vector-push! class))


