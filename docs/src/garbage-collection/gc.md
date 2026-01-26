# Memory management

Steel uses a combination of reference counting and a mark and sweep garbage collector in order to manage memory. Important notes regarding this:

1. All immutable values are reference counted, this includes built in data structures such as:
    - Lists
    - Immutable vectors
    - Hash maps
    - Hash sets
    - Strings
    - Pairs
    - Big integers
    - Symbols (although, most are interned)

2. All primitive types are unboxed, these include:
    - Characters
    - booleans
    - floats (f64)
    - integers (isize)
    - the `#<void>` type

3. Mutable values are transformed into `boxes` - which are mutable memory locations managed by the garbage collector.
4. Mutable vectos are a special form handled by the garbage collector as well.
5. Mutable structs, are immutable vectors with boxes in each slot for each mutable memory location.
6. Heap allocated boxes are weak references to a reference counted pointer living in the heap. Thus, even memory managed by the garbage collector is also reference counted. This means that unboxing a mutable memory location requires two pointer jumps - first the weak pointer to the heap, then the strong pointer to the underlying value.
    - A nice consequence of this is that we can perform fast collections for unreachable values managed by the heap without having to perform a mark and sweep.

## Reference counting implementation

Steel contains four modes: standard `Rc`, standard `Arc`, `triomphe::Arc`, or an implementation of biased reference counting `steel_rc::BiasedRc`.

The biased reference counting implementation is the default for the interpreter, but needs to be opted into itself when embedding steel directly
with the `"biased"` feature.

The biased reference counting implementation will mark each object with the thread that created it. When performing operations, it will first check if its on
the thread that created it - if so, it will perform normal reference count operations. Otherwise, it will use atomic operations. There is some merging of the two
counts that needs to happen which requires runtime support, which means that this doesn't necessarily scale arbitrarily as a standalone reference count operation.
When a new thread is created, steel will register that thread with the runtime, and at regular intervals will merge reference counts as needed.

## Reference counting optimizations

One consequence of using reference counted variables is that there will be a non trivial amount of time spent performing reference count operations on values coming and going from the stack. The steel compiler and vm performs a few optimizations to reduce the reference counting thrash, and also to improve the usage of the functional data structures built in.

Consider the following:

```scheme

(define (reverse input output)
  (if (null? input)
    output
    (reverse (cdr input) (cons (car input) output))))

(reverse (list 10 20 30 40) '()) ;; => (list 40 30 20 10)

```

This is a simple function that takes an input list and reverses it. It does so recursively, calling `reverse` in tail position, meaning we'll get a tail call optimization and this gets converted into a `JUMP` in the VM.

Lists in Steel aren't your classic cons cells - they're implemented more like unrolled linked lists or vlists - meaning they're more like chunks of large contiguous vectors strung together. Copying those on each write to it would be silly, so the compiler analyzes a function and attempts to find the last usage of variable. For every last usage of a variable, the VM doesn't just copy the value from the stack, it actually _moves_ it off of the VM stack - meaning the reference count has the potential to be 1 by the time it hits the last usage of a variable.

When the reference count is 1, we can perform an in-place mutation of the resulting list - which gives us a nice performance win! So in the above snippet, we see the following bytecode:

```
0     SDEF           : 0      reverse
1     PUREFUNC       : 21     lambda
2     PASS           : 0      
3     PASS           : 256    
4     READLOCAL0     : 0      input
5     CALLGLOBAL     : 155    null?
6     FUNC           : 1      null?
7     IF             : 6      
8     READLOCAL1     : 1      output
9     JMP            : 17     
10    READLOCAL0     : 0      input
11    CALLGLOBAL     : 86     cdr
12    FUNC           : 1      cdr
13    MOVEREADLOCAL0 : 0      input ;; <- Last usage of input
14    CALLGLOBAL     : 92     car
15    FUNC           : 1      car
16    MOVEREADLOCAL1 : 1      output ;; <- last usage of output
17    CALLGLOBAL     : 94     cons
18    FUNC           : 2      cons
19    TCOJMP         : 2      reverse
20    PASS           : 0      
21    POPPURE        : 2      
22    ECLOSURE       : 2      
23    EDEF           : 0      
24    BIND           : 975    reverse
25    VOID           : 0      
26    POPPURE        : 0      
```



Which corresponds to these call sites:

```scheme

(define (reverse input output)
  (if (null? input)
    output
    (reverse (cdr input) (cons (car input) output))))
                                    ^^^^^  ^^^^^^
```

In this case, the mutation of `output` is great - since we can `cons` onto the list and this under the hood is just a push onto the underlying vector. However, for input it doesn't mean much; `car` is a reference operation and has no optimizations associated with it. The compiler isn't quite smart enough to do this _yet_, but rewriting this slightly, we can get the optimization we want:


```scheme

(define (reverse input output)
  (if (null? input)
      output
      (let ([first-element (car input)]) 
        (reverse (cdr input) (cons first-element output)))))
                                    
```

Which results in this bytecode:

```
0     SDEF           : 0      reverse
1     PUREFUNC       : 24     lambda
2     PASS           : 0      
3     PASS           : 256    
4     READLOCAL0     : 0      input
5     CALLGLOBAL     : 156    null?
6     FUNC           : 1      null?
7     IF             : 6      
8     READLOCAL1     : 1      output
9     JMP            : 20     
10    BEGINSCOPE     : 0      
11    READLOCAL0     : 0      input
12    CALLGLOBAL     : 98     car
13    FUNC           : 1      car
14    MOVEREADLOCAL0 : 0      input ;; <- Here
15    CALLGLOBAL     : 97     cdr
16    FUNC           : 1      cdr
17    READLOCAL2     : 2      first-element
18    MOVEREADLOCAL1 : 1      output ;; <- Here
19    CALLGLOBAL     : 86     cons
20    FUNC           : 2      cons
21    TCOJMP         : 2      reverse
22    PASS           : 0      
23    LETENDSCOPE    : 2      
24    POPPURE        : 2      
25    ECLOSURE       : 2      
26    EDEF           : 0      
27    BIND           : 975    reverse
28    VOID           : 0      
29    POPPURE        : 0      

```

`cdr` when coupled with a unique list will just move the view of the underlying storage over one element - the element is still _there_ - but we don't see it, and it means we don't have to allocate a new list (since in this new world, `cdr` _can_ allocate). But - we get some nice cache locality for this!

The first example takes 123 ms to reverse a list of 100000 - whereas the second example takes only 23 ms! In racket, the equivalent code takes somewhere between 2 and 5 ms.

Another toy example of this optimization can be seen here, comparing Steel to Racket:

```scheme

(define values-to-insert 
  (map (lambda (n) 
    (cons (number->string n) n)) (range 0 10000)))

(define (hash-insert-loop-test hmap values)
  (if (null? values)
      hmap
      (let ([p (car values)])
        (hash-insert-loop-test (hash-insert hmap (car p) (cdr p)) (cdr values)))))

(hash-insert-loop-test (hash) values-to-insert)
```

This snippet takes about 10-20 ms on my machine, and the equivalent body of code in Racket (replacing `hash-insert` with `hash-set`) takes about 8 ms. So this is a nice win. Without this optimization for hash maps, the same code took 5 seconds!
