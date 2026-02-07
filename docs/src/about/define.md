# Expressions and defining values

Everything in Steel is an expression, meaning it evaluates to a value. You can experiment with this at the repl:

```scheme
λ > 10
=> 10
λ > (if #t 'true 'false)
=> 'true
```

If the value is `#<void>`, then the resulting value won't be printed. Functions will implicitly return the
last expression in the body, or otherwise will return `#<void>`. To return void, you can use the `void` value.
Define expressions implicitly return `#<void>`.


## Defining values

There are two kinds of ways to define values, using the keyword `define`, or with `let`.

`define` is used at the top level:

```scheme
(define x "hello world")

(displayln x) ;; prints "hello world"
```

Defining functions can be done by having the right hand side be a `lambda` expression:

```scheme
(define hello (lambda () (displayln "hi")))
```

Alternatively, you can use the short hand syntax for defining a function, where the
arguments are provided after the function name in a list:

```scheme
(define (hello x y z)
   (displayln x)
   (displayln y)
   (displayln z))

(hello 10 20 30)
```

Define can also be used within the body of a function:

```scheme
(define (foo y)
  (define z (+ 10 y))
  (+ z z))
```

## Lets

For scoped variables, you can use `let`:

```scheme
(let ([x 5]) x)

(let ([x 5]
      [y 10])
      (+ x y))

(let ([x 5])
  (let ([y 2])
    (+ x y)))
```

Variables defined at the same level of let cannot reference each other since its not in scope yet:

```scheme
(let ([x 5]
      [y x]) ;; This would error here with a free identifier
  (+ x y))
```

To avoid this, you can use `let*` instead, which is a macro that expands to lets which can reference
variables at the same level like this:

```scheme
(let* ([x 5]
      [y x])
  (+ x y)) ;; 10
```
h

### Named let

There is another form of let, known as the _named_ let, in which you provide a name, and arguments for a
function with their initial value, plus a body. The function will get called implicitly, like so:

```scheme
(let fac ([n 10])
    (if (zero? n)
        1
        (* n (fac (sub1 n))))) ;; 3628800
```
