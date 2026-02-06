 # Collections

Steel has a number of collections to represent to hold values, each with different runtime characteristics:

* Pairs
* Lists
* Immutable vectors
* Mutable vectors
* Hash maps
* Hash sets

## Lists

Lists are a fundamental data structure in Steel and are often the default structure used for many operations.

The `list` function can be used to construct a list, and takes any number of arguments:

```scheme
(list 1 2 3 4 5)
(list)
(list "hello" "world")
(list "composite" 'data 100)
(list (list "nested" "list"))
```

Accessing a list recursively often involves using the `car` and `cdr` operations:

```scheme
(car (list 10 20 30)) ;; => 10
(cdr (list 10 20 30)) ;; => '(20 30)
```

`car` will extract the first element of the list, where `cdr` will return the "rest" of the
list, without the first element.

See the lists module documentation for all functions available on lists.

### Quoted values

You may often see the following syntax:

```scheme
'(10 20 30)
```

This is a quoted list, which is special syntax where the values inside are not evaluated.

## Pairs

Pairs are fundamentally represented by the cons cell, the building blocks of
a linked lists. You may see the vocabulary `car` and `cdr`, but remember these are synonymous to `first` and `rest`.

A cons cell is created with the `cons` function:

```scheme
(cons 10 20) ;; => '(a . b)
```

Creating a pair with two elements where the second element does not point to another cell.

```scheme
(car (cons 10 20)) ;; => 10
(cdr (cons 10 20)) ;; => 20
```

Consing an element to the empty list will yield a `list`:

```scheme
(cons 1 (list)) ;; => '(1)
```

If you `cons` onto a pair where the second element is not the empty list, you get whats called an improper list:

```scheme
(cons 1 (cons 2 3)) ;; => (1 2 . 3)
```

Improper vs proper in this context would mean that the underlying data structure representing this is different from
a `list`. They both represent sequences, but an improper list will be a single element linked list, whereas
a proper list uses a VList, which is an exponentially growing chain of linked vectors. In general, prefer to use
normal lists when possible.
