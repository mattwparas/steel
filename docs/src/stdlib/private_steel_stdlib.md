# #%private/steel/stdlib
**this module is in the prelude and therefore automatically available when running steel.**

### **filter**
Returns new list, keeping elements from `lst` which applying `pred` to the element
returns #t.


#### Examples

```scheme
(filter even? (range 0 5)) ;; '(0 2 4)
```
### **map**
Applies `func` to the elements of the `lsts` from the first
elements to the last. The `func` argument must accept the same
number of arguments as the number of supplied `lsts`, and all
`lsts` must have the same number of elements. The result is a list
containing each result of `func` in order.

(map func lst . lsts) -> list?

#### Examples
```scheme
(map add1 (range 0 5)) ;; '(1 2 3 4 5)
```
### **\*abort**
### **\*meta-continuation\***
### **\*reset**
### **\*shift**
### **add1**
### **assoc**
### **assq**
### **caaaar**
### **caaar**
### **caadar**
### **caaddr**
### **caadr**
### **caar**
### **cadaar**
### **cadadr**
### **cadar**
### **caddar**
### **cadddr**
### **caddr**
### **cadr**
### **call-with-values**
### **cdaaar**
### **cdaar**
### **cdadar**
### **cdaddr**
### **cdadr**
### **cdar**
### **cddaar**
### **cddadr**
### **cddar**
### **cdddar**
### **cdddr**
### **cdddr**
### **cddr**
### **contains?**
### **curry**
### **curry2**
### **drop**
### **even-rec?**
### **flatten**
### **flip**
### **fold**
### **foldl**
### **foldr**
### **force**
### **id**
### **max**
### **mem-helper**
### **min**
### **odd-rec?**
### **reduce**
### **slice**
### **sub1**
### **sum**
### **unfold**
### **values**
### **with-finalizer**
### **zero?**
