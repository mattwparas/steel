# steel/json
De/serialization from/to JSON.
### **string->jsexpr**
Deserializes a JSON string into a Steel value.

(string->jsexpr json) -> any/c?

* json : string?

#### Examples
```scheme
(string->jsexpr "{\"foo\": [3]}") ;; => '#hash((foo . (3)))
```
### **value->jsexpr-string**
Serializes a Steel value into a string.

(value->jsexpr-string any/c?) -> string?

#### Examples
```scheme
(value->jsexpr-string `(,(hash "foo" #t))) ;; => "[{\"foo\":true}]"
```
