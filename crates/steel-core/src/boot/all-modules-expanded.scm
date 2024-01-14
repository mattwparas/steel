(begin
  (define hash-get (%module-get% %-builtin-module-steel/hash (quote hash-get)))
  (define hash-empty? (%module-get% %-builtin-module-steel/hash (quote hash-empty?)))
  (define hash-insert (%module-get% %-builtin-module-steel/hash (quote hash-insert)))
  (define hash-contains? (%module-get% %-builtin-module-steel/hash (quote hash-contains?)))
  (define hash-try-get (%module-get% %-builtin-module-steel/hash (quote hash-try-get)))
  (define hash-ref (%module-get% %-builtin-module-steel/hash (quote hash-ref)))
  (define hash-keys->list (%module-get% %-builtin-module-steel/hash (quote hash-keys->list)))
  (define hash-clear (%module-get% %-builtin-module-steel/hash (quote hash-clear)))
  (define hash-values->vector (%module-get% %-builtin-module-steel/hash (quote hash-values->vector)))
  (define hash-length (%module-get% %-builtin-module-steel/hash (quote hash-length)))
  (define hash-union (%module-get% %-builtin-module-steel/hash (quote hash-union)))
  (define %keyword-hash (%module-get% %-builtin-module-steel/hash (quote %keyword-hash)))
  (define hash-keys->vector (%module-get% %-builtin-module-steel/hash (quote hash-keys->vector)))
  (define hash-values->list (%module-get% %-builtin-module-steel/hash (quote hash-values->list)))
  (define hash (%module-get% %-builtin-module-steel/hash (quote hash)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define hashset (%module-get% %-builtin-module-steel/sets (quote hashset)))
  (define hashset-clear (%module-get% %-builtin-module-steel/sets (quote hashset-clear)))
  (define hashset->vector (%module-get% %-builtin-module-steel/sets (quote hashset->vector)))
  (define hashset-subset? (%module-get% %-builtin-module-steel/sets (quote hashset-subset?)))
  (define hashset-contains? (%module-get% %-builtin-module-steel/sets (quote hashset-contains?)))
  (define hashset-length (%module-get% %-builtin-module-steel/sets (quote hashset-length)))
  (define hashset-insert (%module-get% %-builtin-module-steel/sets (quote hashset-insert)))
  (define list->hashset (%module-get% %-builtin-module-steel/sets (quote list->hashset)))
  (define hashset->list (%module-get% %-builtin-module-steel/sets (quote hashset->list)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define try-list-ref (%module-get% %-builtin-module-steel/lists (quote try-list-ref)))
  (define length (%module-get% %-builtin-module-steel/lists (quote length)))
  (define list->string (%module-get% %-builtin-module-steel/lists (quote list->string)))
  (define transduce (%module-get% %-builtin-module-steel/lists (quote transduce)))
  (define push-back (%module-get% %-builtin-module-steel/lists (quote push-back)))
  (define car (%module-get% %-builtin-module-steel/lists (quote car)))
  (define third (%module-get% %-builtin-module-steel/lists (quote third)))
  (define list (%module-get% %-builtin-module-steel/lists (quote list)))
  (define second (%module-get% %-builtin-module-steel/lists (quote second)))
  (define range (%module-get% %-builtin-module-steel/lists (quote range)))
  (define cdr (%module-get% %-builtin-module-steel/lists (quote cdr)))
  (define cons (%module-get% %-builtin-module-steel/lists (quote cons)))
  (define cdr-null? (%module-get% %-builtin-module-steel/lists (quote cdr-null?)))
  (define last (%module-get% %-builtin-module-steel/lists (quote last)))
  (define list-ref (%module-get% %-builtin-module-steel/lists (quote list-ref)))
  (define pair? (%module-get% %-builtin-module-steel/lists (quote pair?)))
  (define empty? (%module-get% %-builtin-module-steel/lists (quote empty?)))
  (define list-tail (%module-get% %-builtin-module-steel/lists (quote list-tail)))
  (define take (%module-get% %-builtin-module-steel/lists (quote take)))
  (define first (%module-get% %-builtin-module-steel/lists (quote first)))
  (define rest (%module-get% %-builtin-module-steel/lists (quote rest)))
  (define reverse (%module-get% %-builtin-module-steel/lists (quote reverse)))
  (define apply (%module-get% %-builtin-module-steel/lists (quote apply)))
  (define append (%module-get% %-builtin-module-steel/lists (quote append)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define char-digit? (%module-get% %-builtin-module-steel/strings (quote char-digit?)))
  (define string->number (%module-get% %-builtin-module-steel/strings (quote string->number)))
  (define trim-end (%module-get% %-builtin-module-steel/strings (quote trim-end)))
  (define ends-with? (%module-get% %-builtin-module-steel/strings (quote ends-with?)))
  (define string<? (%module-get% %-builtin-module-steel/strings (quote string<?)))
  (define string-ci>? (%module-get% %-builtin-module-steel/strings (quote string-ci>?)))
  (define string-ci>=? (%module-get% %-builtin-module-steel/strings (quote string-ci>=?)))
  (define string-ci<=? (%module-get% %-builtin-module-steel/strings (quote string-ci<=?)))
  (define string>? (%module-get% %-builtin-module-steel/strings (quote string>?)))
  (define string-ref (%module-get% %-builtin-module-steel/strings (quote string-ref)))
  (define char->number (%module-get% %-builtin-module-steel/strings (quote char->number)))
  (define string-replace (%module-get% %-builtin-module-steel/strings (quote string-replace)))
  (define number->string (%module-get% %-builtin-module-steel/strings (quote number->string)))
  (define trim-end-matches (%module-get% %-builtin-module-steel/strings (quote trim-end-matches)))
  (define char-whitespace? (%module-get% %-builtin-module-steel/strings (quote char-whitespace?)))
  (define string->upper (%module-get% %-builtin-module-steel/strings (quote string->upper)))
  (define make-string (%module-get% %-builtin-module-steel/strings (quote make-string)))
  (define int->string (%module-get% %-builtin-module-steel/strings (quote int->string)))
  (define trim (%module-get% %-builtin-module-steel/strings (quote trim)))
  (define string-ci<? (%module-get% %-builtin-module-steel/strings (quote string-ci<?)))
  (define string->symbol (%module-get% %-builtin-module-steel/strings (quote string->symbol)))
  (define split-whitespace (%module-get% %-builtin-module-steel/strings (quote split-whitespace)))
  (define split-many (%module-get% %-builtin-module-steel/strings (quote split-many)))
  (define string->list (%module-get% %-builtin-module-steel/strings (quote string->list)))
  (define char=? (%module-get% %-builtin-module-steel/strings (quote char=?)))
  (define string->lower (%module-get% %-builtin-module-steel/strings (quote string->lower)))
  (define to-string (%module-get% %-builtin-module-steel/strings (quote to-string)))
  (define string-append (%module-get% %-builtin-module-steel/strings (quote string-append)))
  (define string (%module-get% %-builtin-module-steel/strings (quote string)))
  (define substring (%module-get% %-builtin-module-steel/strings (quote substring)))
  (define char-upcase (%module-get% %-builtin-module-steel/strings (quote char-upcase)))
  (define string=? (%module-get% %-builtin-module-steel/strings (quote string=?)))
  (define split-once (%module-get% %-builtin-module-steel/strings (quote split-once)))
  (define string-ci=? (%module-get% %-builtin-module-steel/strings (quote string-ci=?)))
  (define starts-with? (%module-get% %-builtin-module-steel/strings (quote starts-with?)))
  (define string-length (%module-get% %-builtin-module-steel/strings (quote string-length)))
  (define string>=? (%module-get% %-builtin-module-steel/strings (quote string>=?)))
  (define trim-start-matches (%module-get% %-builtin-module-steel/strings (quote trim-start-matches)))
  (define string->int (%module-get% %-builtin-module-steel/strings (quote string->int)))
  (define trim-start (%module-get% %-builtin-module-steel/strings (quote trim-start)))
  (define string<=? (%module-get% %-builtin-module-steel/strings (quote string<=?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define concat-symbols (%module-get% %-builtin-module-steel/symbols (quote concat-symbols)))
  (define symbol->string (%module-get% %-builtin-module-steel/symbols (quote symbol->string)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define range-vec (%module-get% %-builtin-module-steel/vectors (quote range-vec)))
  (define vector-append! (%module-get% %-builtin-module-steel/vectors (quote vector-append!)))
  (define null? (%module-get% %-builtin-module-steel/vectors (quote null?)))
  (define mut-vector-ref (%module-get% %-builtin-module-steel/vectors (quote mut-vector-ref)))
  (define mut-vec-len (%module-get% %-builtin-module-steel/vectors (quote mut-vec-len)))
  (define vec-rest (%module-get% %-builtin-module-steel/vectors (quote vec-rest)))
  (define push (%module-get% %-builtin-module-steel/vectors (quote push)))
  (define vector-ref (%module-get% %-builtin-module-steel/vectors (quote vector-ref)))
  (define vec-append (%module-get% %-builtin-module-steel/vectors (quote vec-append)))
  (define push-front (%module-get% %-builtin-module-steel/vectors (quote push-front)))
  (define vector-set! (%module-get% %-builtin-module-steel/vectors (quote vector-set!)))
  (define mutable-vector (%module-get% %-builtin-module-steel/vectors (quote mutable-vector)))
  (define pop-front (%module-get% %-builtin-module-steel/vectors (quote pop-front)))
  (define mutable-vector->list
    (%module-get% %-builtin-module-steel/vectors (quote mutable-vector->list)))
  (define make-vector (%module-get% %-builtin-module-steel/vectors (quote make-vector)))
  (define vector (%module-get% %-builtin-module-steel/vectors (quote vector)))
  (define vector-push! (%module-get% %-builtin-module-steel/vectors (quote vector-push!)))
  (define vector-length (%module-get% %-builtin-module-steel/vectors (quote vector-length)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define stream-cons (%module-get% %-builtin-module-steel/streams (quote stream-cons)))
  (define stream-car (%module-get% %-builtin-module-steel/streams (quote stream-car)))
  (define empty-stream (%module-get% %-builtin-module-steel/streams (quote empty-stream)))
  (define #%stream-cdr (%module-get% %-builtin-module-steel/streams (quote #%stream-cdr)))
  (define stream-empty? (%module-get% %-builtin-module-steel/streams (quote stream-empty?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define hash? (%module-get% %-builtin-module-steel/identity (quote hash?)))
  (define procedure? (%module-get% %-builtin-module-steel/identity (quote procedure?)))
  (define #%private-struct? (%module-get% %-builtin-module-steel/identity (quote #%private-struct?)))
  (define future? (%module-get% %-builtin-module-steel/identity (quote future?)))
  (define void? (%module-get% %-builtin-module-steel/identity (quote void?)))
  (define boolean? (%module-get% %-builtin-module-steel/identity (quote boolean?)))
  (define continuation? (%module-get% %-builtin-module-steel/identity (quote continuation?)))
  (define vector? (%module-get% %-builtin-module-steel/identity (quote vector?)))
  (define list? (%module-get% %-builtin-module-steel/identity (quote list?)))
  (define set? (%module-get% %-builtin-module-steel/identity (quote set?)))
  (define bool? (%module-get% %-builtin-module-steel/identity (quote bool?)))
  (define float? (%module-get% %-builtin-module-steel/identity (quote float?)))
  (define struct? (%module-get% %-builtin-module-steel/identity (quote struct?)))
  (define number? (%module-get% %-builtin-module-steel/identity (quote number?)))
  (define not (%module-get% %-builtin-module-steel/identity (quote not)))
  (define symbol? (%module-get% %-builtin-module-steel/identity (quote symbol?)))
  (define char? (%module-get% %-builtin-module-steel/identity (quote char?)))
  (define string? (%module-get% %-builtin-module-steel/identity (quote string?)))
  (define function? (%module-get% %-builtin-module-steel/identity (quote function?)))
  (define integer? (%module-get% %-builtin-module-steel/identity (quote integer?)))
  (define mutable-vector? (%module-get% %-builtin-module-steel/identity (quote mutable-vector?)))
  (define atom? (%module-get% %-builtin-module-steel/identity (quote atom?)))
  (define int? (%module-get% %-builtin-module-steel/identity (quote int?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define expt (%module-get% %-builtin-module-steel/numbers (quote expt)))
  (define * (%module-get% %-builtin-module-steel/numbers (quote *)))
  (define arithmetic-shift (%module-get% %-builtin-module-steel/numbers (quote arithmetic-shift)))
  (define f+ (%module-get% %-builtin-module-steel/numbers (quote f+)))
  (define abs (%module-get% %-builtin-module-steel/numbers (quote abs)))
  (define + (%module-get% %-builtin-module-steel/numbers (quote +)))
  (define even? (%module-get% %-builtin-module-steel/numbers (quote even?)))
  (define log (%module-get% %-builtin-module-steel/numbers (quote log)))
  (define / (%module-get% %-builtin-module-steel/numbers (quote /)))
  (define exp (%module-get% %-builtin-module-steel/numbers (quote exp)))
  (define - (%module-get% %-builtin-module-steel/numbers (quote -)))
  (define exact->inexact (%module-get% %-builtin-module-steel/numbers (quote exact->inexact)))
  (define quotient (%module-get% %-builtin-module-steel/numbers (quote quotient)))
  (define odd? (%module-get% %-builtin-module-steel/numbers (quote odd?)))
  (define round (%module-get% %-builtin-module-steel/numbers (quote round)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define equal? (%module-get% %-builtin-module-steel/equality (quote equal?)))
  (define eqv? (%module-get% %-builtin-module-steel/equality (quote eqv?)))
  (define eq? (%module-get% %-builtin-module-steel/equality (quote eq?)))
  (define = (%module-get% %-builtin-module-steel/equality (quote =)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define > (%module-get% %-builtin-module-steel/ord (quote >)))
  (define < (%module-get% %-builtin-module-steel/ord (quote <)))
  (define <= (%module-get% %-builtin-module-steel/ord (quote <=)))
  (define >= (%module-get% %-builtin-module-steel/ord (quote >=)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define filtering (%module-get% %-builtin-module-steel/transducers (quote filtering)))
  (define dropping (%module-get% %-builtin-module-steel/transducers (quote dropping)))
  (define interleaving (%module-get% %-builtin-module-steel/transducers (quote interleaving)))
  (define into-count (%module-get% %-builtin-module-steel/transducers (quote into-count)))
  (define compose (%module-get% %-builtin-module-steel/transducers (quote compose)))
  (define into-max (%module-get% %-builtin-module-steel/transducers (quote into-max)))
  (define into-string (%module-get% %-builtin-module-steel/transducers (quote into-string)))
  (define into-hashset (%module-get% %-builtin-module-steel/transducers (quote into-hashset)))
  (define flattening (%module-get% %-builtin-module-steel/transducers (quote flattening)))
  (define flat-mapping (%module-get% %-builtin-module-steel/transducers (quote flat-mapping)))
  (define into-list (%module-get% %-builtin-module-steel/transducers (quote into-list)))
  (define extending (%module-get% %-builtin-module-steel/transducers (quote extending)))
  (define into-product (%module-get% %-builtin-module-steel/transducers (quote into-product)))
  (define into-sum (%module-get% %-builtin-module-steel/transducers (quote into-sum)))
  (define mapping (%module-get% %-builtin-module-steel/transducers (quote mapping)))
  (define into-vector (%module-get% %-builtin-module-steel/transducers (quote into-vector)))
  (define into-min (%module-get% %-builtin-module-steel/transducers (quote into-min)))
  (define into-reducer (%module-get% %-builtin-module-steel/transducers (quote into-reducer)))
  (define into-nth (%module-get% %-builtin-module-steel/transducers (quote into-nth)))
  (define taking (%module-get% %-builtin-module-steel/transducers (quote taking)))
  (define into-last (%module-get% %-builtin-module-steel/transducers (quote into-last)))
  (define enumerating (%module-get% %-builtin-module-steel/transducers (quote enumerating)))
  (define zipping (%module-get% %-builtin-module-steel/transducers (quote zipping)))
  (define into-hashmap (%module-get% %-builtin-module-steel/transducers (quote into-hashmap)))
  (define into-for-each (%module-get% %-builtin-module-steel/transducers (quote into-for-each)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define read-to-string (%module-get% %-builtin-module-steel/io (quote read-to-string)))
  (define stdout-simple-displayln
    (%module-get% %-builtin-module-steel/io (quote stdout-simple-displayln)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define create-directory!
    (%module-get% %-builtin-module-steel/filesystem (quote create-directory!)))
  (define delete-directory!
    (%module-get% %-builtin-module-steel/filesystem (quote delete-directory!)))
  (define is-file? (%module-get% %-builtin-module-steel/filesystem (quote is-file?)))
  (define is-dir? (%module-get% %-builtin-module-steel/filesystem (quote is-dir?)))
  (define file-name (%module-get% %-builtin-module-steel/filesystem (quote file-name)))
  (define read-dir (%module-get% %-builtin-module-steel/filesystem (quote read-dir)))
  (define path->extension (%module-get% %-builtin-module-steel/filesystem (quote path->extension)))
  (define copy-directory-recursively!
    (%module-get% %-builtin-module-steel/filesystem (quote copy-directory-recursively!)))
  (define path-exists? (%module-get% %-builtin-module-steel/filesystem (quote path-exists?)))
  (define current-directory
    (%module-get% %-builtin-module-steel/filesystem (quote current-directory)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define raw-write-char (%module-get% %-builtin-module-steel/ports (quote raw-write-char)))
  (define raw-write (%module-get% %-builtin-module-steel/ports (quote raw-write)))
  (define raw-write-string (%module-get% %-builtin-module-steel/ports (quote raw-write-string)))
  (define open-output-string (%module-get% %-builtin-module-steel/ports (quote open-output-string)))
  (define input-port? (%module-get% %-builtin-module-steel/ports (quote input-port?)))
  (define stdin (%module-get% %-builtin-module-steel/ports (quote stdin)))
  (define flush-output-port (%module-get% %-builtin-module-steel/ports (quote flush-output-port)))
  (define get-output-string (%module-get% %-builtin-module-steel/ports (quote get-output-string)))
  (define open-input-file (%module-get% %-builtin-module-steel/ports (quote open-input-file)))
  (define stdout (%module-get% %-builtin-module-steel/ports (quote stdout)))
  (define output-port? (%module-get% %-builtin-module-steel/ports (quote output-port?)))
  (define read-port-to-string (%module-get% %-builtin-module-steel/ports (quote read-port-to-string)))
  (define #%default-output-port
    (%module-get% %-builtin-module-steel/ports (quote #%default-output-port)))
  (define #%default-input-port
    (%module-get% %-builtin-module-steel/ports (quote #%default-input-port)))
  (define open-output-file (%module-get% %-builtin-module-steel/ports (quote open-output-file)))
  (define write-line! (%module-get% %-builtin-module-steel/ports (quote write-line!)))
  (define read-line-from-port (%module-get% %-builtin-module-steel/ports (quote read-line-from-port)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define eval! (%module-get% %-builtin-module-steel/meta (quote eval!)))
  (define Engine::raise_error (%module-get% %-builtin-module-steel/meta (quote Engine::raise_error)))
  (define #%private-cycle-collector
    (%module-get% %-builtin-module-steel/meta (quote #%private-cycle-collector)))
  (define #%iterator-finished (%module-get% %-builtin-module-steel/meta (quote #%iterator-finished)))
  (define error-with-span (%module-get% %-builtin-module-steel/meta (quote error-with-span)))
  (define #%native-fn-ptr-doc (%module-get% %-builtin-module-steel/meta (quote #%native-fn-ptr-doc)))
  (define set-test-mode! (%module-get% %-builtin-module-steel/meta (quote set-test-mode!)))
  (define box (%module-get% %-builtin-module-steel/meta (quote box)))
  (define #%unbox (%module-get% %-builtin-module-steel/meta (quote #%unbox)))
  (define current-os! (%module-get% %-builtin-module-steel/meta (quote current-os!)))
  (define #%set-box! (%module-get% %-builtin-module-steel/meta (quote #%set-box!)))
  (define multi-arity? (%module-get% %-builtin-module-steel/meta (quote multi-arity?)))
  (define #%struct-property-ref
    (%module-get% %-builtin-module-steel/meta (quote #%struct-property-ref)))
  (define struct->list (%module-get% %-builtin-module-steel/meta (quote struct->list)))
  (define Engine::modules->list
    (%module-get% %-builtin-module-steel/meta (quote Engine::modules->list)))
  (define maybe-get-env-var (%module-get% %-builtin-module-steel/meta (quote maybe-get-env-var)))
  (define #%private-cycle-collector-values
    (%module-get% %-builtin-module-steel/meta (quote #%private-cycle-collector-values)))
  (define iter-next! (%module-get% %-builtin-module-steel/meta (quote iter-next!)))
  (define env-var (%module-get% %-builtin-module-steel/meta (quote env-var)))
  (define attach-contract-struct!
    (%module-get% %-builtin-module-steel/meta (quote attach-contract-struct!)))
  (define #%black-box (%module-get% %-builtin-module-steel/meta (quote #%black-box)))
  (define #%function-ptr-table-add
    (%module-get% %-builtin-module-steel/meta (quote #%function-ptr-table-add)))
  (define Engine::new (%module-get% %-builtin-module-steel/meta (quote Engine::new)))
  (define memory-address (%module-get% %-builtin-module-steel/meta (quote memory-address)))
  (define set-strong-box! (%module-get% %-builtin-module-steel/meta (quote set-strong-box!)))
  (define inspect-bytecode (%module-get% %-builtin-module-steel/meta (quote inspect-bytecode)))
  (define join! (%module-get% %-builtin-module-steel/meta (quote join!)))
  (define get-test-mode (%module-get% %-builtin-module-steel/meta (quote get-test-mode)))
  (define %iterator? (%module-get% %-builtin-module-steel/meta (quote %iterator?)))
  (define #%function-ptr-table-get
    (%module-get% %-builtin-module-steel/meta (quote #%function-ptr-table-get)))
  (define expand! (%module-get% %-builtin-module-steel/meta (quote expand!)))
  (define #%get-dylib (%module-get% %-builtin-module-steel/meta (quote #%get-dylib)))
  (define #%private-cycle-collector-get
    (%module-get% %-builtin-module-steel/meta (quote #%private-cycle-collector-get)))
  (define function-name (%module-get% %-builtin-module-steel/meta (quote function-name)))
  (define assert! (%module-get% %-builtin-module-steel/meta (quote assert!)))
  (define Engine::add-module (%module-get% %-builtin-module-steel/meta (quote Engine::add-module)))
  (define make-struct-type (%module-get% %-builtin-module-steel/meta (quote make-struct-type)))
  (define #%struct-update (%module-get% %-builtin-module-steel/meta (quote #%struct-update)))
  (define arity? (%module-get% %-builtin-module-steel/meta (quote arity?)))
  (define call-with-current-continuation
    (%module-get% %-builtin-module-steel/meta (quote call-with-current-continuation)))
  (define read! (%module-get% %-builtin-module-steel/meta (quote read!)))
  (define Engine::clone (%module-get% %-builtin-module-steel/meta (quote Engine::clone)))
  (define poll! (%module-get% %-builtin-module-steel/meta (quote poll!)))
  (define value->iterator (%module-get% %-builtin-module-steel/meta (quote value->iterator)))
  (define breakpoint! (%module-get% %-builtin-module-steel/meta (quote breakpoint!)))
  (define call-with-exception-handler
    (%module-get% %-builtin-module-steel/meta (quote call-with-exception-handler)))
  (define run! (%module-get% %-builtin-module-steel/meta (quote run!)))
  (define current-function-span
    (%module-get% %-builtin-module-steel/meta (quote current-function-span)))
  (define raise-error (%module-get% %-builtin-module-steel/meta (quote raise-error)))
  (define value->string (%module-get% %-builtin-module-steel/meta (quote value->string)))
  (define active-object-count (%module-get% %-builtin-module-steel/meta (quote active-object-count)))
  (define #%box (%module-get% %-builtin-module-steel/meta (quote #%box)))
  (define call/cc (%module-get% %-builtin-module-steel/meta (quote call/cc)))
  (define raise-error-with-span
    (%module-get% %-builtin-module-steel/meta (quote raise-error-with-span)))
  (define block-on (%module-get% %-builtin-module-steel/meta (quote block-on)))
  (define unbox-strong (%module-get% %-builtin-module-steel/meta (quote unbox-strong)))
  (define set-env-var! (%module-get% %-builtin-module-steel/meta (quote set-env-var!)))
  (define get-contract-struct (%module-get% %-builtin-module-steel/meta (quote get-contract-struct)))
  (define #%function-ptr-table
    (%module-get% %-builtin-module-steel/meta (quote #%function-ptr-table)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define string->jsexpr (%module-get% %-builtin-module-steel/json (quote string->jsexpr)))
  (define value->jsexpr-string
    (%module-get% %-builtin-module-steel/json (quote value->jsexpr-string)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define void (%module-get% %-builtin-module-steel/constants (quote void)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define syntax-loc (%module-get% %-builtin-module-steel/syntax (quote syntax-loc)))
  (define syntax->datum (%module-get% %-builtin-module-steel/syntax (quote syntax->datum)))
  (define syntax? (%module-get% %-builtin-module-steel/syntax (quote syntax?)))
  (define #%syntax/raw (%module-get% %-builtin-module-steel/syntax (quote #%syntax/raw)))
  (define syntax/loc (%module-get% %-builtin-module-steel/syntax (quote syntax/loc)))
  (define syntax-span (%module-get% %-builtin-module-steel/syntax (quote syntax-span)))
  (define syntax-e (%module-get% %-builtin-module-steel/syntax (quote syntax-e)))
  (define #%debug-syntax->exprkind
    (%module-get% %-builtin-module-steel/syntax (quote #%debug-syntax->exprkind)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define spawn-process (%module-get% %-builtin-module-steel/process (quote spawn-process)))
  (define child-stdin (%module-get% %-builtin-module-steel/process (quote child-stdin)))
  (define set-current-dir! (%module-get% %-builtin-module-steel/process (quote set-current-dir!)))
  (define set-piped-stdout! (%module-get% %-builtin-module-steel/process (quote set-piped-stdout!)))
  (define child-stdout (%module-get% %-builtin-module-steel/process (quote child-stdout)))
  (define wait (%module-get% %-builtin-module-steel/process (quote wait)))
  (define which (%module-get% %-builtin-module-steel/process (quote which)))
  (define wait->stdout (%module-get% %-builtin-module-steel/process (quote wait->stdout)))
  (define command (%module-get% %-builtin-module-steel/process (quote command)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define Err (%module-get% %-builtin-module-steel/core/result (quote Err)))
  (define Err->value (%module-get% %-builtin-module-steel/core/result (quote Err->value)))
  (define Ok->value (%module-get% %-builtin-module-steel/core/result (quote Ok->value)))
  (define Err? (%module-get% %-builtin-module-steel/core/result (quote Err?)))
  (define Ok (%module-get% %-builtin-module-steel/core/result (quote Ok)))
  (define Ok? (%module-get% %-builtin-module-steel/core/result (quote Ok?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define Some (%module-get% %-builtin-module-steel/core/option (quote Some)))
  (define Some->value (%module-get% %-builtin-module-steel/core/option (quote Some->value)))
  (define Some? (%module-get% %-builtin-module-steel/core/option (quote Some?)))
  (define None? (%module-get% %-builtin-module-steel/core/option (quote None?)))
  (define None (%module-get% %-builtin-module-steel/core/option (quote None)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define TypeId? (%module-get% %-builtin-module-steel/core/types (quote TypeId?)))
  (define #%vtable-update-entry!
    (%module-get% %-builtin-module-steel/core/types (quote #%vtable-update-entry!)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define spawn-thread! (%module-get% %-builtin-module-steel/threads (quote spawn-thread!)))
  (define channel->try-recv (%module-get% %-builtin-module-steel/threads (quote channel->try-recv)))
  (define make-channels (%module-get% %-builtin-module-steel/threads (quote make-channels)))
  (define thread-finished? (%module-get% %-builtin-module-steel/threads (quote thread-finished?)))
  (define thread::current/id (%module-get% %-builtin-module-steel/threads (quote thread::current/id)))
  (define channel->send (%module-get% %-builtin-module-steel/threads (quote channel->send)))
  (define thread-join! (%module-get% %-builtin-module-steel/threads (quote thread-join!)))
  (define channel->recv (%module-get% %-builtin-module-steel/threads (quote channel->recv)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.hash-get (%module-get% %-builtin-module-steel/hash (quote hash-get)))
  (define #%prim.hash-empty? (%module-get% %-builtin-module-steel/hash (quote hash-empty?)))
  (define #%prim.hash-insert (%module-get% %-builtin-module-steel/hash (quote hash-insert)))
  (define #%prim.hash-contains? (%module-get% %-builtin-module-steel/hash (quote hash-contains?)))
  (define #%prim.hash-try-get (%module-get% %-builtin-module-steel/hash (quote hash-try-get)))
  (define #%prim.hash-ref (%module-get% %-builtin-module-steel/hash (quote hash-ref)))
  (define #%prim.hash-keys->list (%module-get% %-builtin-module-steel/hash (quote hash-keys->list)))
  (define #%prim.hash-clear (%module-get% %-builtin-module-steel/hash (quote hash-clear)))
  (define #%prim.hash-values->vector
    (%module-get% %-builtin-module-steel/hash (quote hash-values->vector)))
  (define #%prim.hash-length (%module-get% %-builtin-module-steel/hash (quote hash-length)))
  (define #%prim.hash-union (%module-get% %-builtin-module-steel/hash (quote hash-union)))
  (define #%prim.%keyword-hash (%module-get% %-builtin-module-steel/hash (quote %keyword-hash)))
  (define #%prim.hash-keys->vector
    (%module-get% %-builtin-module-steel/hash (quote hash-keys->vector)))
  (define #%prim.hash-values->list
    (%module-get% %-builtin-module-steel/hash (quote hash-values->list)))
  (define #%prim.hash (%module-get% %-builtin-module-steel/hash (quote hash)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.hashset (%module-get% %-builtin-module-steel/sets (quote hashset)))
  (define #%prim.hashset-clear (%module-get% %-builtin-module-steel/sets (quote hashset-clear)))
  (define #%prim.hashset->vector (%module-get% %-builtin-module-steel/sets (quote hashset->vector)))
  (define #%prim.hashset-subset? (%module-get% %-builtin-module-steel/sets (quote hashset-subset?)))
  (define #%prim.hashset-contains?
    (%module-get% %-builtin-module-steel/sets (quote hashset-contains?)))
  (define #%prim.hashset-length (%module-get% %-builtin-module-steel/sets (quote hashset-length)))
  (define #%prim.hashset-insert (%module-get% %-builtin-module-steel/sets (quote hashset-insert)))
  (define #%prim.list->hashset (%module-get% %-builtin-module-steel/sets (quote list->hashset)))
  (define #%prim.hashset->list (%module-get% %-builtin-module-steel/sets (quote hashset->list)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.try-list-ref (%module-get% %-builtin-module-steel/lists (quote try-list-ref)))
  (define #%prim.length (%module-get% %-builtin-module-steel/lists (quote length)))
  (define #%prim.list->string (%module-get% %-builtin-module-steel/lists (quote list->string)))
  (define #%prim.transduce (%module-get% %-builtin-module-steel/lists (quote transduce)))
  (define #%prim.push-back (%module-get% %-builtin-module-steel/lists (quote push-back)))
  (define #%prim.car (%module-get% %-builtin-module-steel/lists (quote car)))
  (define #%prim.third (%module-get% %-builtin-module-steel/lists (quote third)))
  (define #%prim.list (%module-get% %-builtin-module-steel/lists (quote list)))
  (define #%prim.second (%module-get% %-builtin-module-steel/lists (quote second)))
  (define #%prim.range (%module-get% %-builtin-module-steel/lists (quote range)))
  (define #%prim.cdr (%module-get% %-builtin-module-steel/lists (quote cdr)))
  (define #%prim.cons (%module-get% %-builtin-module-steel/lists (quote cons)))
  (define #%prim.cdr-null? (%module-get% %-builtin-module-steel/lists (quote cdr-null?)))
  (define #%prim.last (%module-get% %-builtin-module-steel/lists (quote last)))
  (define #%prim.list-ref (%module-get% %-builtin-module-steel/lists (quote list-ref)))
  (define #%prim.pair? (%module-get% %-builtin-module-steel/lists (quote pair?)))
  (define #%prim.empty? (%module-get% %-builtin-module-steel/lists (quote empty?)))
  (define #%prim.list-tail (%module-get% %-builtin-module-steel/lists (quote list-tail)))
  (define #%prim.take (%module-get% %-builtin-module-steel/lists (quote take)))
  (define #%prim.first (%module-get% %-builtin-module-steel/lists (quote first)))
  (define #%prim.rest (%module-get% %-builtin-module-steel/lists (quote rest)))
  (define #%prim.reverse (%module-get% %-builtin-module-steel/lists (quote reverse)))
  (define #%prim.apply (%module-get% %-builtin-module-steel/lists (quote apply)))
  (define #%prim.append (%module-get% %-builtin-module-steel/lists (quote append)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.char-digit? (%module-get% %-builtin-module-steel/strings (quote char-digit?)))
  (define #%prim.string->number (%module-get% %-builtin-module-steel/strings (quote string->number)))
  (define #%prim.trim-end (%module-get% %-builtin-module-steel/strings (quote trim-end)))
  (define #%prim.ends-with? (%module-get% %-builtin-module-steel/strings (quote ends-with?)))
  (define #%prim.string<? (%module-get% %-builtin-module-steel/strings (quote string<?)))
  (define #%prim.string-ci>? (%module-get% %-builtin-module-steel/strings (quote string-ci>?)))
  (define #%prim.string-ci>=? (%module-get% %-builtin-module-steel/strings (quote string-ci>=?)))
  (define #%prim.string-ci<=? (%module-get% %-builtin-module-steel/strings (quote string-ci<=?)))
  (define #%prim.string>? (%module-get% %-builtin-module-steel/strings (quote string>?)))
  (define #%prim.string-ref (%module-get% %-builtin-module-steel/strings (quote string-ref)))
  (define #%prim.char->number (%module-get% %-builtin-module-steel/strings (quote char->number)))
  (define #%prim.string-replace (%module-get% %-builtin-module-steel/strings (quote string-replace)))
  (define #%prim.number->string (%module-get% %-builtin-module-steel/strings (quote number->string)))
  (define #%prim.trim-end-matches
    (%module-get% %-builtin-module-steel/strings (quote trim-end-matches)))
  (define #%prim.char-whitespace?
    (%module-get% %-builtin-module-steel/strings (quote char-whitespace?)))
  (define #%prim.string->upper (%module-get% %-builtin-module-steel/strings (quote string->upper)))
  (define #%prim.make-string (%module-get% %-builtin-module-steel/strings (quote make-string)))
  (define #%prim.int->string (%module-get% %-builtin-module-steel/strings (quote int->string)))
  (define #%prim.trim (%module-get% %-builtin-module-steel/strings (quote trim)))
  (define #%prim.string-ci<? (%module-get% %-builtin-module-steel/strings (quote string-ci<?)))
  (define #%prim.string->symbol (%module-get% %-builtin-module-steel/strings (quote string->symbol)))
  (define #%prim.split-whitespace
    (%module-get% %-builtin-module-steel/strings (quote split-whitespace)))
  (define #%prim.split-many (%module-get% %-builtin-module-steel/strings (quote split-many)))
  (define #%prim.string->list (%module-get% %-builtin-module-steel/strings (quote string->list)))
  (define #%prim.char=? (%module-get% %-builtin-module-steel/strings (quote char=?)))
  (define #%prim.string->lower (%module-get% %-builtin-module-steel/strings (quote string->lower)))
  (define #%prim.to-string (%module-get% %-builtin-module-steel/strings (quote to-string)))
  (define #%prim.string-append (%module-get% %-builtin-module-steel/strings (quote string-append)))
  (define #%prim.string (%module-get% %-builtin-module-steel/strings (quote string)))
  (define #%prim.substring (%module-get% %-builtin-module-steel/strings (quote substring)))
  (define #%prim.char-upcase (%module-get% %-builtin-module-steel/strings (quote char-upcase)))
  (define #%prim.string=? (%module-get% %-builtin-module-steel/strings (quote string=?)))
  (define #%prim.split-once (%module-get% %-builtin-module-steel/strings (quote split-once)))
  (define #%prim.string-ci=? (%module-get% %-builtin-module-steel/strings (quote string-ci=?)))
  (define #%prim.starts-with? (%module-get% %-builtin-module-steel/strings (quote starts-with?)))
  (define #%prim.string-length (%module-get% %-builtin-module-steel/strings (quote string-length)))
  (define #%prim.string>=? (%module-get% %-builtin-module-steel/strings (quote string>=?)))
  (define #%prim.trim-start-matches
    (%module-get% %-builtin-module-steel/strings (quote trim-start-matches)))
  (define #%prim.string->int (%module-get% %-builtin-module-steel/strings (quote string->int)))
  (define #%prim.trim-start (%module-get% %-builtin-module-steel/strings (quote trim-start)))
  (define #%prim.string<=? (%module-get% %-builtin-module-steel/strings (quote string<=?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.concat-symbols (%module-get% %-builtin-module-steel/symbols (quote concat-symbols)))
  (define #%prim.symbol->string (%module-get% %-builtin-module-steel/symbols (quote symbol->string)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.range-vec (%module-get% %-builtin-module-steel/vectors (quote range-vec)))
  (define #%prim.vector-append! (%module-get% %-builtin-module-steel/vectors (quote vector-append!)))
  (define #%prim.null? (%module-get% %-builtin-module-steel/vectors (quote null?)))
  (define #%prim.mut-vector-ref (%module-get% %-builtin-module-steel/vectors (quote mut-vector-ref)))
  (define #%prim.mut-vec-len (%module-get% %-builtin-module-steel/vectors (quote mut-vec-len)))
  (define #%prim.vec-rest (%module-get% %-builtin-module-steel/vectors (quote vec-rest)))
  (define #%prim.push (%module-get% %-builtin-module-steel/vectors (quote push)))
  (define #%prim.vector-ref (%module-get% %-builtin-module-steel/vectors (quote vector-ref)))
  (define #%prim.vec-append (%module-get% %-builtin-module-steel/vectors (quote vec-append)))
  (define #%prim.push-front (%module-get% %-builtin-module-steel/vectors (quote push-front)))
  (define #%prim.vector-set! (%module-get% %-builtin-module-steel/vectors (quote vector-set!)))
  (define #%prim.mutable-vector (%module-get% %-builtin-module-steel/vectors (quote mutable-vector)))
  (define #%prim.pop-front (%module-get% %-builtin-module-steel/vectors (quote pop-front)))
  (define #%prim.mutable-vector->list
    (%module-get% %-builtin-module-steel/vectors (quote mutable-vector->list)))
  (define #%prim.make-vector (%module-get% %-builtin-module-steel/vectors (quote make-vector)))
  (define #%prim.vector (%module-get% %-builtin-module-steel/vectors (quote vector)))
  (define #%prim.vector-push! (%module-get% %-builtin-module-steel/vectors (quote vector-push!)))
  (define #%prim.vector-length (%module-get% %-builtin-module-steel/vectors (quote vector-length)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.stream-cons (%module-get% %-builtin-module-steel/streams (quote stream-cons)))
  (define #%prim.stream-car (%module-get% %-builtin-module-steel/streams (quote stream-car)))
  (define #%prim.empty-stream (%module-get% %-builtin-module-steel/streams (quote empty-stream)))
  (define #%prim.#%stream-cdr (%module-get% %-builtin-module-steel/streams (quote #%stream-cdr)))
  (define #%prim.stream-empty? (%module-get% %-builtin-module-steel/streams (quote stream-empty?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.hash? (%module-get% %-builtin-module-steel/identity (quote hash?)))
  (define #%prim.procedure? (%module-get% %-builtin-module-steel/identity (quote procedure?)))
  (define #%prim.#%private-struct?
    (%module-get% %-builtin-module-steel/identity (quote #%private-struct?)))
  (define #%prim.future? (%module-get% %-builtin-module-steel/identity (quote future?)))
  (define #%prim.void? (%module-get% %-builtin-module-steel/identity (quote void?)))
  (define #%prim.boolean? (%module-get% %-builtin-module-steel/identity (quote boolean?)))
  (define #%prim.continuation? (%module-get% %-builtin-module-steel/identity (quote continuation?)))
  (define #%prim.vector? (%module-get% %-builtin-module-steel/identity (quote vector?)))
  (define #%prim.list? (%module-get% %-builtin-module-steel/identity (quote list?)))
  (define #%prim.set? (%module-get% %-builtin-module-steel/identity (quote set?)))
  (define #%prim.bool? (%module-get% %-builtin-module-steel/identity (quote bool?)))
  (define #%prim.float? (%module-get% %-builtin-module-steel/identity (quote float?)))
  (define #%prim.struct? (%module-get% %-builtin-module-steel/identity (quote struct?)))
  (define #%prim.number? (%module-get% %-builtin-module-steel/identity (quote number?)))
  (define #%prim.not (%module-get% %-builtin-module-steel/identity (quote not)))
  (define #%prim.symbol? (%module-get% %-builtin-module-steel/identity (quote symbol?)))
  (define #%prim.char? (%module-get% %-builtin-module-steel/identity (quote char?)))
  (define #%prim.string? (%module-get% %-builtin-module-steel/identity (quote string?)))
  (define #%prim.function? (%module-get% %-builtin-module-steel/identity (quote function?)))
  (define #%prim.integer? (%module-get% %-builtin-module-steel/identity (quote integer?)))
  (define #%prim.mutable-vector?
    (%module-get% %-builtin-module-steel/identity (quote mutable-vector?)))
  (define #%prim.atom? (%module-get% %-builtin-module-steel/identity (quote atom?)))
  (define #%prim.int? (%module-get% %-builtin-module-steel/identity (quote int?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.expt (%module-get% %-builtin-module-steel/numbers (quote expt)))
  (define #%prim.* (%module-get% %-builtin-module-steel/numbers (quote *)))
  (define #%prim.arithmetic-shift
    (%module-get% %-builtin-module-steel/numbers (quote arithmetic-shift)))
  (define #%prim.f+ (%module-get% %-builtin-module-steel/numbers (quote f+)))
  (define #%prim.abs (%module-get% %-builtin-module-steel/numbers (quote abs)))
  (define #%prim.+ (%module-get% %-builtin-module-steel/numbers (quote +)))
  (define #%prim.even? (%module-get% %-builtin-module-steel/numbers (quote even?)))
  (define #%prim.log (%module-get% %-builtin-module-steel/numbers (quote log)))
  (define #%prim./ (%module-get% %-builtin-module-steel/numbers (quote /)))
  (define #%prim.exp (%module-get% %-builtin-module-steel/numbers (quote exp)))
  (define #%prim.- (%module-get% %-builtin-module-steel/numbers (quote -)))
  (define #%prim.exact->inexact (%module-get% %-builtin-module-steel/numbers (quote exact->inexact)))
  (define #%prim.quotient (%module-get% %-builtin-module-steel/numbers (quote quotient)))
  (define #%prim.odd? (%module-get% %-builtin-module-steel/numbers (quote odd?)))
  (define #%prim.round (%module-get% %-builtin-module-steel/numbers (quote round)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.equal? (%module-get% %-builtin-module-steel/equality (quote equal?)))
  (define #%prim.eqv? (%module-get% %-builtin-module-steel/equality (quote eqv?)))
  (define #%prim.eq? (%module-get% %-builtin-module-steel/equality (quote eq?)))
  (define #%prim.= (%module-get% %-builtin-module-steel/equality (quote =)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.> (%module-get% %-builtin-module-steel/ord (quote >)))
  (define #%prim.< (%module-get% %-builtin-module-steel/ord (quote <)))
  (define #%prim.<= (%module-get% %-builtin-module-steel/ord (quote <=)))
  (define #%prim.>= (%module-get% %-builtin-module-steel/ord (quote >=)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.filtering (%module-get% %-builtin-module-steel/transducers (quote filtering)))
  (define #%prim.dropping (%module-get% %-builtin-module-steel/transducers (quote dropping)))
  (define #%prim.interleaving (%module-get% %-builtin-module-steel/transducers (quote interleaving)))
  (define #%prim.into-count (%module-get% %-builtin-module-steel/transducers (quote into-count)))
  (define #%prim.compose (%module-get% %-builtin-module-steel/transducers (quote compose)))
  (define #%prim.into-max (%module-get% %-builtin-module-steel/transducers (quote into-max)))
  (define #%prim.into-string (%module-get% %-builtin-module-steel/transducers (quote into-string)))
  (define #%prim.into-hashset (%module-get% %-builtin-module-steel/transducers (quote into-hashset)))
  (define #%prim.flattening (%module-get% %-builtin-module-steel/transducers (quote flattening)))
  (define #%prim.flat-mapping (%module-get% %-builtin-module-steel/transducers (quote flat-mapping)))
  (define #%prim.into-list (%module-get% %-builtin-module-steel/transducers (quote into-list)))
  (define #%prim.extending (%module-get% %-builtin-module-steel/transducers (quote extending)))
  (define #%prim.into-product (%module-get% %-builtin-module-steel/transducers (quote into-product)))
  (define #%prim.into-sum (%module-get% %-builtin-module-steel/transducers (quote into-sum)))
  (define #%prim.mapping (%module-get% %-builtin-module-steel/transducers (quote mapping)))
  (define #%prim.into-vector (%module-get% %-builtin-module-steel/transducers (quote into-vector)))
  (define #%prim.into-min (%module-get% %-builtin-module-steel/transducers (quote into-min)))
  (define #%prim.into-reducer (%module-get% %-builtin-module-steel/transducers (quote into-reducer)))
  (define #%prim.into-nth (%module-get% %-builtin-module-steel/transducers (quote into-nth)))
  (define #%prim.taking (%module-get% %-builtin-module-steel/transducers (quote taking)))
  (define #%prim.into-last (%module-get% %-builtin-module-steel/transducers (quote into-last)))
  (define #%prim.enumerating (%module-get% %-builtin-module-steel/transducers (quote enumerating)))
  (define #%prim.zipping (%module-get% %-builtin-module-steel/transducers (quote zipping)))
  (define #%prim.into-hashmap (%module-get% %-builtin-module-steel/transducers (quote into-hashmap)))
  (define #%prim.into-for-each
    (%module-get% %-builtin-module-steel/transducers (quote into-for-each)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.read-to-string (%module-get% %-builtin-module-steel/io (quote read-to-string)))
  (define #%prim.stdout-simple-displayln
    (%module-get% %-builtin-module-steel/io (quote stdout-simple-displayln)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.create-directory!
    (%module-get% %-builtin-module-steel/filesystem (quote create-directory!)))
  (define #%prim.delete-directory!
    (%module-get% %-builtin-module-steel/filesystem (quote delete-directory!)))
  (define #%prim.is-file? (%module-get% %-builtin-module-steel/filesystem (quote is-file?)))
  (define #%prim.is-dir? (%module-get% %-builtin-module-steel/filesystem (quote is-dir?)))
  (define #%prim.file-name (%module-get% %-builtin-module-steel/filesystem (quote file-name)))
  (define #%prim.read-dir (%module-get% %-builtin-module-steel/filesystem (quote read-dir)))
  (define #%prim.path->extension
    (%module-get% %-builtin-module-steel/filesystem (quote path->extension)))
  (define #%prim.copy-directory-recursively!
    (%module-get% %-builtin-module-steel/filesystem (quote copy-directory-recursively!)))
  (define #%prim.path-exists? (%module-get% %-builtin-module-steel/filesystem (quote path-exists?)))
  (define #%prim.current-directory
    (%module-get% %-builtin-module-steel/filesystem (quote current-directory)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.raw-write-char (%module-get% %-builtin-module-steel/ports (quote raw-write-char)))
  (define #%prim.raw-write (%module-get% %-builtin-module-steel/ports (quote raw-write)))
  (define #%prim.raw-write-string
    (%module-get% %-builtin-module-steel/ports (quote raw-write-string)))
  (define #%prim.open-output-string
    (%module-get% %-builtin-module-steel/ports (quote open-output-string)))
  (define #%prim.input-port? (%module-get% %-builtin-module-steel/ports (quote input-port?)))
  (define #%prim.stdin (%module-get% %-builtin-module-steel/ports (quote stdin)))
  (define #%prim.flush-output-port
    (%module-get% %-builtin-module-steel/ports (quote flush-output-port)))
  (define #%prim.get-output-string
    (%module-get% %-builtin-module-steel/ports (quote get-output-string)))
  (define #%prim.open-input-file (%module-get% %-builtin-module-steel/ports (quote open-input-file)))
  (define #%prim.stdout (%module-get% %-builtin-module-steel/ports (quote stdout)))
  (define #%prim.output-port? (%module-get% %-builtin-module-steel/ports (quote output-port?)))
  (define #%prim.read-port-to-string
    (%module-get% %-builtin-module-steel/ports (quote read-port-to-string)))
  (define #%prim.#%default-output-port
    (%module-get% %-builtin-module-steel/ports (quote #%default-output-port)))
  (define #%prim.#%default-input-port
    (%module-get% %-builtin-module-steel/ports (quote #%default-input-port)))
  (define #%prim.open-output-file
    (%module-get% %-builtin-module-steel/ports (quote open-output-file)))
  (define #%prim.write-line! (%module-get% %-builtin-module-steel/ports (quote write-line!)))
  (define #%prim.read-line-from-port
    (%module-get% %-builtin-module-steel/ports (quote read-line-from-port)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.eval! (%module-get% %-builtin-module-steel/meta (quote eval!)))
  (define #%prim.Engine::raise_error
    (%module-get% %-builtin-module-steel/meta (quote Engine::raise_error)))
  (define #%prim.#%private-cycle-collector
    (%module-get% %-builtin-module-steel/meta (quote #%private-cycle-collector)))
  (define #%prim.#%iterator-finished
    (%module-get% %-builtin-module-steel/meta (quote #%iterator-finished)))
  (define #%prim.error-with-span (%module-get% %-builtin-module-steel/meta (quote error-with-span)))
  (define #%prim.#%native-fn-ptr-doc
    (%module-get% %-builtin-module-steel/meta (quote #%native-fn-ptr-doc)))
  (define #%prim.set-test-mode! (%module-get% %-builtin-module-steel/meta (quote set-test-mode!)))
  (define #%prim.box (%module-get% %-builtin-module-steel/meta (quote box)))
  (define #%prim.#%unbox (%module-get% %-builtin-module-steel/meta (quote #%unbox)))
  (define #%prim.current-os! (%module-get% %-builtin-module-steel/meta (quote current-os!)))
  (define #%prim.#%set-box! (%module-get% %-builtin-module-steel/meta (quote #%set-box!)))
  (define #%prim.multi-arity? (%module-get% %-builtin-module-steel/meta (quote multi-arity?)))
  (define #%prim.#%struct-property-ref
    (%module-get% %-builtin-module-steel/meta (quote #%struct-property-ref)))
  (define #%prim.struct->list (%module-get% %-builtin-module-steel/meta (quote struct->list)))
  (define #%prim.Engine::modules->list
    (%module-get% %-builtin-module-steel/meta (quote Engine::modules->list)))
  (define #%prim.maybe-get-env-var
    (%module-get% %-builtin-module-steel/meta (quote maybe-get-env-var)))
  (define #%prim.#%private-cycle-collector-values
    (%module-get% %-builtin-module-steel/meta (quote #%private-cycle-collector-values)))
  (define #%prim.iter-next! (%module-get% %-builtin-module-steel/meta (quote iter-next!)))
  (define #%prim.env-var (%module-get% %-builtin-module-steel/meta (quote env-var)))
  (define #%prim.attach-contract-struct!
    (%module-get% %-builtin-module-steel/meta (quote attach-contract-struct!)))
  (define #%prim.#%black-box (%module-get% %-builtin-module-steel/meta (quote #%black-box)))
  (define #%prim.#%function-ptr-table-add
    (%module-get% %-builtin-module-steel/meta (quote #%function-ptr-table-add)))
  (define #%prim.Engine::new (%module-get% %-builtin-module-steel/meta (quote Engine::new)))
  (define #%prim.memory-address (%module-get% %-builtin-module-steel/meta (quote memory-address)))
  (define #%prim.set-strong-box! (%module-get% %-builtin-module-steel/meta (quote set-strong-box!)))
  (define #%prim.inspect-bytecode (%module-get% %-builtin-module-steel/meta (quote inspect-bytecode)))
  (define #%prim.join! (%module-get% %-builtin-module-steel/meta (quote join!)))
  (define #%prim.get-test-mode (%module-get% %-builtin-module-steel/meta (quote get-test-mode)))
  (define #%prim.%iterator? (%module-get% %-builtin-module-steel/meta (quote %iterator?)))
  (define #%prim.#%function-ptr-table-get
    (%module-get% %-builtin-module-steel/meta (quote #%function-ptr-table-get)))
  (define #%prim.expand! (%module-get% %-builtin-module-steel/meta (quote expand!)))
  (define #%prim.#%get-dylib (%module-get% %-builtin-module-steel/meta (quote #%get-dylib)))
  (define #%prim.#%private-cycle-collector-get
    (%module-get% %-builtin-module-steel/meta (quote #%private-cycle-collector-get)))
  (define #%prim.function-name (%module-get% %-builtin-module-steel/meta (quote function-name)))
  (define #%prim.assert! (%module-get% %-builtin-module-steel/meta (quote assert!)))
  (define #%prim.Engine::add-module
    (%module-get% %-builtin-module-steel/meta (quote Engine::add-module)))
  (define #%prim.make-struct-type (%module-get% %-builtin-module-steel/meta (quote make-struct-type)))
  (define #%prim.#%struct-update (%module-get% %-builtin-module-steel/meta (quote #%struct-update)))
  (define #%prim.arity? (%module-get% %-builtin-module-steel/meta (quote arity?)))
  (define #%prim.call-with-current-continuation
    (%module-get% %-builtin-module-steel/meta (quote call-with-current-continuation)))
  (define #%prim.read! (%module-get% %-builtin-module-steel/meta (quote read!)))
  (define #%prim.Engine::clone (%module-get% %-builtin-module-steel/meta (quote Engine::clone)))
  (define #%prim.poll! (%module-get% %-builtin-module-steel/meta (quote poll!)))
  (define #%prim.value->iterator (%module-get% %-builtin-module-steel/meta (quote value->iterator)))
  (define #%prim.breakpoint! (%module-get% %-builtin-module-steel/meta (quote breakpoint!)))
  (define #%prim.call-with-exception-handler
    (%module-get% %-builtin-module-steel/meta (quote call-with-exception-handler)))
  (define #%prim.run! (%module-get% %-builtin-module-steel/meta (quote run!)))
  (define #%prim.current-function-span
    (%module-get% %-builtin-module-steel/meta (quote current-function-span)))
  (define #%prim.raise-error (%module-get% %-builtin-module-steel/meta (quote raise-error)))
  (define #%prim.value->string (%module-get% %-builtin-module-steel/meta (quote value->string)))
  (define #%prim.active-object-count
    (%module-get% %-builtin-module-steel/meta (quote active-object-count)))
  (define #%prim.#%box (%module-get% %-builtin-module-steel/meta (quote #%box)))
  (define #%prim.call/cc (%module-get% %-builtin-module-steel/meta (quote call/cc)))
  (define #%prim.raise-error-with-span
    (%module-get% %-builtin-module-steel/meta (quote raise-error-with-span)))
  (define #%prim.block-on (%module-get% %-builtin-module-steel/meta (quote block-on)))
  (define #%prim.unbox-strong (%module-get% %-builtin-module-steel/meta (quote unbox-strong)))
  (define #%prim.set-env-var! (%module-get% %-builtin-module-steel/meta (quote set-env-var!)))
  (define #%prim.get-contract-struct
    (%module-get% %-builtin-module-steel/meta (quote get-contract-struct)))
  (define #%prim.#%function-ptr-table
    (%module-get% %-builtin-module-steel/meta (quote #%function-ptr-table)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.string->jsexpr (%module-get% %-builtin-module-steel/json (quote string->jsexpr)))
  (define #%prim.value->jsexpr-string
    (%module-get% %-builtin-module-steel/json (quote value->jsexpr-string)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.void (%module-get% %-builtin-module-steel/constants (quote void)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.syntax-loc (%module-get% %-builtin-module-steel/syntax (quote syntax-loc)))
  (define #%prim.syntax->datum (%module-get% %-builtin-module-steel/syntax (quote syntax->datum)))
  (define #%prim.syntax? (%module-get% %-builtin-module-steel/syntax (quote syntax?)))
  (define #%prim.#%syntax/raw (%module-get% %-builtin-module-steel/syntax (quote #%syntax/raw)))
  (define #%prim.syntax/loc (%module-get% %-builtin-module-steel/syntax (quote syntax/loc)))
  (define #%prim.syntax-span (%module-get% %-builtin-module-steel/syntax (quote syntax-span)))
  (define #%prim.syntax-e (%module-get% %-builtin-module-steel/syntax (quote syntax-e)))
  (define #%prim.#%debug-syntax->exprkind
    (%module-get% %-builtin-module-steel/syntax (quote #%debug-syntax->exprkind)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.spawn-process (%module-get% %-builtin-module-steel/process (quote spawn-process)))
  (define #%prim.child-stdin (%module-get% %-builtin-module-steel/process (quote child-stdin)))
  (define #%prim.set-current-dir!
    (%module-get% %-builtin-module-steel/process (quote set-current-dir!)))
  (define #%prim.set-piped-stdout!
    (%module-get% %-builtin-module-steel/process (quote set-piped-stdout!)))
  (define #%prim.child-stdout (%module-get% %-builtin-module-steel/process (quote child-stdout)))
  (define #%prim.wait (%module-get% %-builtin-module-steel/process (quote wait)))
  (define #%prim.which (%module-get% %-builtin-module-steel/process (quote which)))
  (define #%prim.wait->stdout (%module-get% %-builtin-module-steel/process (quote wait->stdout)))
  (define #%prim.command (%module-get% %-builtin-module-steel/process (quote command)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.Err (%module-get% %-builtin-module-steel/core/result (quote Err)))
  (define #%prim.Err->value (%module-get% %-builtin-module-steel/core/result (quote Err->value)))
  (define #%prim.Ok->value (%module-get% %-builtin-module-steel/core/result (quote Ok->value)))
  (define #%prim.Err? (%module-get% %-builtin-module-steel/core/result (quote Err?)))
  (define #%prim.Ok (%module-get% %-builtin-module-steel/core/result (quote Ok)))
  (define #%prim.Ok? (%module-get% %-builtin-module-steel/core/result (quote Ok?)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.Some (%module-get% %-builtin-module-steel/core/option (quote Some)))
  (define #%prim.Some->value (%module-get% %-builtin-module-steel/core/option (quote Some->value)))
  (define #%prim.Some? (%module-get% %-builtin-module-steel/core/option (quote Some?)))
  (define #%prim.None? (%module-get% %-builtin-module-steel/core/option (quote None?)))
  (define #%prim.None (%module-get% %-builtin-module-steel/core/option (quote None)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.TypeId? (%module-get% %-builtin-module-steel/core/types (quote TypeId?)))
  (define #%prim.#%vtable-update-entry!
    (%module-get% %-builtin-module-steel/core/types (quote #%vtable-update-entry!)))
  (%module-get% %-builtin-module-steel/constants (quote void)))

(begin
  (define #%prim.spawn-thread! (%module-get% %-builtin-module-steel/threads (quote spawn-thread!)))
  (define #%prim.channel->try-recv
    (%module-get% %-builtin-module-steel/threads (quote channel->try-recv)))
  (define #%prim.make-channels (%module-get% %-builtin-module-steel/threads (quote make-channels)))
  (define #%prim.thread-finished?
    (%module-get% %-builtin-module-steel/threads (quote thread-finished?)))
  (define #%prim.thread::current/id
    (%module-get% %-builtin-module-steel/threads (quote thread::current/id)))
  (define #%prim.channel->send (%module-get% %-builtin-module-steel/threads (quote channel->send)))
  (define #%prim.thread-join! (%module-get% %-builtin-module-steel/threads (quote thread-join!)))
  (define #%prim.channel->recv (%module-get% %-builtin-module-steel/threads (quote channel->recv)))
  (%module-get% %-builtin-module-steel/constants (quote void)))
