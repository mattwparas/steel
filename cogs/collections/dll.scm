(provide dllist
         dllink
         insert-between
         insert-before
         insert-after
         insert-head
         insert-tail
         remove-link
         dllist-elements
         dllist-head
         dllist-tail
         dllink-content
         dllink-prev
         dllink-next)

(struct dllist (head tail) #:mutable #:transparent)
(struct dllink (content prev next) #:mutable #:transparent)

(define (insert-between dlist before after data)
  ; Insert a fresh link containing DATA after existing link
  ; BEFORE if not nil and before existing link AFTER if not nil
  (define new-link (dllink data before after))
  (if before (set-dllink-next! before new-link) (set-dllist-head! dlist new-link))
  (if after (set-dllink-prev! after new-link) (set-dllist-tail! dlist new-link))
  new-link)

(define (insert-before dlist dlink data)
  ; Insert a fresh link containing DATA before existing link DLINK
  (insert-between dlist (dllink-prev dlink) dlink data))

(define (insert-after dlist dlink data)
  ; Insert a fresh link containing DATA after existing link DLINK
  (insert-between dlist dlink (dllink-next dlink) data))

(define (insert-head dlist data)
  ; Insert a fresh link containing DATA at the head of DLIST
  (insert-between dlist #f (dllist-head dlist) data))

(define (insert-tail dlist data)
  ; Insert a fresh link containing DATA at the tail of DLIST
  (insert-between dlist (dllist-tail dlist) #f data))

(define (remove-link dlist dlink)
  ; Remove link DLINK from DLIST and return its content
  (let ([before (dllink-prev dlink)] [after (dllink-next dlink)])
    (if before (set-dllink-next! before after) (set-dllist-head! dlist after))
    (if after (set-dllink-prev! after before) (set-dllist-tail! dlist before))))

(define (dllist-elements dlist)
  ; Returns the elements of DLIST as a list
  (define (extract-values dlink acc)
    (if dlink (extract-values (dllink-next dlink) (cons (dllink-content dlink) acc)) acc))
  (reverse (extract-values (dllist-head dlist) '())))

(define (run)
  (let ([dlist (dllist #f #f)])
    (insert-head dlist 1)
    (displayln dlist)
    (insert-tail dlist 4)
    (displayln dlist)
    (insert-after dlist (dllist-head dlist) 2)
    (displayln dlist)
    (let* ([next-to-last (insert-before dlist (dllist-tail dlist) 3)]
           [bad-link (insert-before dlist next-to-last 42)])
      (remove-link dlist bad-link))
    (displayln dlist)
    (displayln (dllist-elements dlist))
    (displayln dlist)))

(define (loop)
  (run)
  (loop))
