(define table (vector
                (vector "" "X" "Y" "Z")
                (vector 1 1 2 3)
                (vector 2 4 5 6)
                (vector 3 7 8 9)))

(display "<table style=\"text-align:center; border: 1px solid\">")
(transduce 
    (range 0 (vector-length table))
    (into-for-each (lambda (r)
                (display "<tr>")
                (transduce (range 0 (vector-length (vector-ref table r)))
                           (into-for-each (lambda (c)
                                (when (equal? r 0)
                                        (display "<th>"))
                                (when (> r 0)
                                        (display "<td>"))
                                (display (vector-ref (vector-ref table r) c))
                                ; (display r)
                                (when (equal? r 0)
                                        (display "</th>"))
                                (when (> r 0)
                                        (display "</td>")))))
                                        
                (displayln "</tr>"))))
(display "</table>")