(assert! (equal? (transduce '("hello" " " "Steel" " " "transducer" " " "world") (into-string))
          "hello Steel transducer world"))

(assert! (equal? (transduce (list 1 2 3/4 7.5 #\* 'hoo "ray!" #t) (into-string))
          "123/47.5*hooray!#true"))
