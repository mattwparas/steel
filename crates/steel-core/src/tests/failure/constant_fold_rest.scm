;; Too few arguments to a lambda with a rest parameter.
(display ((lambda (a b c d e f g h i j k l m n o p q r s . rest) a) 1))
