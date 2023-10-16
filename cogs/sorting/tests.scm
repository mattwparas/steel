(require "steel/tests/unit-test.scm"
         (for-syntax "steel/tests/unit-test.scm")
         "merge-sort.scm"
         "trie-sort.scm"
         "quick-sort.scm")

; (provide __module__)

; (define __module__ "tests")

; (displayln "Hello world!")

(test-module "trie-sort-tests"
             (check-equal? "basic sorting"
                           (trie-sort (list "zebras" "bananas" "apples" "foo" "bar"))
                           '("apples" "bananas" "bar" "foo" "zebras")))
