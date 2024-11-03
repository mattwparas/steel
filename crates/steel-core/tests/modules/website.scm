(require "markdown.scm")
(require "steel/fs/fs.scm")
(require "front-matter.scm")
(require "steel/sorting/quick-sort.scm")
(require "steel/logging/log.scm")

(define highlighter (syntax-highlighter))

;; TODO: Add more languages, and make this mapping... make a bit more sense.
(define highlighter-map
  (hash "bash" "Bourne Again Shell (bash)" "scheme" "Lisp" "racket" "Lisp" "rust" "Rust"))

(define (mdfile->html-highlighted-string path)
  (define contents (file->string path))
  (mdstring->html-highlighted-string contents))

(define (mdstring->html-highlighted-string contents)
  (let ([output (open-output-string)]
        [parser (parser contents)])
    ;; TODO: When this is named `parser` - shadowing the global, the compiler
    ;; is unhappy.
    ; (define parser (parser contents))
    (let loop ([mdparser parser]
               [sink output]
               [language #f])
      (define next (parser-next mdparser))
      (when next
        (define start-language (event->start-code-block-label next))
        (define end-language (event->end-code-block-label next))

        ;; If we have a language, then we use that to highlight the text
        ;; on the inner code block, and emit html there.
        (when (and language (event-text? next))
          (set-event-as-html! next
                              (syntax-highlighter/text->highlighted-html highlighter
                                                                         (hash-ref highlighter-map
                                                                                   language)
                                                                         (event->text next))))

        ;; Write the event as an html string
        (display (event->html-string next) sink)
        (loop mdparser sink (if end-language #f (or start-language language)))))

    (get-output-string output)))
;; Use templating engine?
(struct Post (path content))

;; HTML templating syntax
(define (change-tag lst)
  (cond
    [(empty? lst) ""]
    [else (string-append (html->string (first lst)) (html->string (rest lst)))]))

(define (keyword? x)
  (and (symbol? x) (starts-with? (symbol->string x) "#:")))

(define (split-by lst n)
  ; (displayln lst n)
  (if (not (empty? lst)) (cons (take lst n) (split-by (drop lst n) n)) '()))

(define (format-attribute html)
  (string-append (trim-start-matches (symbol->string (first html)) "#:")
                 "="
                 "\""
                 (second html)
                 "\""
                 " "))

(define (html->string html)
  ; (displayln "Visiting: " html)
  (cond
    [(empty? html) ""]
    [(string? html) html]
    ; [(not (symbol? (first html))) (html->string (first html))]

    [(not (symbol? (first html))) (change-tag html)]
    ; (html->string (first html))]

    [(and (> (length html) 2)
          (symbol? (second html))
          (list? (third html))
          (not (empty? (third html)))
          (keyword? (car (third html))))

     (string-append "<"
                    (symbol->string (first html))
                    " "
                    (symbol->string (second html))
                    " "
                    (apply string-append (map format-attribute (split-by (third html) 2)))
                    ">"
                    (change-tag (drop html 3))
                    "</"
                    (symbol->string (first html))
                    ">")]

    [(and (not (cdr-null? html))
          (list? (second html))
          (not (empty? (second html)))
          (keyword? (car (second html))))

     (string-append "<"
                    (symbol->string (first html))
                    " "
                    (apply string-append (map format-attribute (split-by (second html) 2)))
                    ">"
                    (change-tag (drop html 2))
                    "</"
                    (symbol->string (first html))
                    ">")]

    [else
     (string-append "<"
                    (symbol->string (first html))
                    ">"
                    (change-tag (rest html))
                    "</"
                    (symbol->string (first html))
                    ">")]))

;; Provide templates simply as a scheme function?
(define (custom-template body)
  (html->string `(html [#:lang "en"]
                       (head (meta [#:charset "utf-8"])
                             (meta [#:name "viewport"
                                    #:content "width=device-width, initial-scale=1"])
                             (meta [#:name "description"])
                             (title "hi"))
                       (body (h1 "Welcome") ,body))))

;; TODO: Just include the link to the style sheet here...
; (define (get-url name #:trailing-slash [trailing-slash #t])
;   *url-root*)

(define *url-root* "file:///home/matt/Documents/website/mattwparas.github.io/_output/")
(define *url-root-no-slash* (trim-end-matches *url-root* "/"))

(define (get-url name #:trailing-slash [trailing-slash #t])
  (string-append *url-root* name))

(define *build-output* "_output")

(define *file-index* #f)

(struct Subsection (path permalink title pages) #:transparent)

(define (subsection-weight>? l r)
  ;; Snag the page from the file index
  (define left (hash-try-get *file-index* (string-append (Subsection-path l) "/_index.md")))
  (define right (hash-try-get *file-index* (string-append (Subsection-path r) "/_index.md")))

  (if (and left right)
      (> (hash-try-get (Page-front-matter-map left) 'weight)
         (hash-try-get (Page-front-matter-map right) 'weight))
      #f))

(define (subsection-date>? l r)

  ;; Snag the page from the file index
  (define left (hash-try-get *file-index* (string-append (Subsection-path l) "/_index.md")))
  (define right (hash-try-get *file-index* (string-append (Subsection-path r) "/_index.md")))

  (if (and left right)
      (string>=? (hash-try-get (Page-front-matter-map left) 'date)
                 (hash-try-get (Page-front-matter-map right) 'date))
      #f))

;; TODO: Add the description back at some point
(define (blog-post-template base-url website-name title _subsections content)
  `(html
    [#:lang "en"]
    (head (meta [#:charset "utf-8"])
          (meta [#:name "viewport" #:content "width=device-width, initial-scale=1"])
          (title ,website-name)
          (link [#:rel "stylesheet" #:href ,(get-url "theme.css" #:trailing-slash #false)]))
    (body
     (div
      [#:class "content"]
      (header (div [#:class "header-left"]
                   (a [#:href ,(string-append base-url "index.html") #:class "logo"] ,website-name))
              (div [#:class "header-right"]
                   ;; TODO: This will throw an error
                   (nav itemscope
                        [#:itemtype "http://schema.org/SiteNavigationElement"]
                        (ul ,@(map (lambda (section)
                                     `(li [#:class "nav"]
                                          (a [#:itemprop "url" #:href ,(Subsection-permalink section)]
                                             (span [#:itemprop "name"] ,(Subsection-title section)))))
                                   (quicksort (path->subsections "content/_index.md")
                                              subsection-weight>?))
                            (li [#:class "nav"]
                                (a [#:itemprop "url" #:href "https://github.com/mattwparas"]
                                   (img [#:class "icon"
                                         #:src ,(string-append base-url "/icons/github.svg")
                                         #:alt "Github"])))))))
      ;; TODO: Move `main` here out, so that various components can just implement their
      ;; main, while the rest of the content is exactly the same
      (main (article itemscope
                     [#:itemtype "http://schema.org/BlogPosting"]
                     (div [#:itemprop "headline"]
                          (h1 ,title)
                          (div [#:class "border"])
                          (time [#:datetime "2024-04-14" #:class "date" #:itemprop "datePublished"]))
                     (div [#:itemprop "articleBody"] ,content)))
      (footer (div [#:class "border"])
              (div [#:class "footer"]
                   (small [#:class "footer-left"] "Copyright &copy; Matthew Paras")
                   (small [#:class "footer-right"]
                          ,(string-append " Powered by "
                                          (html->string '(a [#:href
                                                             "https://github.com/mattwparas/steel"]
                                                            "Steel"))))))))))

(define (top-level-page-template base-url website-name title subsections content)

  (displayln "Building top level page template")

  `(html
    [#:lang "en"]
    (head (meta [#:charset "utf-8"])
          (meta [#:name "viewport" #:content "width=device-width, initial-scale=1"])
          (title ,website-name)
          (link [#:rel "stylesheet" #:href ,(get-url "theme.css" #:trailing-slash #false)]))
    (body
     (div
      [#:class "content"]
      (header (div [#:class "header-left"]
                   (a [#:href ,(string-append base-url "index.html") #:class "logo"] ,website-name))
              (div [#:class "header-right"]
                   ;; TODO: This will throw an error
                   (nav itemscope
                        [#:itemtype "http://schema.org/SiteNavigationElement"]
                        (ul ,@(map (lambda (section)
                                     `(li [#:class "nav"]
                                          (a [#:itemprop "url" #:href ,(Subsection-permalink section)]
                                             (span [#:itemprop "name"] ,(Subsection-title section)))))
                                   (quicksort (path->subsections "content/_index.md")
                                              subsection-weight>?))
                            (li [#:class "nav"]
                                (a [#:itemprop "url" #:href "https://github.com/mattwparas"]
                                   (img [#:class "icon"
                                         #:src ,(string-append base-url "/icons/github.svg")
                                         #:alt "Github"])))))))
      ;; TODO: Move `main` here out, so that various components can just implement their
      ;; main, while the rest of the content is exactly the same
      (main
       (h1 ,(or title "Index"))
       (div [#:class "border"])
       (ul ,@(map (lambda (section)
                    (append (list `(li (a [#:itemprop "url" #:href ,(Subsection-permalink section)]
                                          (span [#:itemprop "name"] ,(Subsection-title section)))))
                            `(ul ,@(map (lambda (subsection)
                                          (list `(li (a [#:itemprop "url"
                                                         #:href ,(Subsection-permalink subsection)]
                                                        (span [#:itemprop "name"]
                                                              ,(Subsection-title subsection))))))
                                        ;; Not sure if this is going to work...
                                        (quicksort (Subsection-pages section) subsection-date>?)))))
                  (quicksort subsections subsection-weight>?))))
      (footer (div [#:class "border"])
              (div [#:class "footer"]
                   (small [#:class "footer-left"] "Copyright &copy; Matthew Paras")
                   (small [#:class "footer-right"]
                          ,(string-append " Powered by "
                                          (html->string '(a [#:href
                                                             "https://github.com/mattwparas/steel"]
                                                            "Steel"))))))))))

(define (file->html path output func)
  ;; This is the page itself
  (define parsed-file-content (hash-get *file-index* path))
  (define highlighted-html (mdstring->html-highlighted-string (Page-content parsed-file-content)))
  (define title (or (hash-try-get (Page-front-matter-map parsed-file-content) 'title) "Index"))
  (define subsections (path->subsections path))
  (define result (html->string (func *url-root* "Matthew Paras" title subsections highlighted-html)))

  (displayln "Writing to" output)

  (let ([sink (open-output-file output)]) (display result sink)))

(define (file-name-without-extension path)
  (trim-end-matches (trim-end-matches (file-name path) (path->extension path)) "."))

;; Process file, pass in the function to process as well
(define (process-file file)
  (cond
    [(equal? (file-name file) "_index.md")
     (define target-directory
       (string-append *build-output*
                      ;; Drop the prefix
                      (trim-start-matches (trim-end-matches file (file-name file)) "content")))

     (unless (path-exists? target-directory)
       (create-directory! target-directory))

     ;; If the current directory we're in doesn't have any children, and the
     ;; only thing is the index.md, treat the index.md file as the blog post
     (if (= 1 (length (read-dir (trim-end-matches file "_index.md"))))
         (file->html file (string-append target-directory "index.html") blog-post-template)

         ;; Write to the output, drop the content start
         (file->html file (string-append target-directory "index.html") top-level-page-template))]

    [else
     (define target-directory
       (string-append *build-output*
                      ;; Drop the prefix
                      (trim-start-matches (string-append (trim-end-matches file (file-name file))
                                                         (file-name-without-extension file))
                                          "content")))

     (unless (path-exists? target-directory)
       (create-directory! target-directory))

     (file->html file (string-append target-directory "/index.html") blog-post-template)]))

(define (generate-file-index-list path)
  (cond
    [(is-file? path) (parse-file path)]
    [(is-dir? path) (flatten (map generate-file-index-list (read-dir path)))]))

(define (generate-file-index-map path)
  (transduce (generate-file-index-list path)
             (mapping (lambda (page) (cons (Page-path page) page)))
             (into-hashmap)))

(define (capitalize str)
  (define lst (string->list str))
  (list->string (cons (char-upcase (car lst)) (cdr lst))))

(define (surround-with-spaces str)
  (string-append " " str " "))

(define (path->subsections path)
  (cond
    [(equal? (file-name path) "_index.md")

     (define neighboring-files (read-dir (trim-end-matches path (file-name path))))

     (flatten
      (map (lambda (child)

             (cond
               [(equal? (file-name child) "_index.md") '()]

               [(is-dir? child)

                (define permalink
                  (string-append *url-root-no-slash*
                                 ;; Drop the prefix
                                 (trim-start-matches child "content")
                                 "/index.html"))

                (Subsection child
                            permalink
                            ;; Crappy way of getting the directory name
                            (surround-with-spaces (capitalize (file-name child)))
                            ;; TODO: Snag everything else from this directory
                            (flatten (map path->subsections (read-dir child))))]

               [else

                (define permalink
                  (string-append *url-root-no-slash*
                                 ;; Drop the prefix
                                 (trim-start-matches (trim-end-matches child (file-name child))
                                                     "content")
                                 (trim-end-matches (file-name child) ".md")
                                 "/index.html"))

                (Subsection child
                            permalink
                            (surround-with-spaces
                             (or (hash-try-get (Page-front-matter-map (hash-get *file-index* child))
                                               'title)
                                 ;; Crappy way of getting the directory name
                                 (capitalize (file-name (trim-end-matches child (file-name child))))))
                            ;; TODO: Snag everything else from this directory
                            '())]))
           neighboring-files))]

    [(is-dir? path) (map path->subsections (read-dir path))]

    [else '()]))

;; Read
(define (generate-files root)
  ;; TODO: Set values accordingly

  (unless (path-exists? *build-output*)
    (create-directory! *build-output*))

  ;; TODO: Lift this out into a broader context!
  (set! *file-index* (generate-file-index-map "content"))

  (process-file "content/_index.md")

  ;; Subsections
  (walk-files (string-append root "/notes") process-file)
  (walk-files (string-append root "/about") process-file)
  (walk-files (string-append root "/projects") process-file))

(define (with-stdout-piped command)
  (set-piped-stdout! command)
  command)

(define (generate-robots.txt)
  (let ([output-file (open-output-file (string-append *build-output* "/robots.txt"))])
    (display "User-agent: *" output-file)
    (display "\n" output-file)
    (display "Allow: /" output-file)
    (display "\n" output-file)
    (display "Sitemap" output-file)
    (display *url-root* output-file)
    (display "sitemap.xml" output-file)
    (display "\n" output-file)))

(define (generate-sitemap.xml)
  (let ([output-file (open-output-file (string-append *build-output* "/sitemap.xml"))])
    (display "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" output-file)
    (display
     (html->string
      `(urlset
        [#:xmlns "https://www.sitemaps.org/schemas/sitemap/0.9"]
        ,@(transduce
           *file-index*
           (mapping (lambda (kv)
                      (if (ends-with? (car kv) "_index.md")
                          `(url (loc ,(string-append *url-root*
                                                     (trim-end-matches (car kv) "_index.md"))))

                          `(url (loc ,(string-append *url-root* (trim-end-matches (car kv) ".md")))
                                (lastmod ,(hash-get (Page-front-matter-map (cadr kv)) 'date))))))
           (into-list))))
     output-file)))

(define (compile-sass)
  (let ([output-file (open-output-file (string-append *build-output* "/theme.css"))])
    (display (~> (command "grass" '("sass/theme.scss" "--style" "compressed"))
                 (with-stdout-piped)
                 spawn-process
                 Ok->value
                 wait->stdout
                 Ok->value)
             output-file)))

(define (generate-404-not-found)
  (define result
    (html->string
     (top-level-page-template *url-root* "Matthew Paras" "404 Not Found" '() "404 Not Found")))
  (let ([sink (open-output-file (string-append *build-output* "/404.html"))]) (display result sink)))

(provide main)
(define (main #:root-url [root-url #f])

  (when root-url
    (set! *url-root* root-url))

  (log/info! "Generating html files")
  (generate-files "content")
  (log/info! "Compiling sass files")
  (compile-sass)
  (log/info! "Copying static files")
  (copy-directory-recursively! "static/" *build-output*)
  (log/info! "Generating robots.txt")
  (generate-robots.txt)
  (log/info! "Generating sitemap.xml")
  (generate-sitemap.xml)
  (log/info! "Generating 404.html")
  (generate-404-not-found)
  (log/info! "Finished"))

;; Run it
; (main)
