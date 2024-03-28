(require "markdown.scm")
(require "steel/fs/fs.scm")

(define highlighter (syntax-highlighter))

;; TODO: Add more languages, and make this mapping... make a bit more sense.
(define highlighter-map
  (hash "bash" "Bourne Again Shell (bash)" "scheme" "Lisp" "racket" "Lisp" "rust" "Rust"))

(define (mdfile->html-highlighted-string path)
  (define contents (file->string path))

  (let ([output (open-output-string)])
    (define parser (parser contents))
    (let loop ([mdparser parser] [sink output] [language #f])
      (define next (parser-next mdparser))
      ; (displayln language)
      (when next
        (define start-language (event->start-code-block-label next))
        (define end-language (event->end-code-block-label next))

        ;; If we have a language, then we use that to highlight the text
        ;; on the inner code block, and emit html there.
        (when (and language (event-text? next))
          ; (displayln "Highlighting language!" language)
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
(define (get-url name #:trailing-slash [trailing-slash #t])
  "https://mattwparas.github.io/theme.css")

(define (index-template description base-url title subsections content)
  `(html
    [#:lang "en"]
    (head (meta [#:charset "utf-8"])
          (meta [#:name "viewport" #:content "width=device-width, initial-scale=1"])
          (title ,title)
          (link [#:rel "stylesheet" #:href ,(get-url "theme.css" #:trailing-slash #false)]))
    (body
     (div
      [#:class "content"]
      (header (div [#:class "header-left"] (a [#:href ,base-url] [#:class "logo"] ,title))
              (div [#:class "header-right"]
                   ;; TODO: This will throw an error
                   (nav itemscope
                        [#:itemtype "http://schema.org/SiteNavigationElement"]
                        ,@(map (lambda (section)
                                 `(li [#:class "nav"]
                                      (a [#:itemprop "url" #:href "SUBSECTION.PERMALINK"]
                                         (span [#:itemprop "name"] "SUBSECTION.TITLE"))))
                               subsections))))
      ;                       <article itemscope itemtype="http://schema.org/BlogPosting">
      ;     <div itemprop="headline">
      ;         <h1>{{ page.title }}</h1>
      ;         <div class="border"></div>
      ;         <time datetime="{{ page.date | date(format="%Y-%m-%d") }}" class="date" itemprop="datePublished">
      ;             {{ page.date | date(format="%d %b %Y") }}
      ;         </time>
      ;     </div>
      ;     <div itemprop="articleBody">
      ;         {{ page.content | safe }}
      ;     </div>
      ; </article>
      (main (article itemscope
                     [#:itemtype "http://schema.org/BlogPosting"]
                     (div [#:itemprop "headline"] (h1 "PAGE TITLE") (div [#:class "border"]))
                     (div [#:itemprop "articleBody"] ,content)))))))

(define readme-html (mdfile->html-highlighted-string "README.md"))

(define result (html->string (index-template "foo-bar" "foo" "my-title" '() readme-html)))

; (let ([sink (open-output-file "output.html")]) (display result sink))

;     <body>
;         <div class="content">
;         {% block body%}
;         {% block header %}
;             <header>
;                 <div class="header-left">
;                     <a href="{{ config.base_url }}" class="logo">{{ config.title }}</a>
;                 </div>
;                 <div class="header-right">
;                     <nav itemscope itemtype="http://schema.org/SiteNavigationElement">
;                       <ul>
;                         {% set index = get_section(path="_index.md") %}
;                         {% for s in index.subsections %}
;                             {% set subsection = get_section(path=s) %}
;                             <li class="nav">
;                                 <a itemprop="url" href="{{ subsection.permalink | safe}}">
;                                     <span itemprop="name">{{ subsection.title }}</span>
;                                 </a>
;                             </li>
;                         {% endfor %}
;                         {% if config.extra.github %}
;                         <li class="nav">
;                             <a itemprop="url" href="https://github.com/{{ config.extra.github }}">
;                                 <img class="icon" src="{{ config.base_url }}/icons/github.svg" alt="Github">
;                             </a>
;                         </li>
;                         {% endif %}
;                         {% if config.extra.twitter %}
;                         <li class="nav">
;                             <a itemprop="url" href="https://twitter.com/{{ config.extra.twitter }}">
;                                 <img class="icon" src="{{ config.base_url }}/icons/twitter.svg" alt="Twitter">
;                             </a>
;                         </li>
;                         {% endif %}
;                       </ul>
;                     </nav>
;                 </div>
;             </header>
;         {% endblock header %}
;         {% block content %}
;         <main>
;             {% block main %}
;             <h1>Index</h1>
;             <div class="border"></div>
;                 <ul>
;                 {% set index = get_section(path="_index.md") %}
;                 {% for s in index.subsections %}
;                     {% set subsection = get_section(path=s) %}
;                     <li>
;                     <a href="{{ subsection.permalink | safe}}">
;                         {{ subsection.title }}
;                     </a>
;                     </li>
;                     {% if subsection.pages %}
;                     <ul>
;                         {% for page in subsection.pages %}
;                         <li>
;                         <a href="{{ page.permalink | safe}}">
;                             {{ page.title }}
;                         </a>
;                         </li>
;                         {% endfor %}
;                     </ul>
;                     {% endif %}
;                 {% endfor %}
;                 </ul>
;             {% endblock main %}
;         </main>
;         {% endblock content %}
;         <footer>
;             {% block footer %}
;             <div class="border"></div>
;             <div class="footer">
;                 <small class="footer-left">
;                     Copyright &copy; {{ config.extra.author }}
;                 </small>
;                 <small class="footer-right">
;                     Powered by <a href="https://www.getzola.org">Zola</a> | Theme <a href="https://github.com/barlog-m/oceanic-zen">Oceanic Zen</a>
;                 </small>
;             </div>
;         {% endblock footer %}
;         </footer>
;     {% endblock body%}
;         </div>
;     </body>
; </html>
