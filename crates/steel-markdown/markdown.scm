(#%require-dylib "libsteel_markdown"
                 (only-in parse
                          parser
                          parser-next
                          event-text?
                          event-code?
                          event-html?
                          event-start?
                          event-end?
                          event->text
                          event->code
                          event->html
                          event->start-tag
                          event->end-tag
                          tag-paragraph?
                          tag-heading?
                          tag-block-quote?
                          tag-code-block?
                          tag-list?
                          tag-foot-note-definition?
                          tag-table?
                          tag-table-head?
                          tag-table-row?
                          tag-table-cell?
                          tag-emphasis?
                          tag-strong?
                          tag-strike-through?
                          tag-link?
                          tag-image?
                          tag->heading
                          tag->code-block
                          code-block-indented?
                          code-block-fenced?
                          code-block->fenced
                          event->html-string
                          event->string
                          syntax-highlighter
                          syntax-highlighter/text->highlighted-html
                          set-event-code!
                          set-event-text!
                          set-event-as-html!))

(require "steel/fs/fs.scm")

(define Softbreak 'SoftBreak)
(define HardBreak 'HardBreak)
(define Rule 'Rule)

; (define *zygote* (Engine::new))

; (define (eval expr)
;   (define engine (Engine::clone *zygote*))
;   (run! engine expr))

; (define my-parser (parser "```scheme,run-in-playground
; (list 10 20 30)
; (list 20 30 40 50)
; ```"))

;; Loads the markdown file located at the given path, and
;; renders it to an html string. Writes the html string out
;; to the given file.
(define (mdfile->html-file path dest)
  (define contents (file->string path))

  (let ([output (open-output-file dest)])
    (define parser (parser contents))
    (let loop ([mdparser parser] [sink output])
      (define next (parser-next mdparser))
      (when next
        (display (event->html-string next) sink)
        (loop mdparser sink)))))

;; Loads the markdown at the given path, and returns
;; it as an html string
(define (mdfile->html-string path)
  (define contents (file->string path))

  (let ([output (open-output-string)])
    (define parser (parser contents))
    (let loop ([mdparser parser] [sink output])
      (define next (parser-next mdparser))
      (when next
        (display (event->html-string next) sink)
        (loop mdparser sink)))

    (get-output-string output)))

(define (event->start-code-block-label event)
  (if (event-start? event)
      (begin
        (define event-start-tag (event->start-tag event))
        (define event-code-block (tag->code-block event-start-tag))

        (if (and event-code-block (code-block-fenced? event-code-block))
            (code-block->fenced event-code-block)
            #f))

      #f))

(define (event->end-code-block-label event)
  (if (event-end? event)
      (begin
        (define event-start-tag (event->end-tag event))
        (define event-code-block (tag->code-block event-start-tag))
        (if (and event-code-block (code-block-fenced? event-code-block))
            (code-block->fenced event-code-block)
            #f))
      #f))

(define highlighter (syntax-highlighter))

; Plain Text
; ASP
; HTML (ASP)
; ActionScript
; AppleScript
; Batch File
; NAnt Build File
; C#
; C++
; C
; CSS
; Clojure
; D
; Diff
; Erlang
; HTML (Erlang)
; Go
; Graphviz (DOT)
; Groovy
; HTML
; Haskell
; Literate Haskell
; Java Server Page (JSP)
; Java
; JavaDoc
; Java Properties
; JSON
; JavaScript
; Regular Expressions (Javascript)
; BibTeX
; LaTeX Log
; LaTeX
; TeX
; Lisp
; Lua
; Make Output
; Makefile
; Markdown
; MultiMarkdown
; MATLAB
; OCaml
; OCamllex
; OCamlyacc
; camlp4
; Objective-C++
; Objective-C
; PHP Source
; PHP
; Pascal
; Perl
; Python
; Regular Expressions (Python)
; R Console
; R
; Rd (R Documentation)
; HTML (Rails)
; JavaScript (Rails)
; Ruby Haml
; Ruby on Rails
; SQL (Rails)
; Regular Expression
; reStructuredText
; Ruby
; Cargo Build Results
; Rust
; SQL
; Scala
; Bourne Again Shell (bash)
; Shell-Unix-Generic
; commands-builtin-shell-bash
; HTML (Tcl)
; Tcl
; Textile
; XML
; YAML

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

; mdfile->html-string "README.md")

; (displayln (~> (parser-next my-parser) event->start-tag tag->code-block ))
; (displayln (map eval (split-many (event->text (parser-next my-parser)) "\n")))
; (displayln (~> (parser-next my-parser) event->end-tag tag->code-block))

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
