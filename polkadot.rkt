#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         markdown
         racket/date) 

(define polka-name "Polkadot-Wiki")
(define polka-file-root current-directory-for-user)
(define document-header-end-token "===")
(define polka-server-host "127.0.0.1")
(define polka-server-port 8000)
(date-display-format 'iso-8601)

(define document-index (make-hash))
(define tag-index (make-hash))

(define-values (polka-dispatch request-url)
  (dispatch-rules
   (("") render-index)
   (("documents") render-document-list)
   (("document" (string-arg)) render-document-detail)
   (("tags") render-tag-list)
   (("tag" (string-arg)) render-tag-detail)))

; /
(define (render-index request)
  (make-wiki-response (list (include-template "templates/index.html"))))

; /documents
(define (render-document-list request)
  (make-wiki-response
   (cons (include-template "templates/document-introduction.html")
        (render-document-preview-list (hash-values document-index)))))
  
; /document/<string-arg>
(define (render-document-detail request slug)
  (if (document-indexed? slug)
     (make-wiki-response (render-document-template (retrieve-document slug) 'detail))
     (make-wiki-response (list (include-template "templates/404.html")))))

; /tags
(define (render-tag-list request)
  (define rendered-tag-list
    (cons (include-template "templates/tag-introduction.html")
      (map
       (λ (tag)
         (include-template "templates/tag-preview.html"))
       (hash-keys tag-index))))
  (make-wiki-response rendered-tag-list))

; /tag/<string-arg>
(define (render-tag-detail request tag-name)
  (if (tag-exists? tag-name)
     (let ((document-list
             (render-document-preview-list (retrieve-documents-with-tag tag-name))))
       (make-wiki-response (include-template "templates/tag-detail.html")))
     (make-wiki-response (list (include-template "templates/404.html")))))

(define (markdown->html markdown-string)
  (foldl (λ (xexpr html)
           (string-append html (xexpr->string xexpr))) "" (parse-markdown markdown-string)))

(define (path->document document-path)
  (call-with-input-file document-path
    (λ (document-file)
  (define ns (make-base-namespace))
  (define document-list (string-split (port->string document-file) document-header-end-token))
  (call-with-input-string (car document-list) (λ (str) (port->list (λ (datum) (eval (read datum) ns)) str)))
  (define metadata (namespace-variable-value 'polka-metadata #t #f ns))
    (hash
     "title" (hash-ref metadata "title")
     "slug" (hash-ref metadata "slug")
     "tags" (hash-ref metadata "tags")
     "date-modified" (file-or-directory-modify-seconds document-path)
     "body" (markdown->html (cadr document-list))))
    #:mode 'text))

(define (document-file? file-path)
  (define path-string (path->string file-path))
  (and (> (string-length path-string) 7) (equal? (substring path-string (- (string-length path-string) 5)) "polka")))

(define (generate-document-index)
  (hash-clear! document-index)
  (hash-clear! tag-index)
  (current-directory (build-path (polka-file-root) "documents/"))
  (map
   (λ (file) (if (document-file? file) (index-document (path->document file)) '()))
   (directory-list (current-directory))))

(define (document-list-by-date-modified document-list)
  (sort document-list (λ (x y) (if (< (hash-ref x "date-modified") (hash-ref y "date-modified")) #t #f))))

(define (render-document-template document template-type)
  (let ((title (hash-ref document "title"))
        (slug (hash-ref document "slug"))
        (tags (hash-ref document "tags"))
        (date-modified (date->string (seconds->date (hash-ref document "date-modified"))))
        (body (hash-ref document "body")))
    (cond
      ((equal? 'preview template-type) (include-template "templates/document-preview.html"))
      ((equal? 'detail template-type) (include-template "templates/document-detail.html"))
      (else (include-template "templates/document-detail.html")))))

(define (render-document-preview-list document-list)
  (map
         (λ (doc) (render-document-template doc 'preview))
         (document-list-by-date-modified document-list)))

(define (create-indexing-thread)
  (thread (λ ()
            (sync
             (filesystem-change-evt (build-path (polka-file-root) "documents")))
            (generate-document-index)
            (create-indexing-thread))))

(define (index-document document)
  (map
   (λ (tag)
     (hash-set! tag-index tag (cons document (hash-ref! tag-index tag '()))))
   (hash-ref document "tags"))
  (hash-set! document-index (hash-ref document "slug") document))

(define (retrieve-documents-with-tag tag)
  (hash-ref tag-index tag '()))

(define (retrieve-document slug)
  (hash-ref document-index slug '()))

(define (document-indexed? slug)
  (hash-has-key? document-index slug))

(define (tag-exists? tag)
  (hash-has-key? tag-index tag))

(define (make-wiki-response node-list)
  (make-response (include-template "templates/base.html")))

(define (make-response template)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 template))))

(generate-document-index)
(create-indexing-thread)

(serve/servlet polka-dispatch
               #:quit? #t
               #:listen-ip polka-server-host
               #:launch-browser? #f
               #:file-not-found-responder (λ (req) (make-wiki-response (list (include-template "templates/404.html"))))
               #:extra-files-paths (list (build-path (polka-file-root) "htdocs"))
               #:servlet-regexp (regexp ""))