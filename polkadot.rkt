#lang racket

(require web-server/servlet
         web-server/servlet-env
         web-server/templates
         markdown) 

(define polka-name "Polkadot-Wiki")
(define polka-file-root current-directory-for-user)
(define document-header-end-token "===")
(define polka-server-host "mysides.rip")
(define polka-server-port 8000)

(struct document (title slug tags body))
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
        (map/documents (λ (doc) (render-document-template doc 'preview))))))
  
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
     (let ((document-list (map
                           (λ (doc)
                             (cons (document-title doc) (document-slug doc)))
                           (retrieve-documents-with-tag tag-name))))
       (make-wiki-response (include-template "templates/tag-detail.html")))
     (make-wiki-response (list (include-template "templates/404.html")))))

(define (markdown->html markdown-string)
  (foldl (λ (xexpr html)
           (string-append html (xexpr->string xexpr))) "" (parse-markdown markdown-string)))

(define (file->document document-file)
  (define ns (make-base-namespace))
  (define document-list (string-split (port->string document-file) document-header-end-token))
  (call-with-input-string (car document-list) (λ (str) (port->list (λ (datum) (eval (read datum) ns)) str)))
  (let* ((metadata (namespace-variable-value 'polka-metadata #t #f ns))
         (title (hash-ref metadata "title"))
         (slug (hash-ref metadata "slug"))
         (tags (hash-ref metadata "tags")))
    (document title slug tags (markdown->html (cadr document-list)))))

(define (document-file? file-path)
  (define path-string (path->string file-path))
  (and (> (string-length path-string) 7) (equal? (substring path-string (- (string-length path-string) 5)) "polka")))

(define (generate-document-index)
  (hash-clear! document-index)
  (hash-clear! tag-index)
  (current-directory (build-path (polka-file-root) "documents/"))
  (map
   (λ (file)
     (if (document-file? file)
        (call-with-input-file file
          (λ (document-port) (index-document (file->document document-port))) #:mode 'text)
     '()))
   (directory-list (current-directory))))

(define (map/documents proc)
 (hash-map document-index (λ (slug document) (proc document))))

(define (render-document-template polka-doc template-type)  
  (let ((title (document-title polka-doc))
        (slug (document-slug polka-doc))
        (tags (document-tags polka-doc))
        (body (document-body polka-doc)))
    (cond
      ((equal? 'preview template-type) (include-template "templates/document-preview.html"))
      ((equal? 'detail template-type) (include-template "templates/document-detail.html"))
      (else (include-template "templates/document-detail.html")))))

(define (create-indexing-thread)
  (thread (λ ()
            (sync
             (filesystem-change-evt (build-path (polka-file-root) "documents")))
            (generate-document-index)
            (create-indexing-thread))))

(define (index-document polka-doc)
  (map
   (λ (tag)
     (hash-set! tag-index tag (cons polka-doc (hash-ref! tag-index tag '()))))
   (document-tags polka-doc))
  (hash-set! document-index (document-slug polka-doc) polka-doc))

(define (retrieve-documents-with-tag tag)
  (hash-ref tag-index tag '()))

(define (retrieve-document slug)
  (hash-ref document-index slug '()))

(define (document-indexed? slug)
  (hash-has-key? document-index slug))

(define (tag-exists? tag)
  (hash-has-key? tag-index tag))

; consumes a list of rendered template-nodes and displays a wiki page
(define (make-wiki-response node-list)
  (make-response (include-template "templates/base.html")))

; displays http-response
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