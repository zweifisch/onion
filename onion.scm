
(module onion
    (defroutes run wrap-routes body-parser static alist-get-in)

  (import scheme chicken)

  (use tcp tcp-server intarweb uri-common posix srfi-13 srfi-69 medea srfi-1 extras data-structures ports multipart-form-data)

  (define (default-handler-404 req)
    (let ((response (alist-ref 'response req)))
      (response-status-set! response 'not-found)
      "Page Not Found"))

  (define (alist-get-in keys alist)
    (if (and (pair? alist) (pair? keys))
        (if (= 1 (length keys))
            (alist-ref (car keys) alist)
            (alist-get-in (cdr keys) (alist-ref (car keys) alist)))
        #f))

  (begin-for-syntax

   (require-extension matchable data-structures)

   (define (parse-url pattern)
     (let* ((param? (string-prefix? ":" pattern))
            (idx (string-index pattern (if param? #\/ #\:))))
       (if idx
           (cons
            (if param?
                `(param . ,(string->symbol (substring/shared pattern 1 idx)))
                `(static . , (substring/shared pattern 0 idx)))
            (parse-url (substring/shared pattern idx)))
           (if param?
               `((param . ,(string->symbol (substring/shared pattern 1))))
               `((static . ,pattern))))))

   (define (deep-let-1 pattern path)
     (if (pair? pattern)
         (let ((p (car pattern)))
           (if (keyword? p)
               (let ((p (string->symbol (keyword->string p))))
                 (append
                  (deep-let-1 (cadr pattern) (cons p path))
                  (deep-let-1 (cddr pattern) path)))
               (if (symbol? p)
                   (append `((,p . (lambda (alist) (alist-get-in ',(fold cons '() (cons p path)) alist))))
                           (deep-let-1 (cdr pattern) path))
                   '())))
         '()))

   (define (deep-let pattern datum exps)
     (let ((vars (deep-let-1 pattern '())))
       `(apply (lambda ,(map car vars) ,@exps)
               (map (lambda (transform) (transform ,datum)) (list ,@(map cdr vars))))))

   (define (expand-route form)
     (match form
       ((method url destruct handler)
        (if (pair? destruct)
            `(list ',method ',(parse-url url) (lambda (req) ,(deep-let destruct 'req (list handler))))
            `(list ',method ',(parse-url url) (lambda (,destruct) ,handler))))
       ((method url handler)
        `(list ',method ',(parse-url url) (lambda (req) ,handler))))))

  (define (match-url url segments #!optional (params '()))
    (if (pair? segments)
        (let ((segment (car segments)))
          (if (eqv? (car segment) 'static)
              (if (string-prefix? (cdr segment) url)
                  (match-url (substring/shared url (string-length (cdr segment)))
                             (cdr segments)
                             params)
                  #f)
              (let ((idx (string-index url #\/)))
                (if idx
                    (match-url (substring/shared url idx)
                               (cdr segments)
                               (cons (cons (cdr segment) (substring url 0 idx)) params))
                    (if (= 1 (length segments))
                        (cons (cons (cdr segment) url) params)
                        #f)))))
        (if (= 0 (string-length url)) params #f))) 

  (define (prep-request in out)
    (let* ((request (read-request in))
           (uri (request-uri request)))
      `((path . ,(conc "/" (string-join (cdr (uri-path uri)) "/")))
        (query . ,(uri-query uri))
        (headers . ,(request-headers request))
        (method . ,(request-method request))
        (request . ,request)
        (response . ,(make-response port: out)))))

  (define (group-rules rules)
    (fold (lambda (rule ht)
            (hash-table-update!/default ht (car rule) (cut cons (cdr rule) <>) '())
            ht)
          (make-hash-table)
          rules))

  (define (route-dispatch url rules)
    (if (pair? rules)
        (let* ((rule (car rules))
               (result (match-url url (car rule))))
          (if result (list result (cadr rule))
              (route-dispatch url (cdr rules))))
        #f))

  (define (set-content-type-if-missing response content-type)
    (let ((headers (response-headers response)))
      (unless (header-value 'content-type headers)
        (response-headers-set!
         response
         (update-header-contents
          'content-type `(content-type #(,content-type ())) headers)))))

  (define (wrap-routes rules)
    (let ((rules (group-rules rules)))
      (lambda (req)
        (let* ((path (alist-ref 'path req))
               (method (alist-ref 'method req))
               (handler
                (or (route-dispatch path (hash-table-ref/default rules method '()))
                    `(() ,default-handler-404))))
          (let* ((req (cons `(params . ,(car handler)) req)))
            ((cadr handler) (append (car handler) req)))))))

  (define-syntax defroutes
    @("defroutes"
      (@example
       "param handling"
       (defroutes app
         (GET "/hello/:name" (name) (string-append "hello " name)))))
    (er-macro-transformer
     (lambda (form rename compare)
       (match form
         ((_ name . rules)
          `(define ,name (wrap-routes (list ,@(map expand-route rules)))))))))

  (include "onion-middlewares")

  (define (run handler #!key (port 3000) (maxc 10000))
    (let ((handler (render-response handler)))
      ((make-tcp-server
        (tcp-listen port)
        (lambda ()
          (handler
           (prep-request (current-input-port) (current-output-port))))
        maxc)))))
