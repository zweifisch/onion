
(module onion
    (defroutes run wrap-routes body-parser static)

  (import scheme chicken)

  (use tcp6 intarweb uri-common posix srfi-13 srfi-69 medea srfi-1 extras data-structures ports)

  (define (default-handler-404 req)
    (let ((response (alist-ref 'response req)))
      (response-status-set! response 'not-found)
      "Page Not Found"))

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

   (define (deep-let-1 pattern transformer)
     (if (pair? pattern)
         (let ((p (car pattern)))
           (if (keyword? p)
               (let ((p (string->symbol (keyword->string p))))
                 (append
                  (deep-let-1 (cadr pattern) `(compose (lambda (alist) (if alist (alist-ref ',p alist) #f)) ,transformer))
                  (deep-let-1 (cddr pattern) transformer)))
               (if (symbol? p)
                   (append `((,p . (compose (lambda (alist) (if alist (alist-ref ',p alist) #f)) ,transformer)))
                           (deep-let-1 (cdr pattern) transformer))
                   '())))
         '()))

   (define (deep-let pattern datum exps)
     (let ((vars (deep-let-1 pattern 'identity)))
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

  (define (render-response res result)
    (write-response res)
    (cond ((string? result)
           (write-string result #f (response-port res)))
          ((list? result)
           (write-json result (response-port res)))
          ((port? result)
           (copy-port result (response-port res))))
    (finish-response-body res))

  (define (wrap-routes rules)
    (let ((rules (group-rules rules)))
      (lambda (req)
        (let* ((path (alist-ref 'path req))
               (method (alist-ref 'method req))
               (handler
                (or (route-dispatch path (hash-table-ref/default rules method '()))
                    `(() ,default-handler-404))))
          (let* ((req (cons `(params . ,(car handler)) req))
                 (result ((cadr handler) (append (car handler) req))))
            (render-response (alist-ref 'response req) result))))))

  (define-syntax defroutes
    (er-macro-transformer
     (lambda (form rename compare)
       (match form
         ((_ name . rules)
          `(define ,name (wrap-routes (list ,@(map expand-route rules)))))))))

  (define (body-parser handler #!optional content-types)
    (lambda (req)
      (let* ((headers (alist-ref 'headers req))
             (content-type (header-value 'content-type headers))
             (content-length (header-value 'content-length headers))
             (input (request-port (alist-ref 'request req)))
             (body (case content-type
                    ((application/json)
                     (read-json (read-string content-length input)
                                consume-trailing-whitespace: #f))
                    ((application/x-www-form-urlencoded)
                     (form-urldecode (read-string content-length input)))
                    (else '()))))
        (handler (append `((body . ,body)) req)))))

  (define (static handler root)
    (lambda (req)
      (let ((path (alist-ref 'path req)))
        (open-input-file (string-append root path)))))

  (define listener (make-parameter (void)))

  (define (accept-loop handler)
    (let-values (((in out) (tcp-accept (listener))))
      (handler (prep-request in out))
      (close-input-port in)
      (close-output-port out)
      (accept-loop handler)))

  (define (run handler #!key (port 3000))
    (listener (tcp-listen port))
    (accept-loop handler)))
