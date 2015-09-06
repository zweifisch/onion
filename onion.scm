
(module onion
    (defroutes run wrap-routes)

  (import scheme chicken)

  (use tcp6 intarweb uri-common posix srfi-13 srfi-69 medea srfi-1 extras data-structures)


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
        `(list ',method ',(parse-url url) (lambda (req) ,(deep-let destruct 'req (list handler)))))
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

  (define (body-parser headers body)
    #f)

  (define (prep-request in out)
    (let* ((request (read-request in))
           (uri (request-uri request)))
      `((path . ,(conc "/" (string-join (cdr (uri-path uri)) "/")))
        (query . ,(uri-query uri))
        (headers . ,(request-headers request))
        (method . ,(request-method request))
        (response . ,(make-response port: out))
        (output . ,out)
        (input . ,in))))

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

  (define (wrap-routes rules)
    (let ((rules (group-rules rules)))
      (lambda (req)
        (let* ((path (alist-ref 'path req))
               (method (alist-ref 'method req))
               (output (alist-ref 'output req))
               (handler
                (begin
                  ;; (print (hash-table->alist rules))
                  (route-dispatch path (hash-table-ref rules method)))))
          (if handler
              (begin
                (let* ((req (cons `(params . ,(car handler)) req))
                       (res (alist-ref 'response req))
                       (result ((cadr handler) (append (car handler) req))))
                  ;; (print result)
                  (write-response res)
                  (if (string? result)
                      (write-string result #f output)
                      (write-string (json->string result) #f output))
                  (finish-response-body res))))))))

  (define-syntax defroutes
    (er-macro-transformer
     (lambda (form rename compare)
       (match form
         ((_ name . rules)
          `(define ,name (wrap-routes (list ,@(map expand-route rules)))))))))

  (define listener (make-parameter (void)))

  (define (accept-loop router)
    (let-values (((in out) (tcp-accept (listener))))
      (router (prep-request in out))
      (close-input-port in)
      (close-output-port out)
      (accept-loop router)))

  (define (run router #!key (port 3000))
    (listener (tcp-listen port))
    (accept-loop router)))
