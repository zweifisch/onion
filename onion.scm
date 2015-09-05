
(module onion
    (defroutes run wrap-routes GET parse-url)

  (import scheme chicken)

  (use tcp6 intarweb uri-common posix srfi-13 srfi-69 medea srfi-1 extras data-structures)


  (define (parse-url* pattern #!optional (index 0))
    (let* ((param? (string-prefix? ":" pattern 0 1 index))
           (idx (string-index pattern (if param? #\/ #\:) index)))
      (cons (if param? 'param 'static)
            (if idx
                (substring pattern index idx)
                (substring pattern index (string-length pattern))))))


  (define (parse-url pattern)
    (let ((segments '())
          (loop (void)))
      (set! loop (lambda (idx)
                   (if (< idx (string-length pattern))
                       (let ((token (parse-url* pattern idx)))
                         (set! segments
                           (cons token (loop (+ idx (string-length (cdr token))))))))
                   segments))
      (loop 0)))


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
      (alist->hash-table
       `((path . ,(conc "/" (string-join (cdr (uri-path uri)) "/")))
         (query . ,(uri-query uri))
         (headers . ,(request-headers request))
         (method . ,(request-method request))
         (response . ,(make-response port: out))
         (output . ,out)
         (input . ,in)))))

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
    (lambda (req)
      (let* ((path (hash-table-ref req 'path))
             (method (hash-table-ref req 'method))
             (rules (group-rules rules))
             (output (hash-table-ref req 'output))
             (handler
              (route-dispatch path (hash-table-ref rules method))))
        (if handler
            (begin
              (hash-table-set! req 'params (car handler))
              (let ((res (hash-table-ref req 'response))
                    (result ((cadr handler) req)))
                ;; (print result)
                (write-response res)
                (if (string? result)
                    (write-string result #f output)
                    (write-string (json->string result) #f output))
                (finish-response-body res)))))))

  (define-syntax GET
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((url (second form))
             (req (third form))
             (handler (fourth form)))
         `(list 'GET (parse-url ,url) (lambda (req) ,handler))))))

  (define-syntax defroutes
    (er-macro-transformer
     (lambda (form rename compare)
       (let ((name (cadr form))
             (rules (cddr form)))
         `(define ,name (wrap-routes (list ,@rules)))))))

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

