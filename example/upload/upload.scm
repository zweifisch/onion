(use onion clojurian-syntax sxml-transforms multipart-form-data)

(define (render sxml)
  (with-output-to-string
    (lambda ()
      (SXML->HTML sxml))))

(defroutes app
  (GET "/" (render
            `(html
              (meta (@ ((charset "utf-8"))))
              (form (@ ((method "POST") (enctype "multipart/form-data")))
               (input (@ ((type "file") (name "file"))))
               (input (@ ((name "extra"))))
               (input (@ ((type "submit") (value "upload"))))))))
  (POST "/" (body: (file))
        (pp
         (multipart-file-port file)))
  (PUT "/files/:path" (path body)
       (pp body)))

(run (-> app body-parser (static "/public" root: "./public")))
