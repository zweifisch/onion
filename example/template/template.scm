(use onion ersatz-lib matchable clojurian-syntax)

(define (prepare-tempalte-vars alist)
  (map (match-lambda ((key . value)
                      `(,key . ,(sexpr->tvalue value)))) alist))

(define (render path #!optional (vars '()))
  (from-file path
             env: (template-std-env search-path: '("./views/"))
             models: (prepare-tempalte-vars vars)))

(defroutes app
  (GET "/" (render "index.html" '((title . "template")))))

(run app)
